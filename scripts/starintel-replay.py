#!/usr/bin/env python3
"""Verify an event-source/1 NDJSON export; optionally write a PRIVATE shadow copy.

No HTTP, CouchDB, RabbitMQ or actor clients are imported. Never use this command
as live re-execution. A checksum proves byte integrity, not producer authenticity.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import shutil
import sys
import tempfile
from typing import Any, Iterable

PROTOCOL = "starintel.event-source/1"
MAX_STATE_BYTES = 1024 * 1024
MAX_RECORD_BYTES = 8 * MAX_STATE_BYTES
MAX_RECORDS = 10_000
HASH_FIELDS = (
    "protocol", "database", "tenant", "dataset", "document_id", "stream_id",
    "event_id", "mutation_id", "sequence", "previous_hash", "coverage",
    "operation", "recorded_at", "causation_id", "state_json",
)


class ReplayError(ValueError):
    """Validation failure; messages deliberately omit source payloads/secrets."""


def scalar_digest(values: Iterable[str | int]) -> str:
    digest = hashlib.sha256()
    for value in values:
        if isinstance(value, str):
            text = "s" + value
        elif type(value) is int:
            text = "i" + str(value)
        else:
            raise ReplayError("invalid_hash_scalar")
        encoded = text.encode("utf-8")
        digest.update(str(len(encoded)).encode("ascii") + b":" + encoded)
    return digest.hexdigest()


def record_hash(record: dict[str, Any]) -> str:
    try:
        return scalar_digest(["record/v1", *(record[key] for key in HASH_FIELDS)])
    except KeyError as exc:
        raise ReplayError("missing_record_field") from exc


def reject_duplicate_keys(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise ReplayError("duplicate_json_key")
        result[key] = value
    return result


def reject_constant(_: str) -> None:
    raise ReplayError("non_finite_json_number")


def finite_float(text: str) -> float:
    value = float(text)
    if not math.isfinite(value):
        raise ReplayError("non_finite_json_number")
    return value


def strict_json(text: str) -> Any:
    try:
        return json.loads(text, object_pairs_hook=reject_duplicate_keys,
                          parse_constant=reject_constant, parse_float=finite_float)
    except (json.JSONDecodeError, RecursionError) as exc:
        raise ReplayError("invalid_json") from exc


def validate_record(record: Any) -> dict[str, Any]:
    if not isinstance(record, dict):
        raise ReplayError("invalid_record")
    required = set(HASH_FIELDS) | {"_id", "hash"}
    if not required <= record.keys() or record.keys() - required - {"_rev"}:
        raise ReplayError("record_fields_mismatch")
    if record["protocol"] != PROTOCOL:
        raise ReplayError("unsupported_protocol")
    for key in required - {"sequence", "recorded_at"}:
        if not isinstance(record[key], str):
            raise ReplayError("invalid_string")
    for key in ("_id", "database", "dataset", "document_id", "stream_id",
                "event_id", "mutation_id", "hash"):
        if not record[key]:
            raise ReplayError("empty_identity")
    if type(record["sequence"]) is not int or record["sequence"] < 1:
        raise ReplayError("invalid_sequence")
    if type(record["recorded_at"]) not in (int, str):
        raise ReplayError("invalid_recorded_at")
    if record["operation"] not in ("new", "updated", "deleted"):
        raise ReplayError("unknown_operation")
    if record["coverage"] not in ("origin", "baseline", "continuation"):
        raise ReplayError("invalid_coverage")
    if len(record["state_json"].encode("utf-8")) > MAX_STATE_BYTES:
        raise ReplayError("state_too_large")
    stream = scalar_digest(["stream/v1", record["database"], record["tenant"],
                            record["dataset"], record["document_id"]])
    identity = "replay:" + scalar_digest(["record-id/v1", stream,
                                          record["mutation_id"]])
    if (record["stream_id"] != stream or record["_id"] != identity
            or record["hash"] != record_hash(record)):
        raise ReplayError("checksum_or_identity_mismatch")
    return record


def replay(records: Iterable[dict[str, Any]], *, database: str, tenant: str,
           dataset: str, expected_heads: dict[str, list[Any]] | None = None,
           allow_baselines: bool = False,
           max_records: int = MAX_RECORDS) -> tuple[dict[str, Any], dict[str, Any], bool]:
    if (not all(isinstance(v, str) for v in (database, tenant, dataset))
            or type(max_records) is not int or max_records < 1):
        raise ReplayError("invalid_replay_scope")
    states: dict[str, Any] = {}
    heads: dict[str, Any] = {}
    seen: dict[str, str] = {}
    baseline = False
    for count, raw in enumerate(records, 1):
        if count > max_records:
            raise ReplayError("replay_limit")
        event = validate_record(raw)
        if (event["database"], event["tenant"], event["dataset"]) != (database, tenant, dataset):
            raise ReplayError("replay_scope_mismatch")
        identity, checksum = event["_id"], event["hash"]
        if identity in seen:
            if seen[identity] != checksum:
                raise ReplayError("duplicate_id_conflict")
            continue
        stream, sequence = event["stream_id"], event["sequence"]
        prior = heads.get(stream)
        if prior is not None:
            if (sequence != prior[0] + 1 or event["previous_hash"] != prior[1]
                    or event["coverage"] != "continuation"):
                raise ReplayError("sequence_or_predecessor_gap")
        else:
            origin = (sequence == 1 and event["coverage"] == "origin"
                      and event["operation"] == "new")
            accepted_baseline = allow_baselines and event["coverage"] == "baseline"
            if event["previous_hash"] or not (origin or accepted_baseline):
                raise ReplayError("missing_origin")
            baseline |= accepted_baseline
        if prior is not None:
            if event["operation"] == "new" and stream in states:
                raise ReplayError("new_over_existing_state")
            if event["operation"] in ("updated", "deleted") and stream not in states:
                raise ReplayError("missing_live_state")
        if event["operation"] == "deleted":
            if event["state_json"]:
                raise ReplayError("invalid_tombstone")
            states.pop(stream, None)
        else:
            state = strict_json(event["state_json"])
            if (not isinstance(state, dict) or "_rev" in state
                    or state.get("_id") != event["document_id"]
                    or state.get("dataset") != event["dataset"]
                    or state.get("tenant_id", "") != event["tenant"]):
                raise ReplayError("state_scope_mismatch")
            extensions = state.get("extensions", {})
            if not isinstance(extensions, dict) or any(k.startswith("_server_") for k in extensions):
                raise ReplayError("private_state_in_projection")
            states[stream] = state
        heads[stream] = [sequence, checksum]
        seen[identity] = checksum
    if expected_heads is not None:
        valid = isinstance(expected_heads, dict) and all(
            isinstance(stream, str) and isinstance(head, list) and len(head) == 2
            and type(head[0]) is int and head[0] > 0 and isinstance(head[1], str)
            for stream, head in expected_heads.items())
        if not valid or heads != expected_heads:
            raise ReplayError("head_manifest_mismatch")
    return states, heads, expected_heads is not None and not baseline


def read_records(path: Path) -> Iterable[dict[str, Any]]:
    with path.open("r", encoding="utf-8", errors="strict") as stream:
        for _ in range(MAX_RECORDS + 1):
            line = stream.readline(MAX_RECORD_BYTES + 1)
            if not line:
                return
            if len(line.encode("utf-8")) > MAX_RECORD_BYTES:
                raise ReplayError("record_too_large")
            if not line.strip():
                raise ReplayError("empty_record")
            yield strict_json(line)
        raise ReplayError("replay_limit")


def write_shadow(destination: Path, states: dict[str, Any], heads: dict[str, Any],
                 report: dict[str, Any]) -> None:
    """Create a private shadow with report.json last; never replace live state.

    A process kill may leave an incomplete directory without report.json. Such a
    directory is not a completed projection and must not be used for cutover.
    This offline tool does not promise crash-durable fsync or live DB cutover.
    """
    if destination.exists() or destination.is_symlink():
        raise ReplayError("output_already_exists")
    if not destination.parent.is_dir():
        raise ReplayError("output_parent_missing")
    staging = Path(tempfile.mkdtemp(prefix=".starintel-replay-", dir=destination.parent))
    reserved = False
    try:
        documents = "".join(json.dumps(states[key], ensure_ascii=False, separators=(",", ":")) + "\n"
                            for key in sorted(states))
        for name, text in (("documents.ndjson", documents),
                           ("heads.json", json.dumps(heads, sort_keys=True) + "\n"),
                           ("report.json", json.dumps(report, sort_keys=True) + "\n")):
            file = staging / name
            with file.open("x", encoding="utf-8") as output:
                os.chmod(file, 0o600)
                output.write(text)
        # Reserve the final name exclusively. Move only into OUR new directory.
        destination.mkdir(mode=0o700)
        reserved = True
        # report.json is the completion marker; publish it LAST.
        for name in ("documents.ndjson", "heads.json", "report.json"):
            (staging / name).rename(destination / name)
    except BaseException:
        if reserved:
            shutil.rmtree(destination)
        raise
    finally:
        shutil.rmtree(staging)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("events", type=Path, help="Authorized NDJSON replay-record export")
    parser.add_argument("--database", required=True)
    parser.add_argument("--tenant", required=True)
    parser.add_argument("--dataset", required=True)
    parser.add_argument("--heads", type=Path, help="Independent source head manifest")
    parser.add_argument("--allow-baselines", action="store_true")
    parser.add_argument("--allow-incomplete", action="store_true")
    parser.add_argument("--output", type=Path, help="New private shadow directory; never a live database")
    args = parser.parse_args(argv)
    try:
        expected = None
        if args.heads is not None:
            with args.heads.open("r", encoding="utf-8") as source:
                text = source.read(MAX_RECORD_BYTES + 1)
            if len(text.encode("utf-8")) > MAX_RECORD_BYTES:
                raise ReplayError("manifest_too_large")
            expected = strict_json(text)
        states, heads, complete = replay(read_records(args.events), database=args.database,
                                         tenant=args.tenant, dataset=args.dataset,
                                         expected_heads=expected, allow_baselines=args.allow_baselines)
        if not complete and not args.allow_incomplete:
            raise ReplayError("complete_source_manifest_required")
        report = {"protocol": PROTOCOL, "documents": len(states), "streams": len(heads),
                  "scope_complete": complete, "server_fully_replayable": False,
                  "external_effects": 0}
        if args.output is not None:
            write_shadow(args.output, states, heads, report)
        print(json.dumps(report, sort_keys=True))
        return 0
    except (ReplayError, OSError, UnicodeError) as exc:
        # OSError paths may contain sensitive operator information. Do not echo them.
        code = str(exc) if isinstance(exc, ReplayError) else "input_output_error"
        print(json.dumps({"error": code}), file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
