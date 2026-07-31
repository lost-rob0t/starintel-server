#!/usr/bin/env python3
"""Deduplicate StarIntel relation documents in CouchDB.

Dry-run is the default. Pass --apply to write canonical relation documents and
then tombstone legacy duplicates. The canonical identity is the directed tuple:
(dataset, source, predicate, target).
"""

from __future__ import annotations

import argparse
import base64
import copy
import hashlib
import json
import os
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable, Iterator, Mapping, MutableMapping, Sequence

DEFAULT_PREDICATE = "related-to"
DEFAULT_BATCH_SIZE = 200


class MigrationError(RuntimeError):
    pass


@dataclass(frozen=True, order=True)
class RelationIdentity:
    dataset: str
    source: str
    predicate: str
    target: str

    def encoded(self) -> str:
        fields = (self.dataset, self.source, self.predicate, self.target)
        return "|".join(f"{len(value.encode('utf-8'))}:{value}" for value in fields)

    def document_id(self) -> str:
        digest = hashlib.sha256(self.encoded().encode("utf-8")).hexdigest()
        return f"relation:{digest}"


@dataclass
class MigrationPlan:
    relation_documents: int
    invalid_documents: list[dict[str, Any]]
    groups: dict[str, list[dict[str, Any]]]
    upserts: list[dict[str, Any]]
    deletions: list[dict[str, Any]]
    duplicate_documents: int
    rewritten_singletons: int


class CouchDB:
    def __init__(
        self,
        base_url: str,
        database: str,
        user: str | None,
        password: str | None,
        timeout: float,
    ) -> None:
        self.base_url = base_url.rstrip("/")
        self.database = database
        self.timeout = timeout
        self.headers = {"Accept": "application/json"}
        if user is not None:
            token = base64.b64encode(f"{user}:{password or ''}".encode()).decode()
            self.headers["Authorization"] = f"Basic {token}"

    def _url(self, path: str, query: Mapping[str, str] | None = None) -> str:
        database = urllib.parse.quote(self.database, safe="")
        url = f"{self.base_url}/{database}/{path.lstrip('/')}"
        if query:
            url = f"{url}?{urllib.parse.urlencode(query)}"
        return url

    def request(
        self,
        method: str,
        path: str,
        *,
        query: Mapping[str, str] | None = None,
        body: Any | None = None,
    ) -> Any:
        headers = dict(self.headers)
        data = None
        if body is not None:
            data = json.dumps(body, separators=(",", ":")).encode("utf-8")
            headers["Content-Type"] = "application/json"
        request = urllib.request.Request(
            self._url(path, query), data=data, headers=headers, method=method
        )
        try:
            with urllib.request.urlopen(request, timeout=self.timeout) as response:
                payload = response.read()
        except urllib.error.HTTPError as exc:
            detail = exc.read().decode("utf-8", errors="replace")
            raise MigrationError(
                f"CouchDB {method} {request.full_url} failed: {exc.code} {detail}"
            ) from exc
        except urllib.error.URLError as exc:
            raise MigrationError(
                f"CouchDB {method} {request.full_url} failed: {exc.reason}"
            ) from exc
        return json.loads(payload) if payload else None

    def all_documents(self) -> list[dict[str, Any]]:
        response = self.request(
            "GET",
            "_all_docs",
            query={"include_docs": "true", "conflicts": "true"},
        )
        rows = response.get("rows", [])
        return [row["doc"] for row in rows if isinstance(row.get("doc"), dict)]

    def bulk(self, documents: Sequence[dict[str, Any]]) -> list[dict[str, Any]]:
        if not documents:
            return []
        response = self.request("POST", "_bulk_docs", body={"docs": documents})
        if not isinstance(response, list):
            raise MigrationError(f"Unexpected _bulk_docs response: {response!r}")
        errors = [row for row in response if row.get("error")]
        if errors:
            raise MigrationError(
                "CouchDB bulk operation failed: "
                + json.dumps(errors, sort_keys=True)
            )
        return response


def nested_data(document: Mapping[str, Any]) -> Mapping[str, Any]:
    value = document.get("data")
    return value if isinstance(value, Mapping) else {}


def relation_value(document: Mapping[str, Any], key: str) -> Any:
    if key in document:
        return document.get(key)
    return nested_data(document).get(key)


def clean_string(value: Any) -> str:
    return value.strip() if isinstance(value, str) else ""


def is_relation(document: Mapping[str, Any]) -> bool:
    return clean_string(document.get("dtype")).lower() == "relation"


def relation_identity(document: Mapping[str, Any]) -> RelationIdentity | None:
    if not is_relation(document):
        return None
    source = clean_string(relation_value(document, "source"))
    target = clean_string(relation_value(document, "target"))
    if not source or not target:
        return None
    predicate = clean_string(relation_value(document, "predicate")) or DEFAULT_PREDICATE
    dataset = clean_string(document.get("dataset"))
    return RelationIdentity(dataset, source, predicate, target)


def unique_strings(values: Iterable[Any]) -> list[str]:
    seen: set[str] = set()
    result: list[str] = []
    for value in values:
        if not isinstance(value, str):
            continue
        clean = value.strip()
        if clean and clean not in seen:
            seen.add(clean)
            result.append(clean)
    return result


def relation_notes(document: Mapping[str, Any]) -> Iterator[str]:
    note = relation_value(document, "note")
    if isinstance(note, str):
        yield note
    notes = relation_value(document, "notes")
    if isinstance(notes, list):
        yield from (value for value in notes if isinstance(value, str))


def document_sort_key(document: Mapping[str, Any]) -> tuple[str, str]:
    date_added = document.get("dateAdded", document.get("date_added", ""))
    return str(date_added), str(document.get("_id", ""))


def set_relation_value(document: MutableMapping[str, Any], key: str, value: Any) -> None:
    data = document.get("data")
    if isinstance(data, MutableMapping) and key not in document:
        data[key] = value
    else:
        document[key] = value


def set_dedupe_metadata(
    document: MutableMapping[str, Any],
    identity: RelationIdentity,
    old_ids: list[str],
    evidence_count: int,
) -> None:
    if isinstance(document.get("data"), Mapping):
        extensions = document.setdefault("extensions", {})
        if not isinstance(extensions, MutableMapping):
            extensions = {}
            document["extensions"] = extensions
        star_server = extensions.setdefault("star_server", {})
        if not isinstance(star_server, MutableMapping):
            star_server = {}
            extensions["star_server"] = star_server
        star_server["relation_identity"] = identity.encoded()
        star_server["deduplicated_from"] = old_ids
        star_server["evidence_count"] = evidence_count
    else:
        document["relationIdentity"] = identity.encoded()
        document["deduplicatedFrom"] = old_ids
        document["evidenceCount"] = evidence_count


def merge_relation_group(
    identity: RelationIdentity, documents: Sequence[dict[str, Any]]
) -> tuple[dict[str, Any], list[dict[str, Any]]]:
    canonical_id = identity.document_id()
    canonical_existing = next(
        (document for document in documents if document.get("_id") == canonical_id),
        None,
    )
    base = canonical_existing or min(documents, key=document_sort_key)
    merged = copy.deepcopy(base)
    merged["_id"] = canonical_id
    if canonical_existing is None:
        merged.pop("_rev", None)
    else:
        merged["_rev"] = canonical_existing["_rev"]

    set_relation_value(merged, "source", identity.source)
    set_relation_value(merged, "target", identity.target)
    set_relation_value(merged, "predicate", identity.predicate)
    merged["dataset"] = identity.dataset

    sources = unique_strings(
        source
        for document in documents
        for source in (
            document.get("sources", [])
            if isinstance(document.get("sources"), list)
            else []
        )
    )
    if sources:
        merged["sources"] = sources

    notes = unique_strings(
        note for document in documents for note in relation_notes(document)
    )
    if notes:
        set_relation_value(merged, "note", "\n\n".join(notes))

    old_ids = sorted(
        str(document["_id"])
        for document in documents
        if document.get("_id") != canonical_id
    )
    set_dedupe_metadata(merged, identity, old_ids, len(documents))

    deletions = [
        {"_id": document["_id"], "_rev": document["_rev"], "_deleted": True}
        for document in documents
        if document.get("_id") != canonical_id
    ]
    return merged, deletions


def build_plan(
    documents: Sequence[dict[str, Any]], *, rewrite_singletons: bool = True
) -> MigrationPlan:
    groups: dict[str, list[dict[str, Any]]] = defaultdict(list)
    invalid: list[dict[str, Any]] = []
    relation_documents = 0

    for document in documents:
        if not is_relation(document):
            continue
        relation_documents += 1
        identity = relation_identity(document)
        if identity is None:
            invalid.append(document)
            continue
        groups[identity.document_id()].append(document)

    upserts: list[dict[str, Any]] = []
    deletions: list[dict[str, Any]] = []
    duplicate_documents = 0
    rewritten_singletons = 0

    for canonical_id in sorted(groups):
        group = groups[canonical_id]
        duplicate_documents += max(0, len(group) - 1)
        is_noncanonical_singleton = (
            len(group) == 1 and group[0].get("_id") != canonical_id
        )
        if len(group) == 1 and not (rewrite_singletons and is_noncanonical_singleton):
            continue
        identity = relation_identity(group[0])
        assert identity is not None
        merged, group_deletions = merge_relation_group(identity, group)
        upserts.append(merged)
        deletions.extend(group_deletions)
        if is_noncanonical_singleton:
            rewritten_singletons += 1

    return MigrationPlan(
        relation_documents=relation_documents,
        invalid_documents=invalid,
        groups=dict(groups),
        upserts=upserts,
        deletions=deletions,
        duplicate_documents=duplicate_documents,
        rewritten_singletons=rewritten_singletons,
    )


def batches(values: Sequence[dict[str, Any]], size: int) -> Iterator[list[dict[str, Any]]]:
    for start in range(0, len(values), size):
        yield list(values[start : start + size])


def write_backup(path: Path, documents: Sequence[dict[str, Any]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as stream:
        for document in documents:
            if is_relation(document):
                stream.write(json.dumps(document, sort_keys=True))
                stream.write("\n")


def apply_plan(database: CouchDB, plan: MigrationPlan, batch_size: int) -> None:
    for batch in batches(plan.upserts, batch_size):
        database.bulk(batch)
    for batch in batches(plan.deletions, batch_size):
        database.bulk(batch)


def read_password(args: argparse.Namespace) -> str | None:
    if args.password is not None:
        return args.password
    if args.password_file:
        return Path(args.password_file).read_text(encoding="utf-8").strip()
    return None


def parser() -> argparse.ArgumentParser:
    default_url = (
        f"{os.getenv('COUCHDB_SCHEME', 'http')}://"
        f"{os.getenv('COUCHDB_HOST', '127.0.0.1')}:"
        f"{os.getenv('COUCHDB_PORT', '5984')}"
    )
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--url", default=default_url)
    result.add_argument("--database", default=os.getenv("COUCHDB_DATABASE", "starintel"))
    result.add_argument("--user", default=os.getenv("COUCHDB_USER", "admin"))
    result.add_argument("--password", default=os.getenv("COUCHDB_PASSWORD"))
    result.add_argument("--password-file", default=os.getenv("COUCHDB_PASSWORD_FILE"))
    result.add_argument("--timeout", type=float, default=30.0)
    result.add_argument("--batch-size", type=int, default=DEFAULT_BATCH_SIZE)
    result.add_argument("--duplicates-only", action="store_true")
    result.add_argument("--apply", action="store_true")
    result.add_argument("--no-backup", action="store_true")
    result.add_argument("--backup")
    return result


def main(argv: Sequence[str] | None = None) -> int:
    args = parser().parse_args(argv)
    if args.batch_size < 1:
        raise MigrationError("--batch-size must be positive")

    database = CouchDB(
        args.url,
        args.database,
        args.user,
        read_password(args),
        args.timeout,
    )
    documents = database.all_documents()
    plan = build_plan(documents, rewrite_singletons=not args.duplicates_only)

    summary = {
        "database": args.database,
        "relation_documents": plan.relation_documents,
        "identity_groups": len(plan.groups),
        "duplicate_documents": plan.duplicate_documents,
        "canonical_upserts": len(plan.upserts),
        "legacy_deletions": len(plan.deletions),
        "rewritten_singletons": plan.rewritten_singletons,
        "invalid_relations": len(plan.invalid_documents),
        "mode": "apply" if args.apply else "dry-run",
    }
    print(json.dumps(summary, indent=2, sort_keys=True))

    if not args.apply:
        return 0

    if not args.no_backup:
        backup_path = Path(
            args.backup
            or f"relation-dedupe-backup-{time.strftime('%Y%m%d-%H%M%S')}.jsonl"
        )
        write_backup(backup_path, documents)
        print(f"backup={backup_path}")

    apply_plan(database, plan, args.batch_size)
    print("relation deduplication applied")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except MigrationError as exc:
        print(f"error: {exc}", file=sys.stderr)
        raise SystemExit(1) from exc
