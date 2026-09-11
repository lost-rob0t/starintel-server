#!/usr/bin/env python3

from __future__ import annotations

import hashlib
import json
import sys
import urllib.request
from pathlib import Path
from typing import Any


def fail(message: str) -> None:
    raise SystemExit(message)


def load_json(url: str) -> dict[str, Any]:
    with urllib.request.urlopen(url, timeout=30) as response:
        return json.load(response)


def canonical_hash(value: Any) -> str:
    payload = json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False)
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


def branch_for_dtype(schema: dict[str, Any], dtype: str) -> dict[str, Any] | None:
    return next(
        (
            branch
            for branch in schema.get("allOf", [])
            if branch.get("if", {}).get("properties", {}).get("dtype", {}).get("const")
            == dtype
        ),
        None,
    )


def verify_dtype(
    schema: dict[str, Any],
    expansion: dict[str, Any],
    dtype: str,
    required_fields: list[str],
) -> None:
    branch = branch_for_dtype(schema, dtype)
    if branch is None:
        fail(f"canonical schema is missing dtype {dtype}")

    data_schema = branch.get("then", {}).get("properties", {}).get("data", {})
    if data_schema.get("additionalProperties") is not False:
        fail(f"{dtype} data must reject undeclared fields")

    schema_required = set(data_schema.get("required", []))
    missing_schema_required = sorted(set(required_fields) - schema_required)
    if missing_schema_required:
        fail(f"{dtype} is missing required fields: {missing_schema_required}")

    expansion_fields = set(expansion.get("dtype_fields", {}).get(dtype, []))
    if not expansion_fields:
        fail(f"schema expansion is missing dtype {dtype}")
    missing_expansion_fields = sorted(set(required_fields) - expansion_fields)
    if missing_expansion_fields:
        fail(f"{dtype} expansion is missing fields: {missing_expansion_fields}")


def main() -> int:
    lock_path = Path(sys.argv[1] if len(sys.argv) > 1 else "schema/starintel-schema.lock.json")
    lock = json.loads(lock_path.read_text(encoding="utf-8"))
    release_version = lock.get("release_version")
    if not release_version:
        fail("lock is missing release_version")
    repository = lock["canonical_repository"]
    commit = lock["canonical_commit"]
    base_url = f"https://raw.githubusercontent.com/{repository}/{commit}"

    schema = load_json(f"{base_url}/{lock['schema_path']}")
    expansion = load_json(f"{base_url}/{lock['expansion_path']}")
    manifest = load_json(f"{base_url}/{lock['manifest_path']}")

    if schema.get("$id") != "https://spec.starintel.actor/schema/starintel-doc-v0.9.0.json":
        fail("unexpected canonical schema id")
    if manifest.get("schema_version") != lock["schema_version"]:
        fail("manifest schema version does not match lock")
    if expansion.get("schema_version") != lock["schema_version"]:
        fail("expansion schema version does not match lock")
    if manifest.get("release_version") != release_version:
        fail("manifest release version does not match lock")

    required_by_dtype = {
        "research-node": list(lock.get("research_node_required_fields", [])),
        "operation": list(lock.get("operation_required_fields", [])),
    }
    for dtype in lock.get("required_dtypes", []):
        if dtype not in required_by_dtype:
            fail(f"lock is missing required fields for dtype {dtype}")
        verify_dtype(schema, expansion, dtype, required_by_dtype[dtype])

    if manifest.get("dtype_count") != len(expansion.get("dtype_fields", {})):
        fail("schema manifest dtype count does not match expansion")
    if manifest.get("expansion_content_hash") != canonical_hash(expansion):
        fail("schema manifest expansion hash does not match canonical expansion")

    print(
        "verified StarIntel",
        lock["schema_version"],
        "release",
        lock.get("release_version", "unspecified"),
        "required dtypes",
        ", ".join(lock.get("required_dtypes", [])),
        "at",
        commit,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
