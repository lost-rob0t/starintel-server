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


def main() -> int:
    lock_path = Path(sys.argv[1] if len(sys.argv) > 1 else "schema/starintel-schema.lock.json")
    lock = json.loads(lock_path.read_text(encoding="utf-8"))
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

    branches = schema.get("allOf", [])
    research_branch = next(
        (
            branch
            for branch in branches
            if branch.get("if", {}).get("properties", {}).get("dtype", {}).get("const")
            == "research-node"
        ),
        None,
    )
    if research_branch is None:
        fail("canonical schema is missing dtype research-node")

    data_schema = research_branch.get("then", {}).get("properties", {}).get("data", {})
    if data_schema.get("additionalProperties") is not False:
        fail("research-node data must reject undeclared fields")

    required = set(data_schema.get("required", []))
    expected_required = set(lock["research_node_required_fields"])
    missing_required = sorted(expected_required - required)
    if missing_required:
        fail(f"research-node is missing required fields: {missing_required}")

    expansion_fields = set(expansion.get("dtype_fields", {}).get("research-node", []))
    missing_expansion_fields = sorted(expected_required - expansion_fields)
    if missing_expansion_fields:
        fail(f"research-node expansion is missing fields: {missing_expansion_fields}")

    if manifest.get("dtype_count") != len(expansion.get("dtype_fields", {})):
        fail("schema manifest dtype count does not match expansion")
    if manifest.get("expansion_content_hash") != canonical_hash(expansion):
        fail("schema manifest expansion hash does not match canonical expansion")

    print(
        "verified StarIntel",
        lock["schema_version"],
        "research-node schema at",
        commit,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
