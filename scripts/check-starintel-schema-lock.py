#!/usr/bin/env python3

from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
from pathlib import Path
from typing import Any


def fail(message: str) -> None:
    raise SystemExit(message)


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def load(path: Path) -> dict[str, Any]:
    return json.loads(path.read_text(encoding="utf-8"))


def main() -> int:
    parser = argparse.ArgumentParser(description="Verify the pinned Star-Lang release")
    parser.add_argument("lock", nargs="?", default="schema/starintel-schema.lock.json")
    parser.add_argument("--canonical-root", type=Path)
    parser.add_argument("--star-cl-root", type=Path)
    args = parser.parse_args()

    lock = load(Path(args.lock))
    required = {
        "schema_version", "release_version", "canonical_repository",
        "canonical_commit", "schema_path", "release_lock_path",
        "authority_library", "canonical_key_style",
    }
    missing = sorted(required - lock.keys())
    if missing:
        fail(f"lock is missing fields: {missing}")
    if lock["release_version"] != "0.10.1" or lock["schema_version"] != "0.10.1":
        fail("server requires StarIntel release/schema 0.10.1")
    if lock["canonical_repository"] != "nsaspy/star-lang":
        fail("Star-Lang must be the canonical schema repository")
    if lock["canonical_key_style"] != "lowerCamelCase":
        fail("canonical StarIntel keys must be lowerCamelCase")

    if args.canonical_root:
        root = args.canonical_root.resolve()
        head = subprocess.check_output(
            ["git", "rev-parse", "HEAD"], cwd=root, text=True
        ).strip()
        if head != lock["canonical_commit"]:
            fail(f"Star-Lang HEAD {head} does not match {lock['canonical_commit']}")
        release_lock = load(root / lock["release_lock_path"])
        if release_lock["releaseVersion"] != lock["release_version"]:
            fail("release lock version disagrees with consumer lock")
        for name, expected in release_lock["artifacts"].items():
            path = root / "specs/starintel/0.10.1/generated" / name
            if digest(path) != expected:
                fail(f"Star-Lang artifact hash mismatch: {name}")
        for name, expected in release_lock["sources"].items():
            path = root / "specs/starintel/0.10.1" / name
            if digest(path) != expected:
                fail(f"Star-Lang source hash mismatch: {name}")

    if args.star_cl_root:
        star_cl_root = args.star_cl_root.resolve()
        star_cl_head = subprocess.check_output(
            ["git", "rev-parse", "HEAD"], cwd=star_cl_root, text=True
        ).strip()
        star_cl_lock = load(star_cl_root / "schema/starintel-schema.lock.json")
        for field in ("release_version", "schema_version", "canonical_repository", "canonical_commit"):
            if star_cl_lock.get(field) != lock[field]:
                fail(f"star-cl lock disagrees on {field}")
        flake_star_cl = load(Path("flake.lock"))["nodes"]["star-cl"]
        if flake_star_cl["locked"].get("rev") != star_cl_head:
            fail("flake.lock does not pin the verified star-cl checkout")
        expected_url = "https://git.starintel.actor/nsaspy/star-cl"
        if flake_star_cl["locked"].get("url") != expected_url:
            fail("flake.lock star-cl authority is not the canonical Forgejo repository")

    print(
        f"verified StarIntel {lock['release_version']} from "
        f"{lock['canonical_repository']}@{lock['canonical_commit']}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
