#!/usr/bin/env python3
"""Require the server and its immutable star-cl dependency to use one release."""
import json
import re
import urllib.request
from pathlib import Path

root = Path(__file__).resolve().parents[1]
lock = json.loads((root / "schema/starintel-schema.lock.json").read_text())
revision = json.loads((root / "flake.lock").read_text())["nodes"]["star-cl"]["locked"]["rev"]
if not re.fullmatch(r"[0-9a-f]{40}", revision):
    raise SystemExit("star-cl must be pinned to a full immutable commit")
for name in ("qlfile", "qlfile.lock"):
    if revision not in (root / name).read_text():
        raise SystemExit(f"{name} does not match the Nix star-cl pin")
url = f"https://raw.githubusercontent.com/lost-rob0t/star-cl/{revision}/schema/starintel-schema.lock.json"
with urllib.request.urlopen(url, timeout=30) as response:
    dependency = json.load(response)
if lock != dependency:
    raise SystemExit("server and star-cl must consume the same StarLang release and artifacts")
print(f"star-cl {revision} and server consume StarLang {lock['canonical_commit']}")
