#!/usr/bin/env python3
"""Verify the active StarLang consumer lock (positional legacy CLI retained)."""
import runpy
import sys
from pathlib import Path

if len(sys.argv) > 1 and not sys.argv[1].startswith("-"):
    sys.argv[1:2] = ["--lock", sys.argv[1]]
runpy.run_path(str(Path(__file__).with_name("sync-starintel-schema.py")), run_name="__main__")
