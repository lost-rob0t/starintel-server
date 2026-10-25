#!/usr/bin/env bash
# Native transport gate only; see docs/embedded-router-acceptance.org.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
fail() { printf 'embedded-router prerequisite failed: %s\n' "$1" >&2; exit 2; }
command -v sbcl >/dev/null || fail 'SBCL is required'
command -v timeout >/dev/null || fail 'timeout is required'
[[ -n "${STAR_ROUTER_LIBRARY:-}" && -f "$STAR_ROUTER_LIBRARY" ]] || fail 'STAR_ROUTER_LIBRARY must name the built shared library'
[[ -n "${STAR_LANG_SOURCE:-}" && -f "$STAR_LANG_SOURCE/star-actor-wire/star-actor-wire.asd" ]] || fail 'STAR_LANG_SOURCE must name the pinned StarLang checkout'
[[ -f "$repo_root/tests/embedded-router-native.lisp" ]] || fail 'native Lisp driver is required'
export STAR_ROUTER_TEST_ROOT="$repo_root"
if [[ "${1:-}" == "--application" || "${1:-}" == "--rabbit-only" || "${1:-}" == "--sc01" ]]; then
  command -v python3 >/dev/null || fail 'Python 3 is required for application acceptance'
  mode=()
  if [[ "$1" == "--rabbit-only" ]]; then mode=(--rabbit-only); fi
  if [[ "$1" == "--sc01" ]]; then mode=(--sc01); fi
  [[ $# == 1 ]] || fail 'one mode argument is allowed'
  exec timeout --signal=TERM --kill-after=10s 300s python3 "$repo_root/tests/embedded-router-application.py" "${mode[@]}"
fi
[[ $# == 0 ]] || fail 'supported modes are native (no argument), --application, --rabbit-only and --sc01'
exec timeout --signal=TERM --kill-after=5s 90s sbcl --noinform --no-sysinit --no-userinit --script "$repo_root/tests/embedded-router-native.lisp"
