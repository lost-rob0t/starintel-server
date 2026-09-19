#!/usr/bin/env sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$repo_root"

command -v swipl >/dev/null 2>&1 || {
  echo "check-prolog-migrations: swipl is required" >&2
  exit 127
}

# Follow SWI-Prolog's documented non-interactive PlUnit lifecycle: run_tests
# determines the process status, while -t halt exits instead of entering the
# interactive top level after the initial goal completes.
swipl -q \
  -s .prolog/kb/migrations-test.pl \
  -g run_tests \
  -t halt

protocol_stderr=$(mktemp)
trap 'rm -f "$protocol_stderr"' EXIT HUP INT TERM

set +e
protocol_output=$(
  printf '%s\n' \
    '["reset"]' \
    '["add_fun","migration_view(version_distribution)"]' \
    '["map_doc",{"_id":"fixture-protocol","tenant_id":"tenant-a","dataset":"dataset-a","dtype":"person","schema_version":"0.8.0"}]' \
    '["reset"]' \
    '["add_fun","call(shell)"]' \
    '["add_fun",42]' \
  | swipl -q -s scripts/starintel-prolog-view-server.pl 2>"$protocol_stderr"
)
protocol_status=$?
set -e

if [ "$protocol_status" -ne 0 ]; then
  echo "check-prolog-migrations: query-server exited $protocol_status" >&2
  if [ -s "$protocol_stderr" ]; then
    echo "--- query-server stderr ---" >&2
    cat "$protocol_stderr" >&2
  fi
  if [ -n "$protocol_output" ]; then
    echo "--- query-server stdout ---" >&2
    printf '%s\n' "$protocol_output" >&2
  fi
  exit "$protocol_status"
fi

rm -f "$protocol_stderr"
trap - EXIT HUP INT TERM

# Compare parsed JSON terms, not serializer whitespace. This keeps the gate
# strict about protocol shape and values while remaining stable across SWI
# pretty-printer formatting changes.
if ! printf '%s\n' "$protocol_output" | swipl -q -g '
  use_module(library(http/json)),
  Expected = [
    true,
    true,
    [[[ ["tenant-a", "dataset-a", "0.8.0", "person"], 1 ]]],
    true,
    ["error", "unsupported_function", "Unsupported StarIntel migration map function: call(shell)"],
    ["error", "unsupported_function", "Unsupported StarIntel migration map function: selector must be a string."]
  ],
  same_length(Expected, Actual),
  maplist(json_read_dict(user_input), Actual),
  Expected == Actual,
  read_string(user_input, _, Rest),
  normalize_space(string(Tail), Rest),
  Tail == ""
' -t halt; then
  echo "check-prolog-migrations: query-server protocol mismatch" >&2
  echo "--- actual ---" >&2
  printf '%s\n' "$protocol_output" >&2
  exit 1
fi

echo "check-prolog-migrations: ok"
