#!/usr/bin/env sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$repo_root"

command -v swipl >/dev/null 2>&1 || {
  echo "check-prolog-migrations: swipl is required" >&2
  exit 127
}

# Use SWI-Prolog's documented non-interactive PlUnit contract: run_tests/0
# is the initialization goal and halt/0 is the top-level. A failing suite
# exits before the top-level with status 1; a passing suite reaches halt/0
# and exits 0. Do not embed halt/0 in the test goal while also overriding
# the top-level with halt(1), because that can turn a completed test run into
# a false-red process status.
swipl -q \
  -s .prolog/kb/migrations-test.pl \
  -g run_tests \
  -t halt

protocol_output=$(
  printf '%s\n' \
    '["reset"]' \
    '["add_fun","migration_view(version_distribution)"]' \
    '["map_doc",{"_id":"fixture-protocol","tenant_id":"tenant-a","dataset":"dataset-a","dtype":"person","schema_version":"0.8.0"}]' \
    '["reset"]' \
    '["add_fun","call(shell)"]' \
  | swipl -q -s scripts/starintel-prolog-view-server.pl
)

expected=$(cat <<'EOF'
true
true
[[[["tenant-a","dataset-a","0.8.0","person"],1]]]
true
["error","unsupported_function","Unsupported StarIntel migration map function: call(shell)"]
EOF
)

if [ "$protocol_output" != "$expected" ]; then
  echo "check-prolog-migrations: query-server protocol mismatch" >&2
  echo "--- expected ---" >&2
  printf '%s\n' "$expected" >&2
  echo "--- actual ---" >&2
  printf '%s\n' "$protocol_output" >&2
  exit 1
fi

echo "check-prolog-migrations: ok"
