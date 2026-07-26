#!/bin/bash
set -euo pipefail

load_secret() {
  local name="$1"
  local file_variable="${name}_FILE"
  local file="${!file_variable:-}"

  if [[ -n "$file" ]]; then
    export "$name=$(<"$file")"
  fi
}

load_secret COUCHDB_PASSWORD
load_secret COUCHDB_SECRET
load_secret COUCHDB_ERLANG_COOKIE

exec /docker-entrypoint.sh "$@"
