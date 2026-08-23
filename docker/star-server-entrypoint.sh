#!/bin/sh
set -eu

load_secret() {
  name="$1"
  file_variable="${name}_FILE"
  eval "file=\${$file_variable:-}"

  if [ -n "$file" ]; then
    if [ ! -r "$file" ]; then
      printf '%s\n' "Secret file for ${name} is not readable: ${file}" >&2
      exit 1
    fi
    value="$(cat "$file")"
    if [ -z "$value" ]; then
      printf '%s\n' "Secret file for ${name} is empty: ${file}" >&2
      exit 1
    fi
    export "$name=$value"
  fi
}

stage_secret_file() {
  name="$1"
  file_variable="${name}_FILE"
  eval "source=\${$file_variable:-}"

  [ -n "$source" ] || return 0
  if [ ! -r "$source" ]; then
    printf '%s\n' "Secret file for ${name} is not readable: ${source}" >&2
    exit 1
  fi

  target="/tmp/starintel-${name}.secret"
  umask 077
  cat "$source" > "$target"
  [ -s "$target" ] || {
    printf '%s\n' "Secret file for ${name} is empty: ${source}" >&2
    exit 1
  }
  chown 65532:65532 "$target"
  chmod 0400 "$target"
  export "$file_variable=$target"
}

load_secret COUCHDB_PASSWORD
load_secret RABBITMQ_PASSWORD
load_secret STAR_AUTH_PEPPER
load_secret STAR_AUTH_BOOTSTRAP_SECRET

# The lease store intentionally consumes a password file instead of placing the
# Valkey credential in the Lisp process environment. Compose secrets are mounted
# root-owned, while the server runs as uid 65532. Stage a private runtime copy
# before dropping privileges so the lease runtime can read it without weakening
# the source secret permissions.
stage_secret_file VALKEY_PASSWORD

STAR_SERVER_INIT_FILE="${STAR_SERVER_INIT_FILE:-/etc/starintel/init.lisp}"

exec su-exec 65532:65532 \
  /bin/star-server start -i "$STAR_SERVER_INIT_FILE"
