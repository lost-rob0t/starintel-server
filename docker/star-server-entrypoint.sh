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

load_secret COUCHDB_PASSWORD
load_secret RABBITMQ_PASSWORD
load_secret STAR_AUTH_PEPPER
load_secret STAR_AUTH_BOOTSTRAP_SECRET

# The Valkey client deliberately reads a file after privilege drop. Compose
# file secrets preserve host ownership, so a private 0600 mount is not readable
# by UID 65532. Stage only this secret into Docker's private runtime tmpfs.
if [ -n "${VALKEY_PASSWORD_FILE:-}" ]; then
  if [ ! -r "$VALKEY_PASSWORD_FILE" ] || [ ! -s "$VALKEY_PASSWORD_FILE" ]; then
    printf '%s\n' 'Valkey password file is unreadable or empty' >&2
    exit 1
  fi
  umask 077
  runtime_secrets="$(mktemp -d /dev/shm/starintel-secrets.XXXXXX)"
  # The fresh root-owned directory prevents symlink substitution while copying.
  cp "$VALKEY_PASSWORD_FILE" "$runtime_secrets/valkey_password"
  chmod 0600 "$runtime_secrets/valkey_password"
  chown 65532:65532 "$runtime_secrets/valkey_password"
  chown 65532:65532 "$runtime_secrets"
  export VALKEY_PASSWORD_FILE="$runtime_secrets/valkey_password"
fi

STAR_SERVER_INIT_FILE="${STAR_SERVER_INIT_FILE:-/etc/starintel/init.lisp}"

exec su-exec 65532:65532 \
  /bin/star-server start -i "$STAR_SERVER_INIT_FILE"
