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

exec su-exec 65532:65532 \
  /bin/star-server start -i /etc/starintel/init.lisp
