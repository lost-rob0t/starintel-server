#!/bin/sh
set -eu

load_secret() {
  name="$1"
  file_variable="${name}_FILE"
  eval "file=\${$file_variable:-}"

  if [ -n "$file" ]; then
    value="$(cat "$file")"
    export "$name=$value"
  fi
}

load_secret COUCHDB_PASSWORD
load_secret RABBITMQ_PASSWORD

exec su-exec 65532:65532 \
  /bin/star-server start -i /etc/starintel/init.lisp
