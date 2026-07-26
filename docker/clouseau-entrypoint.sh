#!/bin/sh
set -eu

cookie_file=/tmp/clouseau.erlang.cookie
cp "${CLOUSEAU_COOKIE_FILE}" "$cookie_file"
chown 65532:65532 "$cookie_file"
chmod 0400 "$cookie_file"

su-exec 65532:65532 epmd -daemon

exec su-exec 65532:65532 java \
  -server \
  -Djava.net.preferIPv4Stack=true \
  -Dcookie.file="$cookie_file" \
  -jar /share/clouseau/clouseau.jar \
  "${CLOUSEAU_CONFIG}"
