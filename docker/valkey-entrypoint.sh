#!/bin/sh
set -eu

password_file="${VALKEY_PASSWORD_FILE:-/run/secrets/valkey_password}"
if [ ! -r "$password_file" ]; then
  echo "Valkey password file is not readable" >&2
  exit 1
fi

password="$(cat "$password_file")"
if [ -z "$password" ]; then
  echo "Valkey password file is empty" >&2
  exit 1
fi

runtime_dir=/run/starintel-valkey
acl_file="$runtime_dir/users.acl"
mkdir -p "$runtime_dir"
password_hash="$(printf %s "$password" | sha256sum | cut -d ' ' -f 1)"
unset password
printf 'user default on #%s ~* &* +@all\n' "$password_hash" > "$acl_file"
unset password_hash
chown -R 65532:65532 "$runtime_dir"
chmod 0700 "$runtime_dir"
chmod 0600 "$acl_file"

exec setpriv --reuid=65532 --regid=65532 --clear-groups valkey-server \
  --bind 0.0.0.0 \
  --protected-mode yes \
  --aclfile "$acl_file" \
  --appendonly yes \
  --appendfsync everysec \
  --dir /data \
  "$@"
