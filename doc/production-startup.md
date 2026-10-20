# Linux startup

From the checkout, with Nix flakes, Docker Engine, Compose v2, and Python 3.9+:

```sh
python3 scripts/stack-up.py
```

The launcher reuses `nix run .#load-images`, `docker-compose.yml`, and the existing
runtime. It creates seven random secrets in `~/.local/state/starintel/credentials`
without replacing existing files. That directory lives outside the checkout so
Nix source snapshots cannot capture these credentials. Keep it on durable local
storage and include it in your protected operational backup. Losing auth secrets
can invalidate authentication even when database volumes survive.

Use `--credentials-dir /absolute/private/path` for another location. The directory
must belong to the invoking user and have mode 0700; existing secrets must be
regular, nonempty, singly linked files owned by that user with mode 0600.
Symlinks in the path or secret files are rejected. All existing files are checked
before any missing secrets are generated. The seven names are `couchdb_password`,
`couchdb_secret`, `erlang_cookie`, `rabbitmq_password`, `valkey_password`,
`auth_pepper`, and `auth_bootstrap_secret`. The optional actor profile has its own
credential requirements and is outside this default startup command.

`--prepare-only` performs secret preparation without launching anything.
`--timeout 300` bounds Compose health startup; `--build-timeout 1800` separately
bounds image loading/building. A final HTTP `/ready` probe runs inside the server
container with a ten-second request limit. Failures leave services, secrets, and
volumes intact. Subprocess output is suppressed to avoid copying configuration or
application logs into the launcher's output; the failed command and exit code are
reported. Inspect service logs privately when diagnosing a failure.

## Existing init configuration

Compose binds `docker/star-server-init.lisp` read-only at `/etc/starintel/init.lisp`.
The entrypoint starts `star-server start -i` with that path. The init file reads
CouchDB, RabbitMQ, tenant, HTTP, and auth configuration from runtime environment
variables and mounted secret files. Defaults continue to use the Compose service
names and file-backed secrets. `STAR_SERVER_INIT_HOST_FILE` selects a custom host
init file, and `STAR_SERVER_INIT_FILE` selects the container path. Keep custom init
files credential-free; use the existing `environment-secret` file interface.

Compose `.env` settings remain supported for ports and other nonsecret settings.
The launcher sets `CREDENTIALS_DIR` explicitly in the subprocess environment,
overriding `.env` so the checked directory is always used. Do not put credential
values into `.env`, init files, command arguments, tracked files, or Nix inputs.
Only use trusted repository/build inputs.

## Subsequent operations

For manual Compose operations, select the same directory and project settings:

```sh
export CREDENTIALS_DIR="$HOME/.local/state/starintel/credentials"
docker compose ps
docker compose restart
docker compose down
```

Named CouchDB, RabbitMQ, Valkey, and Clouseau volumes survive restart and `down`.
`down --volumes` destroys those volumes: it is not a routine shutdown command.
The launcher does not change credentials, bootstrap an administrator, reset
volumes, publish a release, or deploy to a remote service. Follow the existing auth
bootstrap flow using the secret file; do not copy the secret into shared logs.

Successful startup means the existing health gates and `/ready` passed. It does
not prove synthetic actor ingestion, acknowledged-record durability, restore
correctness, tenant isolation, or a sustained capacity limit. Those require the
separate integration and release gates.
