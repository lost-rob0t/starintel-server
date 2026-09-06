# StarIntel actor Docker deployment

The actor plane is deliberately separate from the core StarIntel Compose stack. The core stack owns CouchDB, RabbitMQ, Valkey, Clouseau, and `star-server`; `actors/compose.yaml` owns independently deployable external actors and joins the existing `starintel_backend` network.

## Prerequisites

- Docker Engine and Docker Compose v2.
- The normal StarIntel server stack is running from the repository root.
- The server-created `starintel_backend` Docker network exists.
- The existing RabbitMQ password file is available under the server secrets directory.
- `curl` and Python 3 are available on the deploy host to install/validate the Playwright seccomp profile.

Check the core stack and network:

```bash
docker compose ps
docker network inspect starintel_backend >/dev/null
```

## Configure the actor stack

```bash
cd actors
cp .env.example .env
```

The defaults assume the actor stack lives in this repository and shares `../secrets/rabbitmq_password` with the core server stack. Do not copy the password into `.env` or bake it into an image.

For browser crawlers, install the seccomp profile matching the pinned Playwright version:

```bash
./scripts/fetch-playwright-seccomp.sh
```

Playwright 1.62.0 is pinned both in the Python package and the browser image build. The production Compose command below applies the matching seccomp override while the container runs as an unprivileged user with host IPC for Chromium.

## Build

```bash
docker compose -f compose.yaml -f compose.seccomp.yaml build wef-ygl
```

The resulting default tag is `starintel/actor-wef-ygl:0.1.0`. Override `WEF_YGL_IMAGE` in `.env` when publishing or rolling a versioned registry tag.

## Verify before deployment

Run the adapter against the current public WEF site inside the exact image that will be deployed:

```bash
docker compose -f compose.yaml -f compose.seccomp.yaml \
  run --rm --no-deps wef-ygl \
  verify wef-ygl --browser auto --min-people 100
```

Then validate the Compose model:

```bash
docker compose -f compose.yaml -f compose.seccomp.yaml config --quiet
```

## Deploy

```bash
docker compose -f compose.yaml -f compose.seccomp.yaml \
  up -d --wait wef-ygl
```

Inspect it:

```bash
docker compose -f compose.yaml -f compose.seccomp.yaml ps
docker compose -f compose.yaml -f compose.seccomp.yaml logs -f wef-ygl
```

The worker declares durable queue `starintel.actor.wef-ygl` on the `documents` exchange and binds both canonical `documents.target.dispatch.wef-ygl` and compatibility `actors.wef-ygl.new.target`.

New targets should use the normal StarIntel target pipeline with actor `wef-ygl`. The actor publishes derived canonical v0.9 documents back to `documents.ingest.<dtype>`; it does not write CouchDB directly.

## Upgrade

Build or pull the new immutable image tag, update `WEF_YGL_IMAGE`, run the live verify command against that tag, then recreate only the actor:

```bash
docker compose -f compose.yaml -f compose.seccomp.yaml \
  up -d --no-deps --wait wef-ygl
```

The server, RabbitMQ, and databases are not recreated.

## Rollback

Set `WEF_YGL_IMAGE` back to the last known-good tag and run:

```bash
docker compose -f compose.yaml -f compose.seccomp.yaml \
  up -d --no-deps --wait wef-ygl
```

Because output IDs are deterministic and StarIntel processing is at-least-once, replaying a target after rollback is expected to be idempotent.

## Stop

```bash
docker compose -f compose.yaml -f compose.seccomp.yaml stop wef-ygl
```

Do not use `down -v` as a routine actor operation; the actor stack does not need persistent data volumes by default.
