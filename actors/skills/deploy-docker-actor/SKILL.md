# Deploy a StarIntel actor with Docker Compose

Use this skill when installing or upgrading an actor on a StarIntel host.

## Read first

Read `actors/DEPLOY.md`, `actors/compose.yaml`, the actor's adapter, and the current core `docker-compose.yml`. Do not modify the core database/server services just to deploy an actor.

## Installation procedure

1. Confirm the core stack is healthy and `starintel_backend` exists.
2. Copy `actors/.env.example` to `actors/.env` if the host has no actor environment file.
3. Point `STARINTEL_SECRETS_DIR` at the existing StarIntel secrets directory. Never copy RabbitMQ credentials into `.env`, Dockerfile, image labels, or git.
4. Run `actors/scripts/fetch-playwright-seccomp.sh` for browser actors.
5. Add/verify the actor service in `actors/compose.yaml` and `actors/compose.seccomp.yaml`.
6. Use a versioned immutable image tag. Do not deploy `latest`.
7. Build or pull the image.
8. Run fixture/unit tests and live `verify` using that exact image.
9. Run `docker compose ... config --quiet`.
10. Deploy only the requested actor with `up -d --no-deps --wait <service>`.
11. Inspect health and logs.
12. Send a normal StarIntel target to the actor and verify canonical output reaches the normal ingest pipeline.

## Compose contract

Website actors join `${STARINTEL_DOCKER_NETWORK:-starintel_backend}`. They consume canonical `documents.target.dispatch.<actor>`, may bind compatibility `actors.<actor>.new.target`, and publish output only to `documents.ingest.<dtype>`.

## Rollback

Rollback is image-tag based: restore the prior tag and recreate only that actor with `--no-deps`. Do not roll back or restart CouchDB/RabbitMQ/star-server for an actor-only failure.
