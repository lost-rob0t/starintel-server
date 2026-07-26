# Nix-built Compose stack

The production-shaped local stack uses project-owned OCI images built by the
Nix flake:

- StarIntel Server 0.1.0
- Apache CouchDB 3.5.2
- Clouseau 3.3.0 on Java 21
- RabbitMQ 4.3.4 with the management plugin

CouchDB is pinned by its linux/amd64 manifest digest and Nix content hash.
Clouseau is pinned by its release artifact hash. Compose never builds or pulls
a project-owned image.

## Prerequisites

- Nix with flakes enabled
- Docker Engine
- Docker Compose v2
- `curl`, `jq`, and `openssl` for the operator commands below

## Build and load

Build every project-owned image without loading it:

```bash
nix build .#star-server-image .#couchdb-image .#clouseau-image
```

Build the images, merge their archives, and load them into Docker:

```bash
nix run .#load-images
```

The loaded tags are `starintel/server:0.1.0`,
`starintel/couchdb:3.5.2`, and `starintel/clouseau:3.3.0`.

## Configure secrets

Compose reads credentials from files and mounts them as Docker secrets. Secret
values are not stored in the image, Compose file, or `.env`.

```bash
cp .env.example .env
install -d -m 0700 secrets
openssl rand -base64 32 > secrets/couchdb_password
openssl rand -base64 48 > secrets/couchdb_secret
openssl rand -hex 24 | tr '[:lower:]' '[:upper:]' > secrets/erlang_cookie
openssl rand -base64 32 > secrets/rabbitmq_password
chmod 0600 secrets/*
```

The Erlang cookie is shared only by CouchDB and Clouseau. Change usernames,
bind addresses, ports, the credentials directory, or the Clouseau heap in
`.env`.

Validate the fully resolved Compose model before starting it:

```bash
docker compose config --quiet
```

## Start and operate

```bash
nix run .#load-images
docker compose up --detach --wait
docker compose ps
docker compose logs --follow
```

The default host bindings are:

- StarIntel API: `http://127.0.0.1:5000`
- CouchDB: `http://127.0.0.1:5984`
- RabbitMQ AMQP: `127.0.0.1:5672`
- RabbitMQ management: `http://127.0.0.1:15672`

Clouseau is internal-only. CouchDB and Clouseau share an Erlang cookie and
CouchDB addresses the search node as
`clouseau@clouseau.starintel.internal`.

Stop containers while keeping all data:

```bash
docker compose down
```

Delete the stack and all persisted data:

```bash
docker compose down --volumes
```

The destructive command above removes the `couchdb_data`, `clouseau_index`,
and `rabbitmq_data` volumes.

## Search initialization and verification

StarIntel creates the application database and installs
`source/views/search.json` during startup. Verify indexing through the server:

```bash
password="$(<secrets/couchdb_password)"
curl --fail --user "admin:${password}" \
  --header 'Content-Type: application/json' \
  --request PUT \
  --data '{"dtype":"note","content":"starintelftsfixture"}' \
  http://127.0.0.1:5984/starintel/fts-fixture

curl --fail --get \
  --data-urlencode 'q=content:starintelftsfixture' \
  http://127.0.0.1:5000/search | jq
```

The automated acceptance test builds and loads all images, waits for every
health check, verifies FTS, restarts the stack, and verifies document and index
recovery:

```bash
./scripts/stack-test.sh
```

## Migration

Export each application database from the old stack while writes are stopped:

```bash
password="$(<secrets/couchdb_password)"
curl --fail --user "admin:${password}" \
  'http://127.0.0.1:5984/starintel/_all_docs?include_docs=true&attachments=true' |
  jq '{docs: [.rows[].doc
      | select(._id | startswith("_design/") | not)
      | del(._rev)]}' > starintel-backup.json
```

Start the new stack, then restore:

```bash
password="$(<secrets/couchdb_password)"
curl --fail --user "admin:${password}" \
  --header 'Content-Type: application/json' \
  --data-binary @starintel-backup.json \
  http://127.0.0.1:5984/starintel/_bulk_docs
```

Repeat for `starintel-event-source` and any additional databases. Search
indexes are derived data; Clouseau rebuilds them when the restored databases
are queried. Keep the old volumes until document counts and representative FTS
queries match.

## Persistence and backup

Named volumes preserve CouchDB documents, Clouseau indexes, and RabbitMQ state
across container replacement. Back up CouchDB through its HTTP API rather than
copying a live volume. The export command in the migration section includes
attachments; store its output in encrypted backup storage. Back up every
application database and test a restore regularly.

The Clouseau volume improves restart time but does not need an independent
backup because its indexes can be rebuilt from CouchDB.

## Upgrade

1. Read the CouchDB and Clouseau release notes and compatibility requirements.
2. Export every CouchDB database.
3. Update the pinned versions, digest, and hashes in `nix/images.nix`.
4. Update the matching image tags in `docker-compose.yml`.
5. Run `nix build` for all three images and `./scripts/stack-test.sh`.
6. Load the new images and recreate the stack with
   `docker compose up --detach --wait`.
7. Verify health, document counts, and representative FTS queries before
   removing old images or backups.

For Clouseau 3.x, Java 21 and CouchDB 3.5 or newer are required.
