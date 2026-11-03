# Synthetic recovery gate (#343)

This drill is for a local disposable stack, with synthetic fixtures only. It is
not a general production backup tool. Stop fixture writers and wait for broker
ready/unacknowledged messages and in-flight writes to drain before export. API
publication acknowledgement alone does not establish CouchDB persistence.

Use a fresh Compose project and fresh volumes for the restore instance. Never
remove production volumes to make a test pass. Store runtime secrets outside the
checkout; pass only the password file path. The script restricts endpoints to
loopback and database names to `readiness-*`, refuses HTTP redirects, emits no
response bodies or credentials, and creates backup files exclusively with mode
0600. Use private encrypted storage for backups. The operator must keep the
source database quiescent throughout the export; this tool does not provide a
cross-database consistent snapshot.

```sh
python3 scripts/readiness_recovery.py backup \
  --url http://127.0.0.1:5984 --database readiness-source \
  --password-file /tmp/readiness-secrets/couchdb_password \
  --backup-file /tmp/readiness-backup.json
python3 scripts/readiness_recovery.py restore \
  --url http://127.0.0.1:15984 --database readiness-restored \
  --password-file /tmp/readiness-restore-secrets/couchdb_password \
  --backup-file /tmp/readiness-backup.json
```

The destination database must not exist, including as an empty database. Restore
checks every `_bulk_docs` result and reads all documents back with attachment
bodies. Sorted canonical JSON hashes must match. CouchDB revision and generated
attachment transport metadata are excluded; IDs, application fields, attachment
names, media types and bytes are preserved. Design documents are rejected: the
application owns index initialization. This drill excludes deleted revision
history, conflicting revisions, local documents, database security settings,
authentication databases, broker state and multi-database recovery. Export
loads the snapshot into memory and is intended for bounded synthetic fixtures.

A failed restore leaves its newly created database for diagnosis; it does not
delete or retry over it. Use another fresh database after resolving the failure.
A local write failure can leave an incomplete exclusive backup file; inspect it
and choose a new filename. Do not treat a checksum as an authenticity signature.

## Upgrade and rollback evidence

An upgrade/rollback gate requires **different immutable exact image identities**,
not two mutable tags or restarting one image. Record baseline and candidate
server image IDs/digests plus dependency identities and schema locks before
starting. With synthetic records already acknowledged and observed in CouchDB:

1. Capture a backup and source normalized hash; verify a clean-instance restore.
2. Start the candidate against the retained test volumes; record actual image
   IDs, successful readiness, supported-client document/attachment reads, search
   results, record count and normalized hash.
3. Stop writes, switch back to the recorded previous exact server image, retain
   test volumes, and repeat the same observed checks. Never downgrade database
   engines in place unless their documented storage compatibility permits it;
   otherwise restore the pre-upgrade backup to a clean previous stack.
4. Fail on a missing image, identical image identities, failed read, mismatch,
   or incompatible schema. Record exact commands, exit codes and image identities.

This document does not certify that upgrade, rollback or live restore passed.
The Python tests cover filesystem behavior and fake HTTP/error handling only.
Until actual two-instance restore and distinct-image upgrade/rollback evidence
exists, the production recovery gate remains **NOT VERIFIED**.

## Existing diagnostics

Reuse existing Compose logs and RabbitMQ management queue statistics (ready,
unacknowledged, consumers), and existing `starintel_rabbit_consumed_total` /
`starintel_rabbit_consume_failures_total` telemetry when enabled. Record queue
snapshots before quiescing and after restore/restart. Keep secrets and document
bodies out of artifacts. Do not infer successful processing from a drained queue
alone; compare acknowledged fixture identities through the supported client.

```sh
python3 -m unittest discover -s tests -p test_readiness_recovery.py
```

## Executed CouchDB-only drill — 2026-10-20

The helper passed against two real, isolated CouchDB instances in the saved
execution environment. This is partial recovery evidence; the full server,
RabbitMQ ingestion, supported client, upgrade and rollback were not exercised.

- Pull command: `docker pull couchdb:3.5.2` (exit 0).
- Exact image used for both instances:
  `couchdb@sha256:5fc596110eac7f412173a7d9de14f70aa5d3e816790fb6e70642755d4a4cf464`.
- Driver command: `python3 /tmp/star-readiness-live-recovery.py` (exit 0).
  The temporary driver used `docker volume create`, `docker create --name
  <unique-name> --publish 127.0.0.1::5984 --mount
  type=volume,src=<unique-volume>,dst=/opt/couchdb/data --entrypoint /bin/sh
  <exact-image> -c <startup-script>`, `docker cp <private-password-file>
  <container>:/run/readiness-password`, then `docker start` for each instance.
  The startup script wrote the random password into a private CouchDB config,
  removed its temporary password copy, and exec'd the official entrypoint.
  No credential was passed in command arguments or environment variables.
- Each instance had its own newly created volume; both reached HTTP `/_up` 200.
- Inserted two synthetic records into `readiness-source`; one carried a 27-byte
  attachment containing a NUL and newline. No writers ran during export.
- Executed both helper CLI commands from the example above, using dynamically
  allocated loopback ports and a temporary private password file/backup path.
- Backup and restore each returned `status: passed`, `records: 2`, and SHA-256
  `576e58188b068dd875c34ab0c1dccb85688109025c6968e931314cca0a1ec45a`.
- Independently reread both databases and compared normalized hashes; identical.
  A second restore into the existing destination was refused as required.
- `docker rm --force <owned-container>` and `docker volume rm <owned-volume>`
  removed both owned containers and volumes. Temporary password/backup files
  were removed. The pulled image remains cached.
- Re-ran `python3 -m unittest discover -s tests -p test_readiness_recovery.py`:
  10 passed, including simulated partial bulk errors.

The driver's informational attachment-length label initially printed 26;
`len(b'synthetic attachment\x00bytes\n')` verified the correct length is 27.
This labeling error did not affect actual byte equality or the checksum proof.
