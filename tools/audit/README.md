# CouchDB operational audit source

Narrow optional sink/CLI slice of server #194, under #46/#121. This is **not** the billing ledger,
a replacement actor registry, or a new StarIntel public document release. The live
consumer lock remains release 0.9.1 over immutable document schema 0.9.0. The
operational envelope has its own `starintel.audit.v1` discriminator and lives in a
separate database, outside ordinary document ingestion.

## Install and provision

```sh
python3 -m pip install ./tools/audit
export STARINTEL_AUDIT_URL=http://127.0.0.1:5984/starintel_audit_system
export STARINTEL_AUDIT_TENANT=system
export STARINTEL_AUDIT_USER=audit-admin
export STARINTEL_AUDIT_PASSWORD_FILE=/run/credentials/audit-admin-password
starintel-audit init
```

`init` is an explicit administrator operation. It creates a database, nonempty
membership security, validation, and history views. Re-running is idempotent only
for the same security/design; it refuses to overwrite different existing policy.
Provision CouchDB users separately. Runtime writers need `audit-writer:system`;
CLI readers need `audit-reader:system`. Change the suffix for each tenant. Switch
the environment to the appropriate dedicated reader/writer after provisioning.
**Never run the bot as a CouchDB administrator.**

Use one database and credential scope per tenant. CouchDB `_security` is database
scoped; CLI filters are not access control. A writer can also read its own tenant's
database. Keep CouchDB off the public Internet. Python supports verified HTTPS or
loopback HTTP; private-network HTTP requires explicit
`STARINTEL_AUDIT_ALLOW_INSECURE=1`. The Lisp adapter deliberately supports only a
127.0.0.1 port or authenticated local tunnel. Credentials in URLs and redirects
are rejected. Passwords come from files, not command arguments or Discord.

## View, follow, replay

```sh
starintel-audit list --limit 50
starintel-audit list --type billing.payment.failed --limit 100
starintel-audit list --before 2026-09-16T20:00:00.000000Z
starintel-audit show audit-<64-hex-digest>
starintel-audit tail --cursor ~/.local/state/starintel/audit-system.cursor
starintel-audit tail --since 0 --type billing.payment.succeeded --cursor ./replay.cursor
```

Output is JSON Lines. Existing cursor files take precedence over `--since` and
must match the database/tenant. `_changes` sequences stay opaque, not numeric.
Checkpoints are atomically replaced, mode 0600, **after** output is flushed. A
crash between output and checkpoint can repeat records: consumers deduplicate by
`_id`. Transient follow failures retry six times with bounded backoff, then return
nonzero. No polling thread is spawned by importing the package.

`append` reads one bounded validated JSON event from stdin. Application code uses
`event(...)` and `CouchAudit.append(...)`. IDs are SHA-256 over newline-separated
`tenant_id`, `source`, `event_id`; bounded ASCII identifiers cannot contain a
newline. Replaying identical content succeeds; changing content under the same
ID fails. Every document records action, principal, resource, outcome, correlation,
source, UTC timestamp and an allowlisted details object. Unknown/secret fields
are rejected before transport. No raw bodies, credentials, email addresses,
payment-card data, or free-form customer metadata belong in this envelope.

## Common Lisp authorization hook

Load `starintel-audit.asd` alongside the already-loaded server, then configure via
trusted host composition in starintel-biz. Loading this ASDF system is inert.

```lisp
(asdf:load-system :starintel-audit)
(let ((writer
        (star.audit:make-couchdb-writer
         :database-url "http://127.0.0.1:5984/starintel_audit_system"
         :tenant "system"
         :username "audit-writer"
         :password (string-trim '(#\Newline #\Return)
                                (uiop:read-file-string
                                 #p"/run/credentials/audit-writer-password")))))
  ;; Save the returned closure in your owned component for shutdown/hot reload.
  (star.audit:install-authorization-sink (list (cons "system" writer))))
```

The existing `star.authorization:*authorization-audit-sink*` is composed, not
replaced with a new authorization authority. Add exact tenant -> writer mappings;
there is no cross-tenant fallback. Global tenant-less decisions route only to an
explicit `system` mapping. Decision ULIDs supply deterministic timestamps. The
returned uninstall closure restores the previous sink only while it still owns
the hook; stop the old component before reinstalling.

Authorization export is explicitly **best effort**, with a failure callback and
the existing local logger preserved. It is synchronous, bounded by an SBCL
request timeout, and may add latency. It does **not** guarantee durable capture
during a CouchDB outage or atomically commit with a security operation. Strict
fail-closed audit needs separately approved durable-audit policy. Billing uses the
PostgreSQL transactional outbox in starintel-biz and retries independently.

CouchDB validation rejects updates/deletes by runtime roles. Administrators can
change design documents, purge records, or restore backups: this is **not WORM or
cryptographic tamper evidence against a CouchDB administrator**. SHA-256 IDs are
identities, not signatures. Backups/immutable export and retention are operator
policy; no automatic purge is installed.

## Verification

```sh
PYTHONPATH=tools/audit python3 -m unittest discover -s tools/audit/tests -v
```

The JavaScript validator test executes in Node; it is not a real CouchDB test.
The companion Biz integration check is provided for PostgreSQL and CouchDB;
its execution requires a disposable stack and is not claimed by the unit results.
Full server ASDF/schema/native gates and independent issue voters have not been
run in the implementation environment. Keep the PR draft until those gates and
an actual live integration pass; no main/master ref or production config is changed.
