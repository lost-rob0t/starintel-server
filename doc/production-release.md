# Production release evidence (#343)

No production-readiness claim or version tag is justified by the verifier's unit
suite. The six runtime gates below must execute on the same clean candidate.
Historical #331/#342 results apply only to their recorded commits. This procedure
adds no schema authority, signing system, deployment, or automatic release.

Use the existing pinned Nix builds on x86_64 Linux. Record the full Git SHA,
SHA-256 of `flake.lock`, `qlfile.lock`, and `schema/starintel-schema.lock.json`,
and the immutable OCI digests of server, CouchDB, RabbitMQ, Clouseau, and Valkey.
Server implementation versions and StarIntel schema 0.10.1 are separate values.
Do not change the schema release to name a server RC. Build twice in clean stores
if claiming byte-for-byte reproducibility; a locked derivation alone is not that
proof. Keep credentials outside source trees, Nix inputs, artifacts, and logs.

The manifest is an operator-reviewed evidence index, not authenticated proof.
Keep it and its sanitized proof files in a directory outside the clean checkout.
Its structure is:

- `format`: 1; `source`: full candidate SHA.
- `locks`: map of the three repository-relative paths above to lowercase SHA-256.
- `images`: keys `server`, `couchdb`, `rabbitmq`, `clouseau`, `valkey`, each an
  immutable `sha256:` digest (64 lowercase hexadecimal characters).
- `gates`: the six keys below. Each record requires `source`, `status: "passed"`,
  `execution: "service-backed"`, actual `command`, positive integer `assertions`,
  integer `exitCode: 0`, and `evidence`.
- Every `evidence` is `{ "path": "relative-file", "sha256": "64-hex-digest" }`.
  Paths must resolve inside the manifest directory. Do not include secret output.
- `ci`: `source`, `status: "passed"`, `evidence`; preserve fresh API exports with
  the required check names, conclusions, run URLs, and exact checked SHA.
- `review`: `source`, `status: "approved"`, `blocking: false`, `evidence`;
  preserve applicable review decisions and independent review notes. An assistant
  review is not a human approval or a substitute for repository protection.

Required gate meanings:

| Key | Required actual proof |
| --- | --- |
| startup | One-command clean Linux stack, pinned images, configured init.lisp, health/readiness, persistent storage. |
| ingestion | Representative synthetic actor through RabbitMQ, schema rejection, idempotency, durable retry, supported client readback, acknowledged-record survival after crash/restart. |
| isolation | Two tenants: reads, writes, search, attachments and changes feeds; bounded request and query work. |
| recovery | Backup restored to a clean instance with identical records; tested upgrade and rollback; observable error and queue state. |
| load | Sustained, declared workload with real service memory, backlog and latency measurements. |
| integration | All slices integrated at the candidate SHA, end-to-end acceptance and independently reviewed release evidence. |

`load.measurements` requires numeric `durationSeconds`, `requests`,
`p95Milliseconds`, `peakMemoryBytes`, `peakQueueBacklog`, `finalQueueBacklog`,
`errors`, `concurrency`, and `documents`. Duration, requests, memory, concurrency,
and documents must be positive; errors and final backlog must be zero.
`load.testedLimits.description` states host resources, document sizes, duration,
load shape, and resulting tested limits. Reviewers must judge whether the duration
and thresholds meet the approved milestone. The minimum sustained soak is 3600 seconds; shorter runs remain smoke tests.
No universal throughput threshold is invented here. Include raw samples, chosen thresholds, and failure
criteria in the referenced evidence. Short smoke tests do not establish soak.

Concrete subproofs are mandatory, not an arbitrary aggregate passed flag:

- `ingestion.measurements`: positive integers `publisherConfirms`,
  `expectedRecords`, `persistedRecords`; persisted count must equal expected and
  confirmations must cover it. `persistedEquality`, `duplicateReplay`,
  `restartReadback`, `schemaRejection`, `supportedClientReadback` must be true.
- `recovery.measurements`: `cleanInstance`, `identicalRecords` true; `upgrade`,
  `rollback` both `"passed"`; distinct immutable `baselineImage` and
  `candidateImage`, with candidate equal to `images.server`. Same-image restart
  cannot count as upgrade/rollback.
- `isolation.coverage`: records for `reads`, `writes`, `search`, `attachments`,
  `changes`, each with nonzero integer `assertions` and `status: "passed"`.
  Attachments/changes may instead explicitly record `supported: false`,
  `status: "inaccessible"`, `routeDenied: true`, `directDatabaseExposed: false`,
  and a nonempty `reason`, backed by actual denial/network boundary assertions.
  This records unavailable surfaces without claiming enabled isolation or adding
  new APIs. Supported reads/writes/search must pass. `boundedRequests` and
  `boundedQueryWork` must both be true on the isolation gate.

Run the structural check after independently inspecting the underlying evidence:

```sh
python3 scripts/readiness_release.py /tmp/rc-evidence/manifest.json \
  --source-root "$PWD" --expected-head "$(git rev-parse HEAD)"
```

This command checks bindings, digests, required fields, and a clean matching
checkout. It does not run the six gates, query live reviews, authenticate claims,
or manufacture success records. Synthetic unit manifests only test rejection
logic. Missing service access or failed tests remain blockers.

Immediately before release, re-read the remote branch, required checks and reviews;
if the head changes, repeat affected evidence on the new candidate. Confirm the
proposed Git tag and registry version do not exist. Never force, overwrite or
retag an existing version. Create a new annotated Git tag only after these gates
and applicable review requirements pass; push only the explicit new tag. Confirm
the remote tag resolves to that exact candidate before publication. A Git tag
conflict must stop publication. Keep verified RC image digests and the evidence
index with release artifacts. No tagging/publishing command is automated here.

The existing stack workflow publishes only a full-commit candidate, never a
semantic version or mutable dependency image. It checks candidate absence and
fails closed on ambiguous registry errors. Registry tag checks are not an atomic
immutability guarantee: configure registry protection and serialize publication
before treating tags as immutable; consume OCI digests for exact identity.
