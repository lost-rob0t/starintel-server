# Bounded Linux Server persistence acceptance (#341)

`.github/workflows/local-persistence.yml` is one Linux job that reuses the existing
pinned `.#default` server and `.#star-server-image` derivation. This stacked
branch targets existing draft PR #331; it checks out and builds its exact
refreshed candidate `c8ac745a6a6c14f33a083a607c556bf08c65220b`, separately
from the workflow/helper head, and records both identities. It is an authorized
GitHub-hosted execution path, with no changes to the restricted development
machine's proxy, TLS, or denied dependency requests. It does not implement the
remaining Debian installer and is not Android, PRoot, emulator, or device proof.

The job has a 35-minute overall deadline, one build worker/two cores, no matrix,
no schedule, per-step deadlines, cancellation of superseded runs, and one-day
retention for small text/JSON artifacts. It neither deploys nor publishes images.
It does not read repository or production secrets. Build output and actor output
retain at most 1 MiB each; sanitized container logs retain 256 KiB per component.
The Nix image archives and private credential directory are never uploaded.

The job first builds the existing isolated FEC wheel using the unchanged
packaging boundary in pro-actors `6095facbc005caa1251e86936ce2a1339499f195`.
It runs the unchanged maintained `python/tests/fec_canonical_server_probe.py`
(SHA-256 `cb55d04752f6a8afeacd9617215e88de946df070f5b440d3c0adafeaa505e219`)
against a fresh owned stack. The probe exercises the installed sa-fec CLI,
actual target dispatch/handler/publisher confirmations and strict canonical
persisted readback, then replay/rejection controls. Its default provider HTTP
responses are synthetic. No provider paths/keys, fixture overrides, production
credentials or proxy variables reach this probe. The runner accesses only IPs
assigned on the owned internal bridge; no ports are published. The actor
repository is checked out read-only and is not modified or published.

The FEC helper invocation owns its own stack/auth database, allowing the probe's
unchanged first-administrator bootstrap. It cleans up before the separate
canonical HTTP/restart invocation starts; credentials and resources are not
reused between those runs. Both invocations share the already-built Server
image and pinned upstream images. Only a passed FEC proof allows the following
HTTP/restart step to execute. PR #331 remains draft with its FEC verification
hold; this workflow does not authorize merge.

The acceptance helper `scripts/ci-local-persistence.py`:

1. Loads the existing Server image after the supported Nix build succeeds.
2. Starts upstream CouchDB and RabbitMQ at the same immutable image digests used
   by `nix/images.nix`. The dedicated Docker network is internal. CouchDB
   readiness is probed by a small client using the already-built Server image
   (the pinned CouchDB base removes curl); Server HTTP is probed on its own
   container loopback via `docker exec` and curl; **no ports are published**. Credential headers/body travel on stdin,
   with curlrc/proxies/globbing and redirects disabled.
   RabbitMQ readiness uses its authenticated
   [ready-to-serve-clients API](https://www.rabbitmq.com/docs/http-api-reference#get-apihealthchecksready-to-serve-clients),
   checking service readiness, an AMQP listener, and the connection limit.
   Each container has two CPUs, 1 GiB RAM, a 512-PID ceiling, and bounded logs.
3. Uses a new labeled CouchDB volume and randomly generated fixture credentials.
   Server credentials use its existing `*_FILE` entry point and trusted init.
   Auth bypass stays false; leases use the existing explicit memory backend.
   CouchDB single-node setup uses the authenticated
   [official cluster-setup API](https://docs.couchdb.org/en/stable/api/server/common.html#post--_cluster_setup)
   and verifies its state. No read-only config mount conflicts with the upstream
   entry point's ownership initialization. This configures system databases only,
   never the canonical fixture document.
4. Waits for actual dependency health and Server `/ready`, bootstraps an API
   credential through `/auth/bootstrap`, and POSTs one synthetic canonical 0.10.1
   Person through `/api/v1/documents`. POST echo is insufficient: it polls the
   authenticated Server GET until the persisted canonical fields match exactly,
   allowing only the server revision field. No fixture document is inserted
   directly into CouchDB.
5. Gracefully stops Server, restarts CouchDB against the same volume, starts the
   same Server container, and reads the same document with the original API key.
   Readback must equal the pre-restart document and revision.
6. Captures bounded logs with generated credentials redacted, then removes only
   resources whose `ci341.run` ownership label matches this run. Failure stages
   are retained in `persistence.json`, including container state/OOM metadata;
   interrupted diagnostics cannot bypass teardown. INT/TERM are deferred until
   all bounded owned-resource removals and evidence writes complete. Pending
   cancellation marks acceptance failed, then propagates; a definition or contract-test pass is not
   a Server acceptance pass.

Only after persistence succeeds does CI run the existing `star-unit-tests` and
require a nonempty passing `COUCHDB-ACTOR-TESTS` suite. This is real Sento mailbox
unit execution with injected storage functions; it is not an additional
service-backed actor conformance claim or all-language proof. A failed build,
missing suite, skipped stage, timeout, billing failure, or permissions failure
must be reported as such. Do not retry broad runs to hide a blocker.

## Invocation and evidence

A PR touching the exact CI files triggers the job. `workflow_dispatch` also works
once the workflow exists on the default branch. To run the acceptance locally
in an authorized Linux Docker/Nix environment, using disposable CI resources:

```sh
nix build .#star-server-image --out-link /tmp/ci341-server-image
python3 scripts/ci-local-persistence.py \
  --image-archive /tmp/ci341-server-image --evidence /tmp/ci341-evidence
```

This is a test invocation, not an Android install/bootstrap command. Inspect
`persistence.json`, `build-status.json`, `actor-status.json` when present, and
`job-status.json`, `fec-harness.json`, and `fec-canonical-server-proof.json`.
The FEC harness records the exact server/probe commits, unchanged probe digest,
actual phases and publisher-confirm count. Missing proof/persistence/actor files
mean those stages did not run.
GitHub scheduler/billing errors may prevent all steps and artifacts; use the
check-run annotations in that case. Record the exact head and actual executed
stages before claiming success.

## CI-only routing

The five older workflows retain their jobs and production-change behavior. Their
PR triggers ignore **only the exact** new helper/workflow/test/docs paths and the
five workflow configuration files validated by this job. This avoids starting
native unit, search, image publication, docs, and schema jobs merely to validate
this CI-only slice. There are no `source/**`, `scripts/**`, `.github/**`, or general
`tests/**` exclusions. A mixed PR touching any production file triggers the older
gates normally. The new job runs actionlint and contracts covering these routing
changes. Push behavior is unchanged; no merge is authorized by issue #341.

## Preflight and present limits

Baseline: `54a5f65f0fb3ecc286da6fe426213f015a255562`. Active schema preflight:
StarIntel 0.10.1, StarLang `96d26c338078676ec1dc6928755901636c9f7ef0`.
Independent issue votes: reality IMPLEMENT; ownership NARROW; research NARROW;
verification NARROW; YAGNI NARROW. Intersection: this CI coverage slice only.
Research consulted:
`starintel-auto-research/roam/research/starintel-android/STARINTEL-ANDROID-RESEARCH-001-abcl-starlang-field-runtime.org`
(pending research, not native Android design approval).

Local contract RED identified the absent bounded workflow/routing. Local contract
GREEN proves only wiring, canonical comparison, redaction, and ownership-safe
cleanup with test doubles. A real Server behavior RED is not claimed until the
new acceptance actually executes after healthy dependencies. Existing native
ECL port #318, canonical-ingest #319 and their held PRs remain separate. The
Debian/PRoot installer, exact dependency closure, and actual Android execution
remain open under #341 regardless of Linux CI outcome.


## Prerequisite behavior evidence

On master `54a5f65`, run39044299632 (head a4f2e49) passed dependency/build,
CouchDB/RabbitMQ and Server readiness, auth and canonical POST. Actual persisted
GET failed strict shape checking with `unexpected keys=['extensions']`; restart
and Sento steps were not run. This is a genuine behavior RED, not an environment
failure. Existing #319/PR #331 owns the preservation fix. The fixture and both
strict readback assertions remain unchanged; adding extensions to the fixture
or stripping it in the test would conceal the defect and is forbidden.

The refreshed #331 preserves its four-file implementation/regressions and
master's newer tests. Current five-role intersection authorizes only this
prerequisite refresh, unchanged synthetic FEC proof and stacked acceptance.
Latest results must be inspected on the exact new candidate before claiming
GREEN; historical October10 #331 checks do not prove the refresh.
