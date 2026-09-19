# StarIntel Server agent contract

This repository is worked **issue-first**. Do not invent a roadmap while an issue already owns the work.

## Authority for an issue run

For `/issue N`, task-specific authority is intentionally bounded to:

1. GitHub issue `lost-rob0t/starintel-server#N`, including its current body and status.
2. The current local `starintel-server` checkout.
3. The current local `starintel-auto-research` checkout, but only the research/design records directly relevant to the issue.

Treat remote research links in old issue prose as locators for the local `starintel-auto-research` checkout. Do not recursively browse unrelated repositories or load the whole research corpus. A cross-repository implementation dependency named by the issue may be inspected only when it is necessary to verify a hard boundary; do not turn that into scope expansion.

Current source beats stale factual claims in an issue. The issue still owns intent and acceptance criteria. If current source proves part of the issue is already fixed, narrow the issue instead of reimplementing it.

## StarIntel schema/version preflight

Before any work that reads, writes, validates, serializes, ingests, migrates, indexes,
or reasons about StarIntel documents or schema-dependent API behavior, resolve and
verify this repository's schema lock:

```bash
python3 scripts/check-starintel-schema-lock.py schema/starintel-schema.lock.json
```

If the reusable `starintel-spec-version` skill is installed, also use its read-only
resolver to make the version dimensions explicit:

```bash
python3 "$HOME/skills/skills/starintel-spec-version/scripts/starintel_spec_version.py" \
  current --lock schema/starintel-schema.lock.json
```

Rules:

- `schema/starintel-schema.lock.json` is the consumer authority for the canonical
  repository, canonical commit, active `release_version`, and immutable
  `schema_version`.
- Report/use `release_version` as the active StarIntel release. Do **not** infer the
  release from `starintel-doc-v0.9.0.schema.json` or another schema filename.
- The v0.9 base schema may remain `schema_version = 0.9.0` while the release/profile
  advances through `0.9.1`, `0.9.2`, and later additive releases.
- Issue prose, research notes, README text, and remembered version numbers never
  override the lock and pinned canonical manifest.
- Never hand-edit a release/profile bump in this repository. Canonical release
  changes occur through the canonical schema repository's bump script; this
  repository then repins its lock through the existing schema sync/lock workflow.
- A lock/manifest/commit mismatch is a hard blocker. Do not implement against an
  unverified or guessed contract.

At the time this rule was added, this lock resolves release `0.9.1` over immutable
base schema `0.9.0`; future agents must read the live lock instead of trusting this
historical value.

## Local research lookup

Prefer a sibling checkout named `starintel-auto-research` near this repository. If needed, locate an already-present local checkout. Do not clone/fetch a research repository merely to begin an issue run.

Load only directly linked or strongly matching research/design files. Record the exact local paths used in the final evidence.

## Source-orchestrator, Auto-Dig targets, and actor identity protocol

This section governs source-discovery, source-coverage, and missing-actor work.

The source-orchestration path is **native StarIntel**. Do not introduce Agent Zero
or another external agent framework into this path unless an issue explicitly
changes this contract. The AI source-orchestrator queries the canonical StarIntel
document graph/database, reads actor manifests and unresolved targets, and emits
normal StarIntel targets. It does not create a second source database or a second
target queue.

The canonical source-orchestrator actor name is:

```text
star:v1:orchestrator:source
```

It may reason over source documents, event documents, actor manifests, target
documents, source-health state, provenance, geo coverage, and corroboration
coverage. Its normal output is a target document. GitHub issue creation is a CI
projection of unresolved targets, not the orchestrator's primary persistence
mechanism.

### Actor-name protocol

New actor identities created by this workflow MUST use this wire-compatible
protocol:

```text
star:v1:<class>:<capability>[:<provider-or-variant>]
```

Allowed classes for this workflow are:

- `orchestrator` — plans/query-driven work and emits targets;
- `collector` — ingests an external source or protocol;
- `resolver` — resolves/normalizes data such as geography or entities;
- `correlator` — joins claims/evidence/events across sources;
- `projector` — derives a presentation/query projection from canonical docs.

Naming rules:

1. Names are lowercase ASCII and must remain valid under the runtime actor-name
   contract `^[A-Za-z0-9][A-Za-z0-9._:-]*# StarIntel Server agent contract

This repository is worked **issue-first**. Do not invent a roadmap while an issue already owns the work.

## Authority for an issue run

For `/issue N`, task-specific authority is intentionally bounded to:

1. GitHub issue `lost-rob0t/starintel-server#N`, including its current body and status.
2. The current local `starintel-server` checkout.
3. The current local `starintel-auto-research` checkout, but only the research/design records directly relevant to the issue.

Treat remote research links in old issue prose as locators for the local `starintel-auto-research` checkout. Do not recursively browse unrelated repositories or load the whole research corpus. A cross-repository implementation dependency named by the issue may be inspected only when it is necessary to verify a hard boundary; do not turn that into scope expansion.

Current source beats stale factual claims in an issue. The issue still owns intent and acceptance criteria. If current source proves part of the issue is already fixed, narrow the issue instead of reimplementing it.

## StarIntel schema/version preflight

Before any work that reads, writes, validates, serializes, ingests, migrates, indexes,
or reasons about StarIntel documents or schema-dependent API behavior, resolve and
verify this repository's schema lock:

```bash
python3 scripts/check-starintel-schema-lock.py schema/starintel-schema.lock.json
```

If the reusable `starintel-spec-version` skill is installed, also use its read-only
resolver to make the version dimensions explicit:

```bash
python3 "$HOME/skills/skills/starintel-spec-version/scripts/starintel_spec_version.py" \
  current --lock schema/starintel-schema.lock.json
```

Rules:

- `schema/starintel-schema.lock.json` is the consumer authority for the canonical
  repository, canonical commit, active `release_version`, and immutable
  `schema_version`.
- Report/use `release_version` as the active StarIntel release. Do **not** infer the
  release from `starintel-doc-v0.9.0.schema.json` or another schema filename.
- The v0.9 base schema may remain `schema_version = 0.9.0` while the release/profile
  advances through `0.9.1`, `0.9.2`, and later additive releases.
- Issue prose, research notes, README text, and remembered version numbers never
  override the lock and pinned canonical manifest.
- Never hand-edit a release/profile bump in this repository. Canonical release
  changes occur through the canonical schema repository's bump script; this
  repository then repins its lock through the existing schema sync/lock workflow.
- A lock/manifest/commit mismatch is a hard blocker. Do not implement against an
  unverified or guessed contract.

At the time this rule was added, this lock resolves release `0.9.1` over immutable
base schema `0.9.0`; future agents must read the live lock instead of trusting this
historical value.

## Local research lookup

Prefer a sibling checkout named `starintel-auto-research` near this repository. If needed, locate an already-present local checkout. Do not clone/fetch a research repository merely to begin an issue run.

Load only directly linked or strongly matching research/design files. Record the exact local paths used in the final evidence.

.
2. Protocol segments use `:`; words inside a segment use kebab-case.
3. The exact registered string is the actor identity. Do not silently rename an
   actor after targets or leases exist; add an explicit compatibility alias or
   migration.
4. Prefer a reusable capability actor over a source-instance actor. A Telegram
   channel, news outlet, theater, conflict, or individual feed belongs in source
   configuration/targets unless it genuinely requires a different protocol or
   execution capability.
5. Do not encode a war/theater into a generic actor name. Ukraine, Iran, Sudan,
   etc. are target/data scope, not collector identity.
6. A provider/variant segment is justified only when it changes protocol,
   authentication, execution, or parsing capability enough that one actor cannot
   safely own both variants.

Canonical examples:

```text
star:v1:orchestrator:source
star:v1:collector:telegram
star:v1:collector:acled
star:v1:collector:gdelt
star:v1:resolver:geo
star:v1:correlator:war-event
star:v1:projector:battle-map
```

A channel such as `deepstateua` is therefore a source handled by
`star:v1:collector:telegram`, not a new actor named after the channel.

### Auto-Dig target protocol

Source-orchestration targets use deterministic logical identities of the form:

```text
star-target:v1:<kind>:<stable-subject-id>
```

The live StarIntel schema lock remains authoritative. Encode the following
logical fields inside the canonical target data/extensions allowed by that lock;
do not add ad-hoc top-level schema fields:

- `kind`;
- `scope` (dataset/theater/topic/event class as applicable);
- `source_type` or source protocol;
- `source_locator` when a concrete source is already known;
- `evidence_document_ids`;
- `required_capabilities`;
- `candidate_actor_name` when an actor gap is suspected;
- `reason`;
- `confidence`;
- `dedupe_key`;
- `created_by` using the canonical actor identity.

Defined target kinds:

- `source-discovery` — search for additional public sources for an identified
  coverage need.
- `source-onboard` — a known source is already supported by an existing actor;
  add/validate its source manifest/configuration and fixtures.
- `actor-gap` — a useful source/protocol is known but no registered actor
  satisfies its required ingestion capability.
- `coverage-gap` — the database has insufficient source diversity/volume for a
  defined theater, topic, geography, or event class.
- `corroboration-gap` — a claim/event lacks sufficient independent supporting
  or contradicting evidence.
- `geo-gap` — a document/event lacks usable geography or has geo confidence
  below the configured threshold.
- `source-health` — an existing source is stale, failing, rate-limited,
  structurally changed, or otherwise degraded.
- `actor-audit` — manifests/registry claim support, but CI/runtime evidence
  cannot prove the actor can accept and process the target correctly.

Classification is strict:

- If an existing actor manifest satisfies the protocol/capability requirements,
  emit `source-onboard`, not `actor-gap`.
- If the actor exists but one source-specific parser/configuration is broken,
  create/fix a bug against that actor; do not create a second actor.
- If no concrete source is known yet, emit `source-discovery` before
  `actor-gap`.
- Emit `actor-gap` only when registry/manifests plus evidence prove no existing
  actor can satisfy the required capability.

### CI projection into issues

CI may materialize unresolved source-orchestration targets into an issue list.
The target remains the machine-readable authority.

For every run CI MUST:

1. query unresolved targets;
2. query the actor registry/manifests;
3. reclassify each target against current capabilities;
4. compute a stable `dedupe_key`;
5. create or update one issue per unresolved dedupe key rather than opening
   duplicates;
6. close/resolve the projected issue only when the corresponding target is
   resolved and CI can prove the required actor/source behavior.

Issue classes should remain visibly distinct, for example:

```text
[actor-gap] star:v1:collector:<provider> — <missing capability>
[source-onboard] star:v1:collector:telegram — <source-id>
[source-health] <canonical actor name> — <source-id>
[geo-gap] <event/document id>
```

An actor-gap issue must include the originating target id(s), evidence document
id(s), source/protocol examples, required capabilities, current registry
evidence, proposed canonical actor name, and executable acceptance criteria.

## Preflight before implementation

1. Fetch/read the issue and confirm it is open, not marked duplicate/superseded, and still applicable.
2. Record the exact starting branch and commit SHA.
3. Re-scan current source and tests for the claimed gap.
4. Check the open StarIntel Server issue set for semantic ownership collisions. Related parent/child issues are allowed; two issues claiming the same invariant are not.
5. Read only the directly relevant local Auto-Research records.
6. Run the five issue-vote subagents defined in `.opencode/agents/`.

Do not mutate production code before this preflight finishes.

## Five-vote YAGNI gate

Use these independent voters:

- `issue-reality` — is the claimed problem still real in current source?
- `issue-ownership` — is this issue the canonical owner, a child slice, or a duplicate/superseded issue?
- `issue-research` — does the smallest proposed direction match relevant local Auto-Research authority?
- `issue-verification` — what is the smallest legitimate RED and completion proof?
- `issue-yagni` — what can be deleted, deferred, reused, or narrowed?

Each voter must return exactly one disposition plus evidence:

`IMPLEMENT | NARROW | DUPLICATE | OBSOLETE | BLOCKED`

The coordinator chooses the **intersection of necessary work**, never the union of suggestions.

Proceed only when at least 3/5 voters support `IMPLEMENT` or `NARROW`, no voter proves `DUPLICATE`/`OBSOLETE`, and no hard dependency or authorization blocker remains. A `NARROW` majority means implement only the agreed smallest slice.

An explicit issue-level approval gate such as `AWAITING_OPERATOR_IMPLEMENTATION_APPROVAL` remains a hard blocker unless that same issue records approval. The vote may recommend approval/narrowing; it does not silently rewrite governance.

## Implementation loop

Once the gate is open:

1. Create or reuse one focused issue branch. Never work directly on `master`.
2. Write the smallest legitimate failing test/proof against the untouched issue baseline.
3. Run it and record the expected RED. A broken environment, typo, missing fixture, or zero-test run is not valid RED.
4. Implement only enough behavior to make that proof GREEN.
5. Refactor only when it reduces complexity without broadening scope.
6. Run focused tests, then the repository-native deterministic gate(s) relevant to the touched subsystem.
7. Inspect the diff adversarially for accidental scope, duplicate authority, hidden side effects, and test weakening.
8. Repeat RED -> GREEN -> verify until the issue acceptance criteria are met or a genuine blocker is proven.
9. Commit coherent changes, push the branch, open/update one focused PR, and continue on the exact PR head until required checks are green.
10. Merge only when the issue explicitly allows implementation/merge, required gates are green on the exact head, and no blocking review remains.

Never make a test green by skipping, xfail, weakening assertions, swallowing errors, or changing the requirement unless current-source evidence proves the issue itself must be narrowed.

## Scope discipline

YAGNI is mandatory:

- Reuse existing runtime, service, adapter, manifest, lease, auth, idempotency, observability, and lifecycle authorities.
- Do not create a second control plane, second DSL, second registry, or second persistence path when one already exists.
- Do not pull future sibling-issue work into the current issue because it is convenient.
- If a newly discovered requirement is independently valuable, record/link a separate issue instead of silently expanding the current one.
- Delete dead proposed scope when current code already solved it.

## Evidence at completion

Report:

- issue number and final disposition;
- starting and final exact SHAs;
- local Auto-Research files consulted;
- voter dispositions and the resulting YAGNI decision;
- RED command/result and GREEN command/result;
- full relevant verification commands/results;
- PR and merge status;
- any follow-up issue created or remaining blocker.

Do not claim a service, integration, CI, hardware, or security test ran unless it actually ran.

## GPL source and generated-code compliance

StarIntel Server is licensed under **GPL-3.0-or-later**. Human contributors,
coding agents, code generators, and release automation must preserve the
repository's license notices and applicable source obligations when conveying
covered StarIntel software or derivatives.

- Keep existing copyright/license notices and SPDX identifiers intact.
- New first-party source files should use
  `SPDX-License-Identifier: GPL-3.0-or-later` where practical.
- Generated or transformed StarIntel code must not silently strip the license,
  source locator, notices, or corresponding-source information.
- When distributing binaries or modified covered software, follow the GPL's
  corresponding-source and license-copy requirements for that mode of
  distribution.
- Do **not** claim that ordinary remote use of the StarIntel HTTP API alone
  triggers an AGPL-style network source-disclosure requirement. This repository
  is GPL-3.0-or-later, not AGPL.
- API and export receipts use `license_scope=server-software`. That metadata
  describes the StarIntel software producing the receipt; it does not
  automatically relicense user datasets under the GPL.

Agents must treat these rules as release/patch invariants, not optional prose.
