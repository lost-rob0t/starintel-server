# StarIntel Server agent contract

This repository is worked **issue-first**. Do not invent a roadmap while an issue already owns the work.

## Authority for an issue run

For `/issue N`, task-specific authority is intentionally bounded to:

1. GitHub issue `lost-rob0t/starintel-server#N`, including its current body and status.
2. The current local `starintel-server` checkout.
3. The current local `starintel-auto-research` checkout, but only the research/design records directly relevant to the issue.

Treat remote research links in old issue prose as locators for the local `starintel-auto-research` checkout. Do not recursively browse unrelated repositories or load the whole research corpus. A cross-repository implementation dependency named by the issue may be inspected only when it is necessary to verify a hard boundary; do not turn that into scope expansion.

Current source beats stale factual claims in an issue. The issue still owns intent and acceptance criteria. If current source proves part of the issue is already fixed, narrow the issue instead of reimplementing it.

## Local research lookup

Prefer a sibling checkout named `starintel-auto-research` near this repository. If needed, locate an already-present local checkout. Do not clone/fetch a research repository merely to begin an issue run.

Load only directly linked or strongly matching research/design files. Record the exact local paths used in the final evidence.

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