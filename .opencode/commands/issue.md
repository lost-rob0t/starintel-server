---
description: Audit, vote, and execute one StarIntel Server issue
agent: build
subagent: false
---

Work `lost-rob0t/starintel-server` issue **#$1** from end to end under the repository `AGENTS.md` contract.

First read the current issue body/status and current local source. Then locate only the directly relevant records in the already-present local `starintel-auto-research` checkout.

Before any production-code edit, run all five named read-only voters independently:

- `issue-reality`
- `issue-ownership`
- `issue-research`
- `issue-verification`
- `issue-yagni`

Require each to return `IMPLEMENT`, `NARROW`, `DUPLICATE`, `OBSOLETE`, or `BLOCKED` with concrete evidence. Summarize the ballot, resolve disagreements from evidence, and take the intersection of necessary scope.

If the YAGNI gate fails, do not implement. Report the canonical issue/disposition and, when authorized, update issue metadata/prose so the backlog no longer invites duplicate or obsolete work.

If the gate passes and no explicit issue-level authorization/dependency blocker remains, enter the repository loop: legitimate RED first, minimum GREEN, focused verification, adversarial diff review, full relevant gates, coherent commit, push, PR, exact-head checks, fix/retest/review until green. Merge only when repository/issue policy permits it.

Do not stop after planning when implementation is authorized and executable. Do not broaden into sibling issues. Do not use unrelated remote research as substitute context. Finish with exact SHAs, tests, voter results, local research paths, PR/merge state, and any genuine blocker.