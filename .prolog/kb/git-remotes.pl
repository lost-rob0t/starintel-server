% Git remote topology and mirror-sync knowledge (verified 2026-09-14 via
% ls-remote + prolog-verify evidence in runs/run-8533dc4*.pl).

remote_role(origin, canonical, 'ssh://forgejo@git.starintel.actor/starintel-labs/starintel-server.git').
remote_role(github, mirror, 'git@github.com:lost-rob0t/starintel-server.git').

sync_tool(git_skill_sync_remotes,
    "/home/unseen/.config/opencode/skills/git/scripts/sync-remotes [--push] [--json]",
    "Compares refs/remotes/<canonical> vs <mirror>. --push fast-forwards
ONLY ahead-canonical branches; never forces diverged branches; refuses
dirty worktree. missing-on-mirror branches are NOT auto-pushed - create
them explicitly with git push <mirror> refs/remotes/origin/<b>:refs/heads/<b>").

invariant(master_canonical_host, "Canonical master lives on Forgejo (origin).
github is a mirror; never force-push either side to 'fix' divergence.").

% 2026-09-14 final state: master REUNIFIED and green.
% - PR #12 merged github/master (47 commits) into canonical master (merge
%   commit e4ba2f6, both parents preserved, no force).
% - GH Actions (operator-designated gate; Forgejo doc-coverage CI is
%   structurally flaky, 24/26 failures incl. green-local heads) surfaced
%   three latent defects, all pre-existing on canonical 8533dc4 and never
%   CI-exercised before reunification:
%     PR #13 duplicate healthcheck key in docker-compose (validate-compose)
%     PR #14 otelcol ${VAR:-default} -> ${env:VAR:-default} (crash loop)
%     PR #15 stack-test wait: running no-healthcheck containers count healthy
% - Final master 51713d5: GH Actions all green (Docstring coverage, Smoke
%   Tests, Container Stack); local integration suite 36/36 + 25/25 + 9/9.
% Still open: 37 branches exist only on github (unmerged; agent/* experiment
% slices + feat/* WIP). 34 fully-merged mirror-only branches were deleted
% from github 2026-09-14 (operator-approved).
reconciliation_pending(mirror_only_branches(37), 'triage: recreate on Forgejo or prune from github; includes tmp/rebase debris').
branch_protected(feat/lisa_playbook_scheduler, 'operator 2026-09-14: NOT stale - active WIP, keep on mirror, do not prune').
resolved(master_divergence, '2026-09-14', 'PR #12 merge + PRs #13/#14/#15 fixes; green on GH Actions at 51713d5').
gate(gh_actions_is_effective_ci, 'Forgejo Actions doc-coverage is flaky/red-by-default; the GitHub mirror workflows are the real gate per operator directive 2026-09-14').

% Operator directive 2026-09-14: .prolog/ is TRACKED in this repo (only
% .prolog/.facts.lock ignored). Verification/KB state must ride with
% checkouts, branches, and worktrees.
policy(prolog_state_tracked, 'git-track .prolog/ (kb, runs, facts.kb, verify.pl); ignore only .prolog/.facts.lock').
