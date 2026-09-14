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

% Debt snapshot 2026-09-14, updated after partial reconciliation:
% The 5 origin-only branches were mirrored to github (now same). master
% divergence fixed on branch reconcile/master-2026-09-14 (merge of
% github/master into origin/master lineage + ingest-path fix): merge was
% textually clean, both parents preserved, no force used. Canonical
% parent 8533dc4 was ALREADY broken (HTTP-API 33/36, see
% runs/run-1599100*.pl) - the merge added no regressions.
% Still open: 71 branches exist only on github (agent/* debris) to triage.
reconciliation_pending(mirror_only_branches(71), 'triage: recreate on Forgejo or prune from github; includes tmp/rebase debris').
resolved(master_divergence, '2026-09-14', 'merge github/master into canonical master on reconcile/master-2026-09-14; landed via Forgejo PR').

% Operator directive 2026-09-14: .prolog/ is TRACKED in this repo (only
% .prolog/.facts.lock ignored). Verification/KB state must ride with
% checkouts, branches, and worktrees.
policy(prolog_state_tracked, 'git-track .prolog/ (kb, runs, facts.kb, verify.pl); ignore only .prolog/.facts.lock').
