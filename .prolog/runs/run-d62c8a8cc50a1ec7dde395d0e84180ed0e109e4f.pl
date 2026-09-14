% Task: sync-remotes across both hosts (forgejo canonical + github mirror)
% HEAD at start: d62c8a8cc50a1ec7dde395d0e84180ed0e109e4f (master)
% See also: git skill /sync-remotes (canonical = origin @ git.starintel.actor, mirror = github)

task(sync_remotes_both_hosts).
status(done_automated_portion).

done('FF local master to origin/master: d62c8a8 -> 8533dc4 (ff-only, 37 files, observability+lease-runtime)').
done('sync-remotes --push ran: zero ahead-canonical rows, nothing auto-pushable, exit 1 as expected').
done('Pushed 5 canonical-only branches to github mirror as new refs (non-force), all now same').
done('Final report: 22 same, 71 missing-on-canonical (github-only), master diverged 11/47').
done('Evidence: 5 observations in runs/run-8533dc40943dc7230b62533458e287a58db527ce.pl; verify.pl invariants added').
done('KB: .prolog/kb/git-remotes.pl promoted (topology, sync tool, reconciliation debt)').

pending('master reconciliation: merge github/master (47 unique) into origin/master via Forgejo PR, then fast-forward mirror').
pending('triage 71 github-only branches (agent/* tmp/rebase debris vs real work)').

observation(remote_origin, 'ssh://forgejo@git.starintel.actor/starintel-labs/starintel-server.git').
observation(remote_github, 'git@github.com:lost-rob0t/starintel-server.git').
observation(branch_counts, origin=22, github=88).
observation(master_diverged, origin_master='8533dc40943d', github_master='f8caa4dfd5c6',
            counts(origin_unique=11, github_unique=47), merge_base='4721efaf8141').
observation(origin_only_branches(5),
    ['agent/star-uri-identity-compat','feat/observability-slice','feat/valkey-lease-runtime',
     'rage/5-http-contract-documents','rage/staging-youtube-actor']).
observation(github_only_branches(71), 'mirror drift; includes many agent/* tmp/rebase branches; left for manual triage').
observation(local_master, '0 ahead / 5 behind origin/master; strictly fast-forwardable').
observation(json_quirk_retracted, 'earlier "swapped JSON labels" was a misread; TSV output + ls-remote agree, script labels are correct').
