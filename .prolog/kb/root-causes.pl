% Root causes and debugging knowledge verified by machine evidence.

:- discontiguous root_cause/3.
:- discontiguous method/2.
:- discontiguous blocker/2.

% Valkey lease concurrency flake (fixed 2026-09-11, PR #154 merged aeae5bf):
% ONE-HUNDRED-CONCURRENT-ACQUIRES-HAVE-EXACTLY-ONE-OBSERVED-OWNER returned
% :OUTCOME-UNKNOWN for 1-5 of 100 concurrent acquires under load.

root_cause(valkey_lease_outcome_unknown, eintr_reply_wait,
    "usocket:wait-for-input returns NIL not only on timeout but also when
select is interrupted (EINTR) (usocket.lisp:339 docstring; SBCL backend
swallows EINTR, backend/sbcl.lisp:790-791). GC/scheduler stop-the-world
signals of 100 contending SBCL threads interrupted select, so an already
submitted mutating EVAL was misclassified as :outcome-unknown.
Raising operation-timeout-ms 1000->2500 did NOT change the failure rate -
EINTR is timeout-independent; only a bounded retry-until-deadline fixes it.").

root_cause(valkey_lease_pool_cascade, condition_wait_no_recheck,
    "call-with-valkey-connection raised valkey-pool-timeout when
condition-wait returned NIL without re-checking the pool predicate.
A notify racing with timeout expiry was consumed, an idle connection was
stranded, and remaining waiters cascade-failed. Fix: hoist the pool
deadline once and re-check closed/idle/free-slot at every wake; total
wait stays bounded by min(pool-wait-timeout-ms, caller deadline).").

invariant(lease_atomicity_was_never_violated,
    "In every failing run the outcome histogram showed exactly 1 :acquired,
0 duplicate fencing tokens, correct winner identity via get-lease; the
atomic EVAL script decides authority server-side. The flake was purely a
client-side failure-class misclassification, never a state-machine bug.").

method(failure_class_identification,
    "The concurrency test discards non-conflict non-owner outcomes. To
identify which failure-class outcome actually fired, temporarily
instrument the test with an outcome-code histogram print (uncommitted,
on a scratch worktree), run once, then discard. Do not guess.").
%% --- Public search FTS, verified live 2026-10-31 (ingest host + local repro) ---

% Fixed by PR #358 (master 55deeee), deployed as design doc rev 76.
root_cause(fts_bare_term_zero_results, default_field_never_indexed,
    "Unfielded Lucene queries search the 'default' field. The fts index
function indexed only named fields, so every bare-term query (exactly
what the public search route compiles: '(q) AND dataset-clauses AND
tenant_id-clause') matched 0 rows while field-scoped queries matched
(verified live: title:reddit -> 140k, default:reddit -> 0 pre-fix).
Fix: index canonical text fields (content/text/body/title/summary,
data-first) into 'default' in source/views/search.json. Post-fix
default:reddit AND dataset:reddit AND tenant_id:public -> 141933.").

% Verified: hot-patched _design/search rev 73 was overwritten by pristine
% rev 74 on the next server start.
root_cause(design_doc_hotpatch_reverted, init_views_repushes_at_startup,
    "init-views (source/init.lisp) PUTs every source/views/*.json at each
server start. Editing _design/search directly on the production DB is
temporary by construction; the only durable path is changing
source/views/ in the repo and redeploying the image.").

% Open: https://github.com/lost-rob0t/starintel-server/issues/359
% RESOLVED 2026-10-31 ~24:05Z as PROD CONFIG (no source defect):
root_cause(open_public_search_empty_359, stale_allowlist_in_flattened_host_init,
    "Prod starintel-star-server-1 bind-mounts
docker/star-server-init.lisp from a divergent host checkout (live/master
4af0921, 8 ahead/132 behind, hand-merged on the host). That init
hardcodes a 74-name *public-search-datasets* allowlist without 'reddit';
biz#18 added reddit only to starintel-biz server/modules/
public-datasets.lisp, which never reaches the flattened host copy.
Both /api/v1/search and /api/v1/stats scope to the stale allowlist ->
fast empty 200s while every underlayer works (docs GET, direct fts
141933, reduce views). Fixed by adding 'reddit' to the host init +
container recreate; verified prod search total_rows=141933 and stats
total=217789. Route code at 55deeee verified correct on the local dbg
stack (3/3 rows). Structural follow-up: the ingest host star-server
deploy bypasses the biz-managed deploy tree, so allowlist merges can
silently miss prod.").
method(verify_public_search_stack,
    "Reproduce bottom-up with the server's own creds from inside the
container: (1) POST couchdb:5984/<db>/_design/search/_search/fts with
the compiled query shape, (2) the data reduce view keyed
[tenant,dataset,dtype], (3) GET /api/v1/documents/<id> through the pool.
All three passing while /api/v1/search is empty isolates the defect to
public-search-authorized-query / query-view composition.").

% Incident 2026-10-31 ~18:48Z: ingest star-server crash-looped ~4 min.
root_cause(couch_proxy_broke_server_auth, session_cookie_not_forwarded,
    "cl-couch authenticates via POST /_session and cookie sessions. A
naive urllib TCP-logging proxy between server and CouchDB drops the
session cookie; the server reads a corrupted handshake
(INVALID-VERSION), fails startup, and crash-loops. Never naive-proxy
the couch connection; use a cookie-aware forwarder or instrument in
Lisp.").

% Image env contract: verified while building the local repro stack.
image_contract(init_lisp_is_the_env_loader,
    "/etc/starintel/init.lisp baked into starintel/server:0.1.0 IS the
env loader (COUCHDB_HOST, RABBITMQ_*, STAR_AUTH_PEPPER, tenant map...).
Mounting a replacement init.lisp that omits the preamble silently drops
all env - server falls back to 127.0.0.1 couchdb and fails. Extend the
baked init, never replace it.").

% Verified 2026-10-31: dbgnet repro stack search transport death.
root_cause(dbg_stack_fts_500_hang, erlang_dist_idle_timeout_on_bridge,
    "On the local dbgnet docker bridge, the couchdb<->clouseau erlang
distribution link idles out ('Invalid distribution message ... Idle
Timeout' in couch logs) after ~hours quiet. Symptom: every
_design/search/_search POST couch-500s in ~31ms while mapreduce views
and /_up stay healthy, and clouseau IndexCleanupService removes the
index shards for dbs it thinks are gone. Repair: docker restart
dbg-clouseau, then dbg-couch, then DELETE+re-PUT _design/search to
force index recreation; verify direct fts before blaming server code.").

%% --- Target dispatch durability gap, verified live 2026-11-01 ---

% Recurring target schedules live ONLY in the in-memory wheel
% (*active-target-schedules*, source/target-dispatch.lisp); no code path
% rehydrates schedules at boot. Verified by the 2026-11-01 00:04Z
% container recreate: all 46 reddit recurring seeds silently stopped
% (actor idle, rabbit queues empty, no dispatch activity) while 218k
% collected docs + the user-hunt lane's reddit:user:* writes made the
% dataset look continuously fresh.
root_cause(recurring_collection_dies_on_restart, in_memory_wheel_no_rehydration,
    "Restarting star-server silently stops every recurring target.
Seeding is not fire-and-forget: after any restart, verify collection
via couch _changes and reseed. Compounding the invisibility: (a) the
2026-10-31 POSTed targets left no queryable docs - the couch targets
actor_count view has no reddit-actor row, so either not persisted or
persisted transient (rabbit.lisp: transient documents intentionally
not persisted); (b) GET /api/v1/targets denies the service principal
with 403 unmapped:http-route, so operators cannot even list what
should be running. Durable recurring schedules need boot rehydration
from persisted target docs + a mapped read route; issue filed.").

blocker(target_schedules_not_durable, 'https://github.com/lost-rob0t/starintel-server/issues/360').
blocker(targets_list_route_unmapped, 'https://github.com/lost-rob0t/starintel-server/issues/360').

%% --- #360 fix verified 2026-11-03 (branch issue-360-target-rehydration) ---

% Three stacked causes, all fixed in one slice:
%   1. runtime-lifecycle.lisp start-actors (the operative def, loaded last)
%      never called recover-persisted-targets; the recovery-calling defs in
%      actors.lisp/target-recovery.lisp were shadowed dead code (deleted).
%   2. process-target-dispatch-envelope early-returned on a durable
%      :duplicate acceptance without re-arming the wheel, so even wired
%      recovery could not resume recurring dispatch (one-shot duplicates
%      intentionally remain no-ops).
%   3. couchdb-persist-target-acceptance persisted only the acceptance
%      envelope; the canonical dtype=target document is now ensured
%      standalone (idempotent) so targets/by_actor indexes it.
% Plus: recovery query bounded with startkey "" (null-actor rows sort
% before every string; they would quarantine-storm boot), and
% GET /api/v1/targets mapped as targets:read (contract op targets.list).
fix(recurring_rehydration, 'runtime-lifecycle start-actors calls recover-persisted-targets at boot; failure fails boot loudly, not silently').
fix(recurring_duplicate_rearm, 'process-target-dispatch-envelope re-arms recurring schedules on :duplicate; register-target-schedule keeps in-process retries idempotent').
fix(standalone_target_document, 'couchdb-persist-target-acceptance ensures the embedded canonical target doc exists standalone for view indexing').
resolved(target_schedules_not_durable).
resolved(targets_list_route_unmapped).
