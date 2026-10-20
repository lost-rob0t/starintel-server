% Root causes and debugging knowledge verified by machine evidence.

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

% Heap leaks found 2026-10-20 by tools/probe-findings.lisp probes
% (cl-alloc-tools with-heap-growth; branch cl-alloc-tooling @ 47da0e4).

root_cause(bulk_jobs_registry_growth, no_terminal_eviction,
    "*bulk-ingest-jobs* only grows: submit inserts at http-bulk-jobs.lisp
     :257 and the sole remhash (:273) is the enqueue-error rollback.
     execute-bulk-job (:184-185) drops documents/service-context, so the
     retained bytes per terminal job are small (struct + id strings) but
     strictly unbounded under sustained async bulk ingest. The pending
     caps (32 global / 4 per principal) rate-limit submission, they do
     not evict completed entries. Evidence: 2200 terminal jobs ->
     registry 0->2200, +136,864 bytes / 1222 BULK-INGEST-JOB structs.
     Filed as Forgejo issue 23."). 

root_cause(target_schedule_registry_growth, no_removal_path,
    "*active-target-schedules* (target-dispatch.lisp :468) has NO removal
     path anywhere in the file - no remhash, no cancel-eviction, no
     expiry sweep. One-shot schedules fire exactly once; their envelopes
     (each holding a full target record) are dead but retained forever.
     Recurring schedules are bounded by schedule identity, one-shots are
     the unbounded path. Evidence: 3000 registered+fired one-shot
     schedules -> registry 0->3000. Filed as Forgejo issue 24.").

method(submit_bulk_job_probe_stubs,
    "Probing submit-bulk-ingest-job directly needs three stubs matching
     the real async shape: (1) ensure-workers-fn must populate
     *bulk-ingest-workers* or the round-robin (mod index 0) signals
     DIVISION-BY-ZERO; (2) tell-fn runs INSIDE *bulk-ingest-lock*, so a
     synchronous worker stub re-enters the lock via
     release-bulk-job-slot (recursive lock error, surfaces as 503
     bulk_enqueue_failed) - capture told jobs and drain them after
     submit returns; (3) drain in chunks <= 4 to respect the per-
     principal pending cap, else 429 principal_bulk_quota_exceeded.").