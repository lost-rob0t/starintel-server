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