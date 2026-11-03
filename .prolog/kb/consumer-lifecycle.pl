% RabbitMQ consumer lifecycle knowledge (verified live 2026-10-30).

% Live incident: broker restart left documents.ingest/documents.targets/
% documents.update at 0 consumers for ~25 minutes while the Lisp process
% and its HTTP API stayed up. Root cause: run-consumer returned
% permanently on any transport-class error (issue #37; GitHub
% lost-rob0t/starintel-server#37 comment 6531242249 records operator
% approval for the narrowed reconnect slice).
root_cause(broker_restart_zero_consumers, run_consumer_terminal_on_transport_error,
    "run-consumer treated every error as terminal: a broker-forced closure
raised from cl-rabbit:consume-message hit the generic error handler,
the worker thread exited to :stopped, and no code path ever reopened the
stream. The container healthcheck (curl /health -> runtime-ready-p)
would have reported honestly, but Docker never restarts an unhealthy
container, so nothing recovered it until a manual restart.").

% Fix contract for the reconnect slice (rage/37-consumer-reconnect):
% - reconnect only rabbit-startup-retryable-p transport errors;
%   settlement failures keep the stop-for-redelivery contract;
% - reconnect fenced by :stopping before and between attempts;
% - reuse the stream retry policy (retry-delay-ms, *retry-sleep-function*
%   is the test seam); no new config knobs or states;
% - reset-rabbit-stream-after-open-failure is safe to call mid-session,
%   not just at open; it clears owner-thread so reopen re-establishes it.
invariant(consumer_owner_thread_reestablished_on_reopen,
    "reset-rabbit-stream-after-open-failure nils rabbit-stream-owner-thread;
assert-rabbit-stream-owner then passes on the same thread after open-stream
re-runs. Test fakes that are not rabbit-queue-stream subclasses need
reset-consumer-stream's close-stream fallback instead.").

invariant(reconnect_classifies_by_origin_not_name,
    "name-based transport classification (rabbit-startup-retryable-p)
misclassifies settlement failures: the regression test signals
test-rabbitmq-server-error from stream-settle, and its name matches
RABBITMQ, producing an infinite reconnect loop (28-min hang, exit 124).
Only read-path errors (no delivery in hand, nothing to settle) may
trigger reconnect; wrap consumer-read in a consumer-transport-lost
condition and keep settlement errors terminal").

invariant(lisp_files_need_reader_balance_check_before_slow_gates,
    "before paying the ~12-min unit-test gate, run a local reader check:
sbcl --script with *read-suppress* t over the file catches unmatched
parens instantly (docstrings contain parens; naive counting scripts
false-positive). A paren slip otherwise surfaces only as 'compilation
unit aborted' deep inside the runner").

invariant(readiness_is_consumer_aware,
    "runtime-ready-p requires every runtime consumer :running AND
consumer-running-p; /health and /ready return 503 otherwise. Honesty
exists at the HTTP layer; recovery does not (Docker does not restart
unhealthy containers) - that is why in-process reconnect is required.").
