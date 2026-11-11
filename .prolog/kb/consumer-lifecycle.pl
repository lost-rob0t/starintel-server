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

% Live incident 2026-11-03/04 (issue #37 comment 6623886706): all four
% documents-ingest workers terminally stopped after CouchDB I/O timeouts
% on unsettled deliveries (settlement path). Post-#357 image; the stop is
% per the approved contract. Queue sat at 0 consumers ~23h while the
% watchlist actor published ~4.6GB; backlog reached 8,826,348.
root_cause(settlement_stop_zero_consumers_23h, no_supervised_restart_after_terminal_stop,
    "run-consumer correctly ends in :stopped for settlement-path failures
(downstream store I/O timeout mid-delivery, stop-for-redelivery), and the
#357 transport reconnect intentionally does not cover them. Nothing ever
restarts a terminally-stopped worker: runtime-ready-p honestly reports
503/unhealthy, but Docker never restarts an unhealthy container and the
HTTP plane stays up, so ingest stays at 0 consumers until a manual
container restart. A transient CouchDB stall therefore becomes a
day-scale outage with an unbounded queue backlog.").

invariant(terminal_stops_need_bounded_supervised_restart,
    "A consumer worker that ends in :stopped after a settlement/downstream
failure must be restarted by bounded supervised backoff (reusing the
stream retry policy) unless the runtime itself is stopping; the
stop-for-redelivery contract governs the DELIVERY, not the lifetime of
the worker. Until that exists, the documented recovery op is:
ssh starintel 'docker restart starintel-star-server-1' (operations.org).
Supervised restart remains gated on issue #37 operator implementation
approval; the 2026-10-30 approval covered only transport reconnect.").

invariant(quarantine_grows_during_backlog_drain,
    "When a multi-million-message documents.ingest backlog finally drains,
starintel-quarantine grows as the poison fraction dead-letters
(~19% of processed in the 2026-11-04 first window: 139,881 -> 145,362).
Quarantine has 0 consumers by design, so large drains need a follow-up
operator review/replay decision; a growing quarantine during drain is
expected, not a new consumer outage.").

method(monitor_backlog_drain,
    "After restoring consumers, sample rabbitmqctl list_queues twice and
compare messages_ready (observed net drain 1.3k-11.9k msgs/min with 4
consumers, ~800 steady unacked). Watch docker logs for 'stopped after
unsettled delivery' recurrences: a repeat of that line means the CouchDB
stall returned and the drain will silently stall again until the
supervised-restart slice ships.").

%% --- 2026-11-04 drain resilience deployment (forgejo issue #38, PR #39 bb8f61e) ---

invariant(new_observation_of_existing_document_upserts,
    "prepare-outbox-mutation applies a :new mutation for an EXISTING _id as
an :updated mutation publishing documents.updated.<dtype>; the mutation
ledger key stays derived from the incoming :new content hash so identical
redeliveries remain :duplicate acks. Explicit idempotency keys reused for
different content still raise mutation-conflict. Before this, recurring
collection (steady-v2) re-observations were permanently dead-lettered:
825k+ couch quarantine records + 500k+ rabbit starintel-quarantine
messages, with re-observation dead-lettering at up to 14/sec during
drains. Post-deploy: quarantine count frozen, _changes show rev-2+ upserts.").

invariant(quarantine_save_is_idempotent,
    "couchdb-save-quarantine-record catches dexador:http-request-conflict
and returns the existing record by _id: a duplicate quarantine of a
redelivered failure is ack-able bookkeeping. Previously a raw couch 409
escaped delivery settlement and terminally stopped workers (production
2026-11-04 10:07Z/13:13Z same-second pairs).").

invariant(supervised_worker_restart_complements_reconnect,
    "run-consumer (runtime-lifecycle.lisp) keeps the #357 contract per
delivery (settlement failure = stop-for-redelivery) but supervision
restarts the worker with the reconnect policy's capped backoff; honest
readiness (consumer-running-p nil during backoff), :stopping-fenced,
:eof exits gracefully without restart. Transport-read errors still
reconnect in-loop. failed-settlement-closes-worker-without-acknowledging
keeps its four original assertions on a finite-delivery fixture.").

fact(ingest_worker_scaling_20261104,
    "biz init sets star:*ingest-workers* 12 (was 4): at 4 workers the
steady-v2 publish rate (~310/s) exceeded consumption (~200/s, queue
+113/s); 12 workers reached ~265-300/s consumption (queue growth slowed
to ~+45/s, still not net-drain). Single-node CouchDB write throughput,
not worker count, is the next ceiling. Quarantine stayed frozen at
639444 through the scaling change; zero worker stops across the
post-fix observation windows").

%% --- Queue topology ownership (issue #385, verified by unit gate 2026-11-11) ---

invariant(queue_topology_single_declaration,
          'Exactly one open-stream method for rabbit-queue-stream (owner-fixes.lisp) declares queues through declare-queue-with-reconciliation; the shadowed duplicate in consumers.lisp and the dead rabbit-settlement.lisp second implementation were deleted. A new duplicate declaration path reintroduces the PRECONDITION_FAILED class of permanent startup failure.').
invariant(queue_topology_non_destructive_reconcile,
          'On 406 PRECONDITION_FAILED the canonical declare deletes the stale queue ONLY with :if-empty t :if-unused t, reopens the channel, and redeclares with the supported arguments; a non-empty incompatible queue signals queue-topology-mismatch and is never dropped (issue forbids volume deletion / quarantine disabling).').
invariant(queue_topology_terminal_containment,
          'queue-topology-mismatch is never startup-retryable and never reconnected: ensure-stream-open records terminal-failure-p, the worker loop exits without supervised restart, the consumer lands :stopped, and consumer-aware readiness (/health, /ready) reports 503 with an actionable drain-or-delete log. Supervised restart machinery stays gated on issue #37.').
invariant(worker_stream_topology_propagation,
          'make-rabbit-worker-consumer clones must forward queue-arguments and the dead-letter triple; a clone that drops them makes WORKERS redeclare bare queues and reintroduces the mismatch (regression: worker-stream-clones-carry-queue-topology).').
method(event_lane_dlq_topology,
       'The events queue is declared with x-dead-letter-exchange=events.dead-letter and x-dead-letter-routing-key=events.invalid; the DLX exchange, events.quarantine queue, and binding are declared in the retrying open-stream when the triple is set (start-event-consumer via make-event-consumer).').
