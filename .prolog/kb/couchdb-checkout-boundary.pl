% CouchDB checkout boundary knowledge (issue #433, verified 2026-11-12).
% AnyPool's fetch runs the pool ping AND connector allocation inside the
% pool-wide lock; putback needs the same lock. Verified against the
% qlfile.lock-pinned anypool ql-2024-10-12 snapshot (NOT the newer
% anypool-20260101 quicklisp dist copy - always check qlfile.lock for the
% version the build actually loads).

root_cause(pool_wide_lock_across_session_probe,
    anypool_ping_and_allocation_inside_pool_lock,
    "anypool fetch holds the pool lock across (funcall ping conn) and
allocate-new; the server pool ping was couchdb-client-session-valid-p
(GET /_session) and the connector ran password-auth, so one slow or
parked session probe blocked every unrelated checkout and return even
without pool exhaustion. putback also needs the lock, so returns
stalled too (#433 RED: CHECKOUT-INDEPENDENCE-WHILE-SESSION-PROBE-PARKED)").

invariant(couchdb_checkout_boundary,
    "All CouchDB client checkouts flow through
star.databases.couchdb:with-couchdb-client (defined in
source/databases/couchdb.lisp): anypool:fetch -> ensure-couchdb-session
(validate GET /_session, renew password-auth in place when expired) ->
body -> anypool:putback. Validation and re-authentication run OUTSIDE
the pool lock; pool connectors must be pure constructors (new-couchdb
does no I/O) and pool ping is (constantly t). Never reintroduce network
I/O into anypool connector/ping paths: the lock is held there.").

invariant(authsession_expiry_guarantee_221,
    "#221 lives at the checkout boundary, not the pool: every client is
validated by couchdb-client-session-valid-p before use and renewed via
reauthenticate-couchdb-client when expired; a probe that errors (timeout,
network) counts as expired. This is strictly stronger than the old
pool-ping wiring, which skipped validation for freshly allocated
clients. AnyPool has NO public discard API and its failed-ping path
never calls the disconnector, which is why renewal is in-place.").

fact(auth_pool_validation_was_illusory,
    "Before #433 the live auth-store pool (auth/store.lisp) was built by
store.lisp's own make-auth-couchdb-pool with the DEFAULT always-true
ping at asd load time; couchdb-session-hardening.lisp defined a second
make-auth-couchdb-pool that only won for LATER callers (tests). The
running credential-store pool therefore never validated sessions.
Fixed by consolidating to one definition over make-star-couchdb-pool
plus boundary validation; do not re-add a second pool constructor.").

method(hermetic_concurrency_red_for_pool_locks,
    "Deterministic contention proof pattern (no network, no sleeps):
pool with max-open>=2 and two prewarmed idle fake clients; worker A
parks inside the validation probe on the first client (signal a-in-probe
then bt:wait-on-semaphore release-a); oracle = worker B's full
checkout+return must signal b-done within (bt:wait-on-semaphore b-done
:timeout 2); cleanup releases A and joins both, then assert
pool-active-count=0 / pool-idle-count=2 for accounting. A (declare)
form inside a macro body that wraps body in progn is EVALUATED - never
put declarations in with-couchdb-client/anypool:with-connection bodies.").

fact(session_probe_timeout,
    "*couchdb-session-probe-timeout* (5s) bounds every /_session probe;
dexador :timeout errors hit the existing handler-case and count as
expired, so an unresponsive CouchDB degrades to renewal-attempt errors
on the consumer thread instead of an indefinite stall.").
