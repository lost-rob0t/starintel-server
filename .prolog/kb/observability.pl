% Observability add-on lifecycle and adapter invariants.

root_cause(hosted_observability_autoload_failure, recursive_asdf_load,
    "The hosted autoload path ran while ASDF was already testing the server.
Reloading a registered add-on system recursively re-entered ASDF. Treat an
add-on as already loaded only when its lifecycle definition is registered and
ASDF can still resolve the component; an unavailable registered system must
continue through ASDF and fail closed.").

root_cause(observability_couchdb_install_failure, unqualified_transport_special,
    "The gserver adapter referenced *couchdb-view-transport* in the STAR
package even though the transport authority is
STAR.DATABASES.COUCHDB:*COUCHDB-VIEW-TRANSPORT*. Qualify both the read and the
write so add-on startup wraps the actual database transport.").

invariant(observability_addon_unload_stops_exporter,
    "Unloading the active observability add-on must leave both the exporter
thread and exporter-running flag NIL.").

root_cause(query_audit_capture_empty, parallel_let_closure_scope,
    "CAPTURE-QUERY-AUDIT created its export transport lambda in the same LET
that introduced CAPTURED. Common Lisp evaluates LET initializers outside the
new lexical bindings, so the lambda did not close over the returned string.
Use LET* when a later initializer must capture an earlier lexical binding.").

root_cause(observability_export_tests_noop, missing_explicit_opt_in,
    "The exporter API correctly became disabled by default, but two positive
export tests only bound *EXPORTER-RUNNING*. Positive signal tests must also
bind *OBSERVABILITY-ENABLED* to true; otherwise queueing is intentionally a
no-op and drop/export assertions test nothing.").

invariant(hermetic_observability_fixture_stops_exporter,
    "A fixture that replaces the global OTLP queues must stop and join any
background exporter before resetting them, then use an explicit local
exporter-running binding for synchronous flush assertions.").
