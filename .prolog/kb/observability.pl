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
