% Async bulk job payload lifetime and operator reconciliation, issue #189.

root_cause(terminal_bulk_payload_retention, process_local_job_registry,
    "The process-wide bulk job hash table retains completed jobs. Before issue
#189, each terminal job retained its full documents list and request-only
service context; polling recomputed total from the retained documents.").

invariant(terminal_bulk_jobs_are_pollable_without_request_payload,
    "An asynchronous bulk job retains job ID, submitter, correlation ID,
original total, status, success count, failure count, and error code after
completion. Its documents and service context are released on terminal paths.").

method(backlog_bulk_ambiguity_audit,
    "Compare exact canonical source-commit document IDs and payloads against
CouchDB after an accepted bulk POST loses status polling. Run 35100690881
confirmed through offset 44500; direct audit found offsets 29696..44499
present and 44500..44749 absent, with no present documents after the first
missing offset. One older matching document at offset 38409 lacks tenant_id;
the canonical payload fields match. Resume from 44500 after server repair.").
