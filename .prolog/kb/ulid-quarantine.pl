% Concurrent quarantine collision diagnosed during Auto-Dig backlog import,
% 2026-09-16, server issue #187.

root_cause(quarantine_id_collision, unsynchronized_cms_ulid,
    "The pinned cms-ulid:ulid function keeps LAST in a shared mutable closure.
Concurrent Rabbit and HTTP workers can read and increment the same value,
issuing one quarantine:_id for different deliveries. CouchDB returns 409 for
the second create; an unhandled settlement error used to unwind through
consumer thread joining and terminate the HTTP listener.").

invariant(server_generated_ulids_use_one_lock,
    "All server production call sites, including default ID generators, use
star.ids:ulid. The helper holds one process-wide Bordeaux Threads lock while
calling the pinned cms-ulid generator.").

invariant(quarantine_ack_requires_persistence,
    "persist-and-publish-quarantine persists to CouchDB before publishing and
ACKing. A persistence conflict leaves the Rabbit delivery unacknowledged;
the owner closes its stream for redelivery without killing the listener.").

method(ambiguous_bulk_reconciliation,
    "After accepted bulk POST and failed status polling, inspect the failure
ledger and compare the exact canonical source-commit payload with CouchDB.
For run 35096900863, offsets 28259..28445 (187 documents) matched after
removing server-added tenant_id and extensions; offsets 28446..28508 (63)
were absent. Resume from verified offset 28446, never blindly replay 28259.").
