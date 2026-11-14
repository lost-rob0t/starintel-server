% Durable outbox request-count optimization, Forgejo issue #427.
invariant(outbox_marker_saved_revision,
    'process-outbox-mutation passes the persisted state to the first publication-marker CAS. A conflict forces a fresh load; recovery without initial-state remains load-first.').
invariant(outbox_ingest_request_budget,
    'An uncontended new mutation requires one load, two durable saves and one publication. Saved-state reuse removes only a GET, never a durability boundary.').
method(outbox_ingest_regressions,
    't/couchdb-outbox-recovery-test.lisp covers request counts, saved revisions, intervening mutations and ledger preservation, bounded conflicts, duplicate suppression, failed saves/publication and replay with stable event IDs. Run nix run .#star-unit-tests.').
