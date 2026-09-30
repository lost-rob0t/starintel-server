% StarIntel 0.10.1 ingress authority, 2026-09-30.

invariant(star_lang_is_document_authority,
    "schema/starintel-schema.lock.json pins nsaspy/star-lang commit
919833266723edc9bddb337d606f72ae25fb8ced for release/schema 0.10.1.").

invariant(canonical_document_keys_are_lower_camel_case,
    "The server publishes only star-cl-migrated and validated 0.10.1 documents.
Snake-case aliases are accepted only at the explicit legacy input boundary.").

invariant(bulk_migration_is_failure_isolated,
    "Each source document is migrated independently. Invalid inputs produce a
quarantine result while subsequent valid inputs continue; authorization runs
over all canonical documents that will be published before side effects.").

method(verify_star_lang_ingress,
    "Verify the server lock against sibling star-lang and star-cl checkouts,
build starintel-gserver with the local star-cl flake override, then run the
canonical-document-batch smoke proof with ASDF output redirected outside the
Nix store.").
