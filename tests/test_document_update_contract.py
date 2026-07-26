from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


class DocumentUpdateContractTests(unittest.TestCase):
    def test_update_service_has_structured_outcomes(self) -> None:
        source = (
            ROOT / "source" / "databases" / "document-update.lisp"
        ).read_text(encoding="utf-8")
        self.assertIn("defstruct (document-update-outcome", source)
        for status in (
            ":created",
            ":updated",
            ":duplicate",
            ":conflict-exhausted",
            ":validation-failed",
        ):
            self.assertIn(status, source)

    def test_revision_and_identity_are_persistence_controlled(self) -> None:
        source = (
            ROOT / "source" / "databases" / "document-update.lisp"
        ).read_text(encoding="utf-8")
        self.assertIn("+document-update-persistence-keys+", source)
        self.assertIn("'(\"_id\" \"_rev\")", source)
        self.assertIn("copy-json-object-excluding", source)
        self.assertIn("'(\"_rev\")", source)
        self.assertIn("patch cannot change", source)
        self.assertIn("server-private-extension-key-p", source)

    def test_merge_is_explicitly_immutable_and_retries_are_bounded(self) -> None:
        source = (
            ROOT / "source" / "databases" / "document-update.lisp"
        ).read_text(encoding="utf-8")
        self.assertIn("clone-document-update-json", source)
        self.assertIn("immutable-patch", source)
        self.assertIn("loop for attempt from 1 to max-attempts", source)
        self.assertIn("optimistic concurrency retry budget exhausted", source)
        self.assertNotIn("merge-jsown", source)

    def test_http_put_uses_canonical_update_service(self) -> None:
        source = (
            ROOT / "source" / "frontends" / "http-document-update.lisp"
        ).read_text(encoding="utf-8")
        self.assertIn('"/document/:id" :method :put', source)
        self.assertIn("couchdb-upsert-document-update", source)
        self.assertIn("document-update-outcome-json", source)

    def test_acceptance_matrix_runs_in_clos_gate(self) -> None:
        tests = (
            ROOT / "tests" / "run-document-update-conformance.lisp"
        ).read_text(encoding="utf-8")
        dockerfile = (ROOT / "Dockerfile").read_text(encoding="utf-8")
        for name in (
            "test-stale-client-revision-refetches-and-updates",
            "test-missing-document-strips_stale_revision_before_create",
            "test-patch-cannot_change_identity_or_schema",
            "test-conflict_retries_are_bounded_and_refetch_latest_revision",
            "test_revision_only_patch_is_duplicate",
            "test-server_private_extensions_are_preserved",
        ):
            self.assertIn(name, tests)
        self.assertIn("run-document-update-conformance.lisp", dockerfile)


if __name__ == "__main__":
    unittest.main()
