from __future__ import annotations

import json
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


class CouchDBOutboxContractTests(unittest.TestCase):
    def test_pending_outbox_view_is_valid(self) -> None:
        view = json.loads(
            (ROOT / "source" / "views" / "outbox.json").read_text(encoding="utf-8")
        )
        self.assertEqual(view["_id"], "_design/outbox")
        source = view["views"]["pending"]["map"]
        self.assertIn("_server_outbox", source)
        self.assertIn("event.status === 'pending'", source)
        self.assertIn("[doc._id, event.sequence]", source)

    def test_outbox_state_is_embedded_in_document_extensions(self) -> None:
        source = (ROOT / "source" / "databases" / "outbox.lisp").read_text(
            encoding="utf-8"
        )
        self.assertIn('+outbox-extension-key+ "_server_outbox"', source)
        self.assertIn('+mutation-ledger-extension-key+ "_server_mutations"', source)
        self.assertIn("prepare-outbox-mutation", source)
        self.assertIn("mutation-content-hash", source)
        self.assertIn("mark-outbox-published", source)
        self.assertIn("recover-outbox-documents", source)
        self.assertIn("pending-outbox-tuples", source)
        self.assertIn('(:updated "updated")', source)
        self.assertIn('"documents.~a.~a"', source)

    def test_rabbit_ingress_and_downstream_routes_are_separate(self) -> None:
        source = (ROOT / "source" / "rabbit.lisp").read_text(encoding="utf-8")
        self.assertIn('+injest-key+ "documents.ingest.#"', source)
        self.assertIn('+update-key+ "documents.update.#"', source)
        self.assertIn("couchdb-process-outbox-mutation", source)
        self.assertIn("recover-pending-publications", source)

    def test_required_crash_recovery_cases_are_executable(self) -> None:
        tests = (ROOT / "tests" / "run-outbox-conformance.lisp").read_text(
            encoding="utf-8"
        )
        for name in (
            "test-new-publish-failure-is-retry-safe",
            "test-update-publish-failure-is-retry-safe",
            "test-crash-recovery-publishes-pending-event",
            "test-duplicate-mutation-does-not-append-outbox-entry",
            "test-conflicting-idempotency-key-is-rejected",
            "test-recovery-preserves-document-event-order",
        ):
            self.assertIn(name, tests)


if __name__ == "__main__":
    unittest.main()
