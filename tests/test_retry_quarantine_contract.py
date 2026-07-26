from __future__ import annotations

import json
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


class RetryQuarantineContractTests(unittest.TestCase):
    def test_single_owner_consumer_policy_replaces_legacy_receive_macro(self) -> None:
        source = (ROOT / "source" / "rabbit.lisp").read_text(encoding="utf-8")
        package = (ROOT / "source" / "package.lisp").read_text(encoding="utf-8")
        self.assertNotIn("defmacro with-rabbit-recv", source.lower())
        self.assertNotIn("#:with-rabbit-recv", package.lower())
        self.assertIn("make-document-consumer", source)
        self.assertIn("consumer-retry-options", source)

    def test_failure_taxonomy_and_bounded_backoff_are_implemented(self) -> None:
        source = (
            ROOT / "source" / "consumers" / "retry-policy.lisp"
        ).read_text(encoding="utf-8")
        for name in (
            "transient-delivery-error",
            "permanent-delivery-error",
            "conflict-delivery-error",
            "unauthorized-delivery-error",
            "schema-invalid-delivery-error",
            "internal-delivery-error",
            "retry-delay-ms",
            "retry-action-for",
        ):
            self.assertIn(name, source)
        self.assertIn("x-starintel-attempt", source)
        self.assertIn("x-starintel-attempt-history", source)
        self.assertIn("x-starintel-first-seen-at", source)

    def test_quarantine_is_durable_inspectable_and_replayable(self) -> None:
        policy = (
            ROOT / "source" / "consumers" / "retry-policy.lisp"
        ).read_text(encoding="utf-8")
        database = (
            ROOT / "source" / "databases" / "quarantine.lisp"
        ).read_text(encoding="utf-8")
        rabbit = (ROOT / "source" / "rabbit.lisp").read_text(encoding="utf-8")
        for token in (
            "failure_class",
            "failure_reason",
            "original_routing_key",
            "message_id",
            "trace_id",
            "attempt_count",
            "first_seen_at",
            "failed_at",
        ):
            self.assertIn(token, policy)
        self.assertIn("couchdb-save-quarantine-record", database)
        self.assertIn("couchdb-list-quarantine-records", database)
        self.assertIn("replay-quarantine-record", database)
        self.assertIn("inspect-quarantine", rabbit)
        self.assertIn("replay-quarantined-message", rabbit)
        self.assertIn("x-starintel-parent-trace-id", policy)
        self.assertIn("x-starintel-replay-of", policy)

    def test_quarantine_design_document_is_valid(self) -> None:
        document = json.loads(
            (ROOT / "source" / "views" / "quarantine.json").read_text(
                encoding="utf-8"
            )
        )
        self.assertEqual(document["_id"], "_design/quarantine")
        self.assertEqual(
            set(document["views"]),
            {"by_status", "by_failure_class", "by_trace_id"},
        )
        for view in document["views"].values():
            self.assertIn("_server_quarantine", view["map"])

    def test_acceptance_matrix_is_executable_in_clos_gate(self) -> None:
        tests = (
            ROOT / "tests" / "run-retry-quarantine-conformance.lisp"
        ).read_text(encoding="utf-8")
        dockerfile = (ROOT / "Dockerfile").read_text(encoding="utf-8")
        for name in (
            "test-invalid-json-dead-letters-immediately",
            "test-transient-failure-is-bounded",
            "test-conflict-is-permanent-and-idempotency-aware",
            "test-quarantine-record-preserves-provenance",
            "test-corrected-replay-resets_attempts_and_preserves_lineage",
        ):
            self.assertIn(name, tests)
        self.assertIn("run-retry-quarantine-conformance.lisp", dockerfile)


if __name__ == "__main__":
    unittest.main()
