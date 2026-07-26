from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


class RabbitOwnerContractTests(unittest.TestCase):
    def test_consumer_does_not_dispatch_handlers_to_lparallel(self) -> None:
        source = (ROOT / "source" / "consumers" / "consumers.lisp").read_text(
            encoding="utf-8"
        )
        self.assertNotIn("submit-task", source)
        self.assertNotIn("receive-result", source)
        self.assertIn("consumer-process-delivery", source)
        self.assertIn("stream-settle", source)

    def test_structured_settlement_actions_exist(self) -> None:
        source = (ROOT / "source" / "consumers" / "consumers.lisp").read_text(
            encoding="utf-8"
        )
        for action in (":ack", ":filtered-ack", ":retry", ":dead-letter", ":reject"):
            self.assertIn(action, source)
        self.assertIn("consumer-settlement", source)
        self.assertIn("consumer-in-flight", source)
        self.assertIn("consumer-unsettled", source)

    def test_rabbit_stream_has_an_explicit_owner(self) -> None:
        source = (ROOT / "source" / "consumers" / "consumers.lisp").read_text(
            encoding="utf-8"
        )
        self.assertIn("rabbit-stream-owner-thread", source)
        self.assertIn("assert-rabbit-stream-owner", source)
        self.assertIn("wrong-stream-owner", source)
        self.assertIn("make-rabbit-worker-consumer", source)
        self.assertIn("fresh stream/connection owner", source)

    def test_handlers_return_results_instead_of_settling_channels(self) -> None:
        rabbit = (ROOT / "source" / "rabbit.lisp").read_text(encoding="utf-8")
        event_actor = (
            ROOT / "source" / "actor-systems" / "event-actor.lisp"
        ).read_text(encoding="utf-8")
        self.assertIn("settlement-ack", rabbit)
        self.assertIn("settlement-ack", event_actor)
        handler_region = rabbit[rabbit.index("(defun process-rabbit-document-mutation") :]
        self.assertNotIn("basic-ack", handler_region)
        self.assertNotIn("basic-nack", handler_region)

    def test_filtered_messages_default_to_filtered_ack(self) -> None:
        source = (ROOT / "source" / "consumers" / "consumers.lisp").read_text(
            encoding="utf-8"
        )
        self.assertIn(":initform :filtered-ack", source)
        self.assertIn("configured-filter-settlement", source)
        self.assertIn("consumer filter declined delivery", source)

    def test_acceptance_matrix_is_executable(self) -> None:
        tests = (
            ROOT / "tests" / "run-consumer-owner-conformance.lisp"
        ).read_text(encoding="utf-8")
        for name in (
            "test-handler-and-settlement-use-owner-thread",
            "test-filtered-delivery-restores-prefetch-credit",
            "test-handler-failure-settles-exactly-once",
            "test-wrong-thread-settlement-is-rejected",
            "test-concurrent-workers-do-not-share-stream-or-channel",
        ):
            self.assertIn(name, tests)


if __name__ == "__main__":
    unittest.main()
