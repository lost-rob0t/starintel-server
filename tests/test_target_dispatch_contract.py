from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


class TargetDispatchContractTests(unittest.TestCase):
    def test_typed_dispatch_and_outcome_models_exist(self) -> None:
        source = (ROOT / "source" / "target-dispatch.lisp").read_text(
            encoding="utf-8"
        )
        for token in (
            "defstruct (target-destination-handle",
            "defstruct (target-dispatch-envelope",
            "defstruct (target-dispatch-outcome",
            "execution-id",
            "attempt",
            "trace-id",
            "lease-id",
            "fencing-token",
        ):
            self.assertIn(token, source)

    def test_local_and_remote_destinations_share_schedule_registration(self) -> None:
        source = (ROOT / "source" / "target-dispatch.lisp").read_text(
            encoding="utf-8"
        )
        self.assertIn("register-target-schedule", source)
        self.assertIn("schedule-once-fn", source)
        self.assertIn("schedule-recurring-fn", source)
        self.assertIn("target-destination-handle-kind", source)
        self.assertNotIn("(when (and destination", source)

    def test_canonical_route_and_compatibility_alias_are_explicit(self) -> None:
        source = (ROOT / "source" / "target-dispatch.lisp").read_text(
            encoding="utf-8"
        )
        self.assertIn('"documents.target.dispatch.~a"', source)
        self.assertIn('"actors.~a.new.target"', source)
        self.assertIn("make-remote-target-consumer", source)
        self.assertIn("canonical-target-routing-key actor-name", source)

    def test_acceptance_is_durable_before_rabbit_ack(self) -> None:
        dispatch = (ROOT / "source" / "target-dispatch.lisp").read_text(
            encoding="utf-8"
        )
        database = (
            ROOT / "source" / "databases" / "target-acceptance.lisp"
        ).read_text(encoding="utf-8")
        rabbit = (ROOT / "source" / "rabbit.lisp").read_text(encoding="utf-8")
        self.assertIn("couchdb-persist-target-acceptance", dispatch)
        self.assertIn("couchdb-update-target-acceptance", dispatch)
        self.assertIn("status\") \"pending\"", dispatch)
        self.assertIn("target-acceptance-store-conflict", database)
        self.assertIn("target-outcome-settlement", rabbit)
        self.assertNotIn("target accepted by typed actor mailbox", rabbit)
        self.assertIn("(:accepted :duplicate)", rabbit)
        self.assertIn("(:overloaded :unavailable :failed)", rabbit)

    def test_validation_rejects_bad_identity_delay_transience_and_deadlines(self) -> None:
        source = (ROOT / "source" / "target-dispatch.lisp").read_text(
            encoding="utf-8"
        )
        self.assertIn("valid-target-actor-name-p", source)
        self.assertIn("(plusp (target-record-delay record))", source)
        self.assertIn("*target-max-delay-seconds*", source)
        self.assertIn("transient targets cannot create durable schedules", source)
        self.assertIn("target deadline has expired", source)
        self.assertIn("duplicate active schedule identity", source)

    def test_acceptance_matrix_runs_in_clos_gate(self) -> None:
        tests = (
            ROOT / "tests" / "run-target-dispatch-acceptance.lisp"
        ).read_text(encoding="utf-8")
        dockerfile = (ROOT / "Dockerfile").read_text(encoding="utf-8")
        for name in (
            "test-local-and-remote-recurrence-share-one-scheduler-contract",
            "test-invalid-zero-negative-delays-and-missing-id",
            "test-duplicate-and-conflicting-schedule-identities",
            "test-durable-pending-state-resumes-after-scheduler-crash",
            "test-canonical-remote-route-is-bound-by-consumer",
            "test-overloaded-stopped-and-invalid-ingress_settlement",
        ):
            self.assertIn(name, tests)
        self.assertIn("run-target-dispatch-acceptance.lisp", dockerfile)


if __name__ == "__main__":
    unittest.main()
