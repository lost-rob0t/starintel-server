from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


class TargetRecoveryContractTests(unittest.TestCase):
    def test_recovery_component_loads_after_actor_registry_layer(self) -> None:
        system = (
            ROOT / "source" / "starintel-gserver.asd"
        ).read_text(encoding="utf-8")
        self.assertIn('(:file "target-recovery")', system)
        self.assertLess(
            system.index('(:file "actors-v09")'),
            system.index('(:file "target-recovery")'),
        )
        self.assertLess(
            system.index('(:file "target-recovery")'),
            system.index('(:file "rabbit")'),
        )

    def test_repository_uses_configured_database_and_canonical_view(self) -> None:
        source = (ROOT / "source" / "target-recovery.lisp").read_text(
            encoding="utf-8"
        )
        self.assertIn("query-persisted-target-documents", source)
        self.assertIn('client database "targets" "by_actor"', source)
        self.assertNotIn('"actor-targets"', source)
        self.assertIn("load-persisted-target-records", source)

    def test_typed_records_replace_overloaded_cons_submission(self) -> None:
        recovery = (ROOT / "source" / "target-recovery.lisp").read_text(
            encoding="utf-8"
        )
        rabbit = (ROOT / "source" / "rabbit.lisp").read_text(encoding="utf-8")
        self.assertIn("defstruct (target-record", recovery)
        self.assertIn("defstruct (target-command", recovery)
        self.assertIn("submit-target", recovery)
        self.assertIn("star.actors:submit-target", rabbit)
        self.assertNotIn("tell star.actors:*targets* (cons", rabbit)

    def test_recovery_is_started_after_registry_hooks(self) -> None:
        source = (ROOT / "source" / "target-recovery.lisp").read_text(
            encoding="utf-8"
        )
        hook = source.rindex("(nhooks:run-hook star:*actors-start-hook*)")
        recovery = source.rindex("(recover-persisted-targets)")
        self.assertLess(hook, recovery)
        self.assertIn("*recovered-target-fingerprints*", source)
        self.assertIn("target-active-lease-p", source)

    def test_invalid_targets_have_deterministic_quarantine(self) -> None:
        source = (ROOT / "source" / "target-recovery.lisp").read_text(
            encoding="utf-8"
        )
        self.assertIn("invalid-persisted-target", source)
        self.assertIn("invalid-target-quarantine-record", source)
        self.assertIn("quarantine:target-recovery:", source)
        self.assertIn("couchdb-save-quarantine-record", source)
        self.assertIn("http-request-conflict", source)

    def test_acceptance_matrix_runs_in_clos_gate(self) -> None:
        tests = (
            ROOT / "tests" / "run-target-recovery-conformance.lisp"
        ).read_text(encoding="utf-8")
        dockerfile = (ROOT / "Dockerfile").read_text(encoding="utf-8")
        for name in (
            "test-target-repository-uses-configured-database-and_canonical_view",
            "test-empty-target_repository-is_empty",
            "test-invalid-persisted_target_is_quarantined",
            "test_one_shot_and_recurring_targets_recover_once",
            "test-active_lease_suppresses_recovery",
        ):
            self.assertIn(name, tests)
        self.assertIn("run-target-recovery-conformance.lisp", dockerfile)


if __name__ == "__main__":
    unittest.main()
