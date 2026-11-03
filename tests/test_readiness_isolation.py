"""Probe guardrails only; these tests do not certify server isolation."""
import importlib.util
import unittest
from pathlib import Path

spec = importlib.util.spec_from_file_location(
    "isolation", Path(__file__).parents[1] / "scripts/readiness_isolation.py")
probe = importlib.util.module_from_spec(spec)
spec.loader.exec_module(probe)


class IsolationProbeTests(unittest.TestCase):
    def test_rejects_external_or_credentialed_destinations(self):
        for url in ("http://example.com", "http://secret@localhost", "https://localhost",
                    "http://localhost?token=secret"):
            with self.assertRaises(ValueError):
                probe.Client(url, "secret")

    def test_redirects_are_not_followed(self):
        self.assertIsNone(probe.NoRedirect().redirect_request(None, None, None, None, None, None))

    def test_canonical_and_couch_ids_are_detected(self):
        self.assertEqual(probe.identifiers({"rows": [{"doc": {"id": "a"}},
                                                   {"doc": {"_id": "b"}}]}), {"a", "b"})

    def test_existing_nonfixture_ids_cannot_be_mutated(self):
        with self.assertRaises(ValueError):
            probe.validate_inputs([{"id": "production"}, {"id": "readiness:b"}], ["a", "b"])

    def test_success_cannot_satisfy_a_denial(self):
        class LeakyClient:
            def request(self, *args):
                return 200, {"secret": "do not log"}
        with self.assertRaisesRegex(AssertionError, "got 200") as error:
            probe.expect(LeakyClient(), "GET", "/document/b", {403})
        self.assertNotIn("do not log", str(error.exception))

    def test_scope_confound_fails_before_fixture_writes(self):
        class NarrowDatasetClient:
            def request(self, method, path, body):
                self_path = path
                assert self_path == "/auth/context"
                return 200, {"scopes": ["dataset:a", "tenant:a"]}
        with self.assertRaisesRegex(AssertionError, "credential scopes"):
            probe.run([NarrowDatasetClient()] * 2,
                      [{"id": "readiness:a", "dataset": "a"},
                       {"id": "readiness:b", "dataset": "b"}], ["a", "b"])


if __name__ == "__main__":
    unittest.main()
