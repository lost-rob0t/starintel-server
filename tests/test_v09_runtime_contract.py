from __future__ import annotations

import json
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
STAR_CL_V09 = "b8dfbe2f9f56065ace8c3313b92ca748a115cdfa"


class V09RuntimeContractTests(unittest.TestCase):
    def text(self, relative: str) -> str:
        return (ROOT / relative).read_text(encoding="utf-8")

    def test_schema_and_dependency_locks_agree_on_v09(self) -> None:
        schema_lock = json.loads(self.text("schema/starintel-schema.lock.json"))
        flake_lock = json.loads(self.text("flake.lock"))
        qlot_lock = self.text("qlfile.lock")

        self.assertEqual(schema_lock["schema_version"], "0.9.0")
        self.assertEqual(flake_lock["nodes"]["star-cl"]["locked"]["rev"], STAR_CL_V09)
        self.assertIn(f'github-{STAR_CL_V09}', qlot_lock)
        self.assertNotIn("github-4065d8689ad118dc93fc95688ca1bf63973e3c0d", qlot_lock)
        self.assertNotEqual(
            flake_lock["nodes"]["star-cl"]["locked"]["rev"],
            "36c88aabcbe43a02c189e5b7704a1f063461c888",
        )

    def test_http_boundary_uses_schema_version_not_document_revision(self) -> None:
        boundary = self.text("source/frontends/http-boundary-core.lisp")
        self.assertIn('(jsown:val-safe document "schema_version")', boundary)
        self.assertIn("star.documents:validate-v09-document", boundary)
        self.assertIn('"invalid_document_schema"', boundary)
        self.assertNotIn('(jsown:val-safe document "version")', boundary)

    def test_canonical_validator_is_reused_not_forked(self) -> None:
        access = self.text("source/document-access.lisp")
        system = self.text("source/starintel-gserver.asd")
        self.assertIn("starintel::validate-v090-document", access)
        self.assertIn("starintel-doc-v0.9.0.schema.json", access)
        self.assertIn("#:com.inuoe.jzon", system)
        self.assertNotIn("defun validate-v090-value", access)

    def test_rabbit_mutations_are_strict_but_legacy_targets_are_explicit(self) -> None:
        rabbit = self.text("source/rabbit.lisp")
        self.assertIn("(strict-schema-p t)", rabbit)
        self.assertIn("star.documents:validate-v09-document", rabbit)
        self.assertIn(':strict-schema-p nil', rabbit)
        self.assertIn(':route-dtype "target"', rabbit)


if __name__ == "__main__":
    unittest.main()
