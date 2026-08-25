from __future__ import annotations

import json
import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
STAR_CL_V09 = "b8dfbe2f9f56065ace8c3313b92ca748a115cdfa"


class V09RuntimeContractTests(unittest.TestCase):
    def text(self, relative: str) -> str:
        return (ROOT / relative).read_text(encoding="utf-8")

    def test_schema_and_dependency_locks_converge_exactly(self) -> None:
        schema_lock = json.loads(self.text("schema/starintel-schema.lock.json"))
        flake_lock = json.loads(self.text("flake.lock"))
        qlot_lock = self.text("qlfile.lock")

        self.assertEqual(schema_lock["schema_version"], "0.9.0")
        self.assertEqual(
            flake_lock["nodes"]["star-cl"]["locked"]["rev"], STAR_CL_V09
        )
        match = re.search(r'\("star-cl".*?github-([0-9a-f]{40})', qlot_lock, re.S)
        self.assertIsNotNone(match)
        self.assertEqual(match.group(1), STAR_CL_V09)

    def test_http_boundary_uses_schema_version_and_canonical_validator(self) -> None:
        boundary = self.text("source/frontends/http-boundary-core.lisp")
        self.assertIn('(jsown:val-safe document "schema_version")', boundary)
        self.assertIn("star.documents:validate-v09-document", boundary)
        self.assertIn('"invalid_document_schema"', boundary)
        self.assertNotIn('(jsown:val-safe document "version")', boundary)

    def test_update_boundary_validates_before_persistence_and_is_authorized(self) -> None:
        update = self.text("source/databases/document-update.lisp")
        auth = self.text("source/frontends/http-authorization.lisp")
        routes = self.text("source/frontends/http-authorization-routes.lisp")
        system = self.text("source/starintel-gserver.asd")
        self.assertIn("star.documents:validate-v09-document candidate", update)
        self.assertIn("document-update-validation-code", update)
        self.assertIn('(:put "documents:write")', auth)
        self.assertIn("authorized-update-document", routes)
        self.assertLess(
            system.index('(:file "frontends/http-boundary-core")'),
            system.index('(:file "frontends/http-document-update")'),
        )

    def test_canonical_validator_is_reused_not_forked(self) -> None:
        access = self.text("source/document-access.lisp")
        package = self.text("source/document-access-package.lisp")
        system = self.text("source/starintel-gserver.asd")
        self.assertIn("starintel::validate-v090-document", access)
        self.assertIn("starintel-doc-v0.9.0.schema.json", access)
        self.assertIn("document-schema-validation-error", package)
        self.assertIn("#:com.inuoe.jzon", system)
        self.assertNotIn("defun validate-v090-value", access)

    def test_rabbit_mutations_are_strict_and_targets_are_explicit(self) -> None:
        rabbit = self.text("source/rabbit.lisp")
        self.assertIn("(strict-schema-p t)", rabbit)
        self.assertIn("star.documents:validate-v09-document", rabbit)
        self.assertIn(":strict-schema-p nil", rabbit)
        self.assertIn(':route-dtype "target"', rabbit)
        self.assertIn("persist-rabbit-document-mutation", rabbit)

    def test_target_compatibility_is_direct_and_narrow(self) -> None:
        target = self.text("source/frontends/http-bulk-jobs.lisp")
        routes = self.text("source/frontends/http-authorization-routes.lisp")
        self.assertIn("compatibility-target-ingress-routing-key", target)
        self.assertIn("publish-target-document-unchecked", routes)
        self.assertIn(
            '"documents.new.target.~a"',
            self.text("source/target-dispatch.lisp"),
        )


if __name__ == "__main__":
    unittest.main()
