from __future__ import annotations

import json
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


class ServerV09ContractTests(unittest.TestCase):
    def test_design_documents_are_valid_json(self) -> None:
        paths = sorted((ROOT / "source" / "views").glob("*.json"))
        self.assertTrue(paths)
        for path in paths:
            with self.subTest(path=path.name):
                document = json.loads(path.read_text(encoding="utf-8"))
                self.assertTrue(document["_id"].startswith("_design/"))

    def test_nested_data_views_are_migrated(self) -> None:
        for name in ("messages.json", "orgs.json", "persons.json", "relations.json", "targets.json"):
            text = (ROOT / "source" / "views" / name).read_text(encoding="utf-8")
            with self.subTest(view=name):
                self.assertIn("doc.data", text)
                self.assertNotIn("socialmpost", text)
                self.assertNotIn("socialmediapost", text)
                self.assertNotIn('doc.dtype===\"Relation\"', text)

    def test_server_boundary_requires_v09(self) -> None:
        text = (ROOT / "source" / "document-v09.lisp").read_text(encoding="utf-8")
        for field in (
            "_id",
            "dataset",
            "dtype",
            "schema_version",
            "version",
            "date_added",
            "date_updated",
            "sources",
            "evidence",
            "data",
        ):
            self.assertIn(f'\"{field}\"', text)
        self.assertIn('string= (or (object-value document \"schema_version\") \"\") \"0.9.0\"', text)
        self.assertIn("spec:schema-org-metadata", text)

    def test_ingestion_routes_use_boundary(self) -> None:
        rabbit = (ROOT / "source" / "rabbit.lisp").read_text(encoding="utf-8")
        http = (ROOT / "source" / "frontends" / "http-api-v09.lisp").read_text(encoding="utf-8")
        self.assertIn("ensure-v09-document", rabbit)
        self.assertIn("ensure-v09-document", http)
        self.assertIn("route-dtype", http)

    def test_design_documents_are_upserted(self) -> None:
        init = (ROOT / "source" / "init.lisp").read_text(encoding="utf-8")
        self.assertIn("upsert-view-document", init)
        self.assertIn('jsown:val current \"_rev\"', init)
        self.assertIn("(init-views client database)", init)


if __name__ == "__main__":
    unittest.main()
