"""Execute the maintained CouchDB search index on name-only documents."""
from __future__ import annotations

import json
import subprocess
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


class NameSearchIndexTests(unittest.TestCase):
    def test_name_only_documents_emit_default_terms_and_scope(self) -> None:
        source = json.loads((ROOT / "source/views/search.json").read_text())
        index = source["indexes"]["fts"]["index"]
        documents = [
            {"id": "domain:fixture", "schemaVersion": "0.10.1", "dtype": "domain",
             "dataset": "testing", "tenant_id": "public", "name": "search-needle.example"},
            {"_id": "legacy-user:fixture", "dtype": "user", "dataset": "testing",
             "tenant_id": "public", "data": {"name": "public-search-needle"}},
            {"id": "private-domain:fixture", "schemaVersion": "0.10.1", "dtype": "domain",
             "dataset": "testing", "tenant_id": "private", "name": "search-needle.example"},
        ]
        script = """
const fs = require('node:fs');
const {source, documents} = JSON.parse(fs.readFileSync(0, 'utf8'));
const execute = new Function('doc', 'index', '(' + source + ')(doc)');
process.stdout.write(JSON.stringify(documents.map(doc => {
  const terms = [];
  execute(doc, (field, value) => terms.push([field, value]));
  return terms;
})));
"""
        result = subprocess.run(
            ["node", "-e", script], input=json.dumps({"source": index, "documents": documents}),
            text=True, capture_output=True, check=True,
        )
        for document, terms in zip(documents, json.loads(result.stdout), strict=True):
            with self.subTest(id=document.get("id", document.get("_id"))):
                name = document.get("name", document.get("data", {}).get("name"))
                self.assertIn(["name", name], terms)
                self.assertIn(["default", name], terms)
                self.assertIn(["tenant_id", document["tenant_id"]], terms)
                self.assertIn(["dataset", document["dataset"]], terms)


if __name__ == "__main__":
    unittest.main()
