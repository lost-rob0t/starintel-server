import importlib.util
import pathlib
import sys
import unittest

SCRIPT = pathlib.Path(__file__).with_name("dedupe_relations.py")
SPEC = importlib.util.spec_from_file_location("dedupe_relations", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = MODULE
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class RelationDeduplicationTests(unittest.TestCase):
    def relation(self, document_id, *, sources=None, note=""):
        return {
            "_id": document_id,
            "_rev": f"1-{document_id}",
            "dataset": "hunter-biden",
            "dtype": "relation",
            "source": "person:robert-hunter-biden",
            "target": "email:hunter@example.test",
            "predicate": "related",
            "sources": sources or [],
            "note": note,
        }

    def test_directed_identity_is_stable(self):
        document = self.relation("legacy")
        identity = MODULE.relation_identity(document)
        self.assertEqual(
            identity.document_id(),
            MODULE.relation_identity(document).document_id(),
        )
        reversed_document = dict(
            document,
            source=document["target"],
            target=document["source"],
        )
        self.assertNotEqual(
            identity.document_id(),
            MODULE.relation_identity(reversed_document).document_id(),
        )

    def test_duplicate_group_merges_evidence_and_deletes_legacy_ids(self):
        first = self.relation("01A", sources=["actor-a"], note="first")
        second = self.relation(
            "01B",
            sources=["actor-b", "actor-a"],
            note="second",
        )
        plan = MODULE.build_plan([first, second])
        self.assertEqual(plan.duplicate_documents, 1)
        self.assertEqual(len(plan.upserts), 1)
        self.assertEqual(len(plan.deletions), 2)
        merged = plan.upserts[0]
        self.assertEqual(merged["sources"], ["actor-a", "actor-b"])
        self.assertEqual(merged["note"], "first\n\nsecond")
        self.assertEqual(merged["evidenceCount"], 2)

    def test_existing_canonical_document_is_updated_not_deleted(self):
        legacy = self.relation("01A")
        identity = MODULE.relation_identity(legacy)
        canonical = self.relation(
            identity.document_id(),
            sources=["canonical"],
        )
        plan = MODULE.build_plan([legacy, canonical])
        self.assertEqual(plan.upserts[0]["_id"], identity.document_id())
        self.assertEqual(plan.upserts[0]["_rev"], canonical["_rev"])
        self.assertEqual([row["_id"] for row in plan.deletions], ["01A"])

    def test_nested_09_relation_is_supported(self):
        document = {
            "_id": "01A",
            "_rev": "1-a",
            "schema_version": "0.9.0",
            "dataset": "d",
            "dtype": "relation",
            "sources": ["manual"],
            "data": {
                "source": "a",
                "target": "b",
                "predicate": "owns",
            },
        }
        plan = MODULE.build_plan([document])
        merged = plan.upserts[0]
        self.assertEqual(merged["data"]["source"], "a")
        self.assertIn(
            "relation_identity",
            merged["extensions"]["star_server"],
        )

    def test_invalid_relation_is_reported_not_deleted(self):
        invalid = {
            "_id": "bad",
            "_rev": "1-bad",
            "dtype": "relation",
        }
        plan = MODULE.build_plan([invalid])
        self.assertEqual(plan.invalid_documents, [invalid])
        self.assertEqual(plan.upserts, [])
        self.assertEqual(plan.deletions, [])


if __name__ == "__main__":
    unittest.main()
