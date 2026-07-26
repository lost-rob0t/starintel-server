from __future__ import annotations

import json
import re
import unittest
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parents[1]
FIXTURES = ROOT / "test" / "fixtures" / "starintel"

CANONICAL_DTYPES = {
    "actor-manifest", "address", "alert", "analysis", "asset", "breach",
    "campaign-finance", "claim", "concept", "contract", "dataset-manifest",
    "document", "domain", "education", "email", "email-message", "employment",
    "entity", "event", "evidence-record", "file", "financial-observation",
    "geo", "grant", "host", "investigation-target", "legal-case",
    "lobbying-filing", "location", "media", "meeting", "message", "network",
    "observation", "org", "ownership", "person", "phone", "policy",
    "procurement", "product", "relation", "research-pass", "social-media-post",
    "source", "target", "task", "url", "user",
}

V09_ENVELOPE = {
    "_id", "_rev", "dataset", "dtype", "schema_version", "version",
    "date_added", "date_updated", "title", "summary", "description", "status",
    "language", "tags", "labels", "aliases", "keywords", "identifiers",
    "sources", "evidence", "temporal", "provenance", "assessment",
    "verification", "handling", "lineage", "quality", "workflow",
    "geospatial", "attachments", "related_ids", "notes", "schema_org", "data",
    "extensions",
}

REQUIRED_V09 = {
    "_id", "dataset", "dtype", "schema_version", "version", "date_added",
    "date_updated", "sources", "evidence", "data",
}

REVISION = re.compile(r"^[1-9][0-9]*-[A-Za-z0-9]+$")


def load_json(path: Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def semantic_json(value: Any) -> str:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False)


class DocumentConformanceTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.contract = load_json(FIXTURES / "contract.json")
        cls.v09_documents = []
        for path in sorted((FIXTURES / "v0.9").glob("fixtures-*.json")):
            cls.v09_documents.extend(load_json(path)["fixtures"])
        cls.v08_cases = load_json(FIXTURES / "v0.8" / "index-fixtures.json")["cases"]
        cls.negative_cases = load_json(FIXTURES / "negative-cases.json")["cases"]

    def test_v09_is_the_only_write_profile(self) -> None:
        profiles = self.contract["profiles"]
        self.assertEqual(self.contract["latest_schema_version"], "0.9.0")
        self.assertEqual(profiles["0.9.0"]["mode"], "read-write")
        self.assertEqual(profiles["0.8.0"]["mode"], "read-index-only")
        self.assertEqual(self.contract["migration"]["write_version"], "0.9.0")

    def test_every_canonical_dtype_has_exactly_one_minimal_fixture(self) -> None:
        seen = [document["dtype"] for document in self.v09_documents]
        self.assertEqual(set(seen), CANONICAL_DTYPES)
        self.assertEqual(len(seen), len(CANONICAL_DTYPES))
        self.assertEqual(len(seen), len(set(seen)))

    def test_v09_envelope_and_key_names_are_strict(self) -> None:
        for document in self.v09_documents:
            with self.subTest(dtype=document["dtype"]):
                self.assertTrue(REQUIRED_V09 <= document.keys())
                self.assertFalse(set(document) - V09_ENVELOPE)
                self.assertEqual(document["schema_version"], "0.9.0")
                self.assertIsInstance(document["version"], int)
                self.assertIsInstance(document["date_added"], str)
                self.assertIsInstance(document["date_updated"], str)
                self.assertIsInstance(document["sources"], list)
                self.assertIsInstance(document["evidence"], list)
                self.assertIsInstance(document["data"], dict)
                if "_rev" in document:
                    self.assertRegex(document["_rev"], REVISION)

    def test_semantic_round_trip_is_stable(self) -> None:
        for document in self.v09_documents:
            with self.subTest(dtype=document["dtype"]):
                encoded = semantic_json(document)
                decoded = json.loads(encoded)
                self.assertEqual(semantic_json(decoded), encoded)

    def test_null_false_empty_array_empty_string_and_object_are_distinct(self) -> None:
        person = next(doc for doc in self.v09_documents if doc["dtype"] == "person")
        data = person["data"]
        self.assertIsNone(data["nullable"])
        self.assertIs(data["verified"], False)
        self.assertEqual(data["misc"], [])
        self.assertEqual(data["empty_string"], "")
        self.assertEqual(data["nested"], {"kind": "evidence", "confidence": 0.75})
        signatures = {
            semantic_json(data["nullable"]),
            semantic_json(data["verified"]),
            semantic_json(data["misc"]),
            semantic_json(data["empty_string"]),
            semantic_json(data["nested"]),
        }
        self.assertEqual(len(signatures), 5)

    def test_v08_fixture_contract_is_index_only(self) -> None:
        for case in self.v08_cases:
            with self.subTest(case=case["name"]):
                document = case["input"]
                expected = case["expected"]
                self.assertEqual(document["version"], "0.8.0")
                self.assertNotIn("data", document)
                self.assertEqual(expected["schema_version"], "0.8.0")
                self.assertEqual(expected["dtype"], document["dtype"])
                self.assertIsInstance(expected["data"], dict)

    def test_v08_false_and_empty_array_survive_index_projection(self) -> None:
        case = next(
            case for case in self.v08_cases
            if case["name"] == "message-preserves-false-and-empty-array"
        )
        data = case["expected"]["data"]
        self.assertIs(data["is_reply"], False)
        self.assertEqual(data["media"], [])
        self.assertEqual(data["mentions"], [])
        self.assertNotEqual(semantic_json(data["is_reply"]), semantic_json(data["media"]))

    def test_negative_corpus_covers_required_failures(self) -> None:
        names = {case["name"] for case in self.negative_cases}
        self.assertTrue({
            "missing-id",
            "renamed-schema-version",
            "unknown-top-level-field",
            "wrong-version-type",
            "invalid-revision",
            "legacy-write",
        } <= names)

        for case in self.negative_cases:
            document = case["document"]
            with self.subTest(case=case["name"]):
                if case["expect"] == "reject":
                    valid = (
                        REQUIRED_V09 <= document.keys()
                        and not (set(document) - V09_ENVELOPE)
                        and document.get("schema_version") == "0.9.0"
                        and isinstance(document.get("version"), int)
                        and (
                            "_rev" not in document
                            or bool(REVISION.fullmatch(document["_rev"]))
                        )
                    )
                    self.assertFalse(valid)
                else:
                    self.assertEqual(case["expect"], "read-index-only")
                    self.assertEqual(document["version"], "0.8.0")

    def test_server_uses_clos_profiles_and_mop_slot_mapping(self) -> None:
        source = (ROOT / "source" / "document-schema-profile.lisp").read_text(
            encoding="utf-8"
        )
        self.assertIn("(defclass document-schema-profile", source)
        self.assertIn("(defclass v09-schema-profile", source)
        self.assertIn("(defclass v08-schema-profile", source)
        self.assertIn("(defgeneric profile-normalize-for-index", source)
        self.assertIn("closer-mop:class-slots", source)
        self.assertIn("closer-mop:slot-definition-name", source)
        self.assertIn("starintel-v08-index-adapter", source)

    def test_write_boundary_rejects_legacy_profile(self) -> None:
        source = (ROOT / "source" / "document-v09.lisp").read_text(
            encoding="utf-8"
        )
        self.assertIn("writable-schema-profile-for-document", source)
        self.assertIn("canonical v0.9 envelope at a write boundary", source)


if __name__ == "__main__":
    unittest.main()
