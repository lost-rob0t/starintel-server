"""Behavioral tests for the offline verifier; no external services are contacted."""
import copy
import importlib.util
import io
import json
import os
from pathlib import Path
import stat
import tempfile
import unittest
from contextlib import redirect_stdout, redirect_stderr
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("starintel_replay", ROOT / "scripts/starintel-replay.py")
replay_tool = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(replay_tool)
SCOPE = {"database": "starintel", "tenant": "tenant-a", "dataset": "fixture"}


def event(sequence=1, previous=None, *, operation=None, document_id="doc-1", coverage=None,
          state=None, mutation=None, **overrides):
    value = {"protocol": replay_tool.PROTOCOL, **SCOPE, "document_id": document_id,
             "event_id": f"event-{document_id}-{sequence}",
             "mutation_id": mutation or f"mutation-{document_id}-{sequence}",
             "sequence": sequence, "previous_hash": previous["hash"] if previous else "",
             "coverage": coverage or ("continuation" if previous else "origin"),
             "operation": operation or ("updated" if previous else "new"),
             "recorded_at": "2026-09-16T23:00:00Z", "causation_id": ""}
    state = state if state is not None else {
        "_id": document_id, "dataset": SCOPE["dataset"], "tenant_id": SCOPE["tenant"],
        "dtype": "note", "data": {"value": sequence, "false": False, "null": None,
                                      "empty": [], "unicode": "λ日本語"}, "extensions": {}}
    value["state_json"] = "" if value["operation"] == "deleted" else json.dumps(state, ensure_ascii=False)
    value.update(overrides)
    value["stream_id"] = replay_tool.scalar_digest(["stream/v1", value["database"], value["tenant"],
                                                     value["dataset"], value["document_id"]])
    value["_id"] = "replay:" + replay_tool.scalar_digest(["record-id/v1", value["stream_id"],
                                                           value["mutation_id"]])
    value["hash"] = replay_tool.record_hash(value)
    return value


def heads(*events):
    return {row["stream_id"]: [row["sequence"], row["hash"]] for row in events}


class ReplayTests(unittest.TestCase):
    def test_create_update_literals_and_repeated_replay(self):
        first = event()
        second = event(2, first)
        expected = heads(second)
        one = replay_tool.replay([first, second, first, second], **SCOPE, expected_heads=expected)
        two = replay_tool.replay([first, second], **SCOPE, expected_heads=expected)
        self.assertEqual(one, two)
        self.assertTrue(one[2])
        self.assertEqual(one[0][first["stream_id"]], json.loads(second["state_json"]))
        self.assertIs(one[0][first["stream_id"]]["data"]["false"], False)
        self.assertIsNone(one[0][first["stream_id"]]["data"]["null"])
        self.assertEqual(one[0][first["stream_id"]]["data"]["empty"], [])

    def test_delete_and_recreate_keep_stream_sequence(self):
        first = event()
        deletion = event(2, first, operation="deleted")
        states, _, complete = replay_tool.replay([first, deletion], **SCOPE, expected_heads=heads(deletion))
        self.assertEqual(states, {})
        self.assertTrue(complete)
        recreated = event(3, deletion, operation="new")
        states, _, complete = replay_tool.replay([first, deletion, recreated], **SCOPE,
                                                  expected_heads=heads(recreated))
        self.assertEqual(len(states), 1)
        self.assertTrue(complete)

    def test_update_after_delete_rejected(self):
        first = event()
        deletion = event(2, first, operation="deleted")
        update = event(3, deletion, operation="updated")
        with self.assertRaisesRegex(replay_tool.ReplayError, "missing_live_state"):
            replay_tool.replay([first, deletion, update], **SCOPE)

    def test_new_over_existing_rejected(self):
        first = event()
        second = event(2, first, operation="new")
        with self.assertRaisesRegex(replay_tool.ReplayError, "new_over_existing_state"):
            replay_tool.replay([first, second], **SCOPE)

    def test_interleaved_streams_have_no_global_clock_requirement(self):
        a = event(recorded_at="later")
        b = event(document_id="doc-2", recorded_at="earlier")
        update = event(2, a, recorded_at="much-earlier")
        states, _, complete = replay_tool.replay([a, b, update], **SCOPE, expected_heads=heads(b, update))
        self.assertEqual(len(states), 2)
        self.assertTrue(complete)

    def test_checksum_mutation_fails(self):
        value = event()
        value["state_json"] += " "
        with self.assertRaisesRegex(replay_tool.ReplayError, "checksum_or_identity"):
            replay_tool.replay([value], **SCOPE)

    def test_duplicate_id_with_different_valid_hash_fails(self):
        first = event()
        changed = copy.deepcopy(first)
        changed["causation_id"] = "changed"
        changed["hash"] = replay_tool.record_hash(changed)
        with self.assertRaisesRegex(replay_tool.ReplayError, "duplicate_id_conflict"):
            replay_tool.replay([first, changed], **SCOPE)

    def test_scope_is_not_taken_from_input(self):
        for field in ("database", "tenant", "dataset"):
            with self.subTest(field=field):
                value = event(**{field: "different"})
                with self.assertRaisesRegex(replay_tool.ReplayError, "replay_scope_mismatch"):
                    replay_tool.replay([value], **SCOPE)

    def test_missing_prefix_gap_and_tail_fail(self):
        first = event()
        second = event(2, first)
        third = event(3, second)
        for rows, expected_code in (([second], "missing_origin"),
                                     ([first, third], "sequence_or_predecessor_gap"),
                                     ([first, second], "head_manifest_mismatch")):
            with self.subTest(code=expected_code):
                with self.assertRaisesRegex(replay_tool.ReplayError, expected_code):
                    replay_tool.replay(rows, **SCOPE, expected_heads=heads(third))

    def test_missing_whole_stream_fails_manifest(self):
        a, b = event(), event(document_id="doc-2")
        with self.assertRaisesRegex(replay_tool.ReplayError, "head_manifest_mismatch"):
            replay_tool.replay([a], **SCOPE, expected_heads=heads(a, b))

    def test_no_manifest_never_means_complete(self):
        self.assertFalse(replay_tool.replay([event()], **SCOPE)[2])
        self.assertFalse(replay_tool.replay([], **SCOPE)[2])
        self.assertTrue(replay_tool.replay([], **SCOPE, expected_heads={})[2])

    def test_boolean_head_sequence_is_not_an_integer(self):
        value = event()
        expected = {value["stream_id"]: [True, value["hash"]]}
        with self.assertRaisesRegex(replay_tool.ReplayError, "head_manifest_mismatch"):
            replay_tool.replay([value], **SCOPE, expected_heads=expected)

    def test_baseline_is_explicit_and_never_full_history(self):
        value = event(19, operation="updated", coverage="baseline")
        with self.assertRaisesRegex(replay_tool.ReplayError, "missing_origin"):
            replay_tool.replay([value], **SCOPE)
        states, _, complete = replay_tool.replay([value], **SCOPE, allow_baselines=True,
                                                 expected_heads=heads(value))
        self.assertEqual(len(states), 1)
        self.assertFalse(complete)

    def test_unknown_protocol_effect_and_fields_fail(self):
        for changed in ({"protocol": "future/99"}, {"operation": "dispatch"}, {"surprise": "x"}):
            with self.subTest(changed=changed):
                value = event(**changed)
                with self.assertRaises(replay_tool.ReplayError):
                    replay_tool.replay([value], **SCOPE)

    def test_state_identity_and_private_fields_fail(self):
        original = json.loads(event()["state_json"])
        for changed in ({"_id": "other"}, {"_rev": "1-foo"}, {"tenant_id": "other"},
                        {"extensions": {"_server_token": "not-a-real-secret"}}):
            with self.subTest(changed=changed):
                with self.assertRaises(replay_tool.ReplayError):
                    replay_tool.replay([event(state={**original, **changed})], **SCOPE)

    def test_strict_json_rejects_duplicates_trailing_and_nonfinite(self):
        for text in ('{"a":1,"a":2}', '{"a":1,}', 'NaN', 'Infinity', '1e9999', '{}{}'):
            with self.subTest(text=text):
                with self.assertRaises(replay_tool.ReplayError):
                    replay_tool.strict_json(text)

    def test_record_bounds_and_bad_tombstone(self):
        with self.assertRaisesRegex(replay_tool.ReplayError, "replay_limit"):
            replay_tool.replay([event(), event(document_id="doc-2")], **SCOPE, max_records=1)
        value = event(state_json="x" * (replay_tool.MAX_STATE_BYTES + 1))
        with self.assertRaisesRegex(replay_tool.ReplayError, "state_too_large"):
            replay_tool.replay([value], **SCOPE)
        first = event()
        tombstone = event(2, first, operation="deleted", state_json="{}")
        with self.assertRaisesRegex(replay_tool.ReplayError, "invalid_tombstone"):
            replay_tool.replay([first, tombstone], **SCOPE)

    def test_scalar_encoding_is_unambiguous(self):
        self.assertNotEqual(replay_tool.scalar_digest(["ab", "c"]), replay_tool.scalar_digest(["a", "bc"]))
        self.assertNotEqual(replay_tool.scalar_digest([1]), replay_tool.scalar_digest(["1"]))
        with self.assertRaises(replay_tool.ReplayError):
            replay_tool.scalar_digest([True])


class CommandTests(unittest.TestCase):
    def invoke(self, *arguments):
        stdout, stderr = io.StringIO(), io.StringIO()
        with redirect_stdout(stdout), redirect_stderr(stderr):
            result = replay_tool.main(list(arguments))
        return result, stdout.getvalue(), stderr.getvalue()

    def test_cli_requires_manifest_and_writes_only_new_private_shadow(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            value = event()
            source, manifest, output = root / "events.ndjson", root / "heads.json", root / "shadow"
            source.write_text(json.dumps(value) + "\n")
            manifest.write_text(json.dumps(heads(value)))
            args = [str(source), "--database", "starintel", "--tenant", "tenant-a", "--dataset", "fixture"]
            code, _, error = self.invoke(*args)
            self.assertEqual(code, 2)
            self.assertIn("complete_source_manifest_required", error)
            self.assertFalse(output.exists())
            code, text, _ = self.invoke(*args, "--heads", str(manifest), "--output", str(output))
            self.assertEqual(code, 0)
            report = json.loads(text)
            self.assertTrue(report["scope_complete"])
            self.assertFalse(report["server_fully_replayable"])
            self.assertEqual(report["external_effects"], 0)
            self.assertEqual(stat.S_IMODE(output.stat().st_mode), 0o700)
            for name in ("documents.ndjson", "heads.json", "report.json"):
                self.assertEqual(stat.S_IMODE((output / name).stat().st_mode), 0o600)
            before = (output / "documents.ndjson").read_bytes()
            code, _, _ = self.invoke(*args, "--heads", str(manifest), "--output", str(output))
            self.assertEqual(code, 2)
            self.assertEqual(before, (output / "documents.ndjson").read_bytes())

    def test_output_failure_does_not_leave_success_marker(self):
        with tempfile.TemporaryDirectory() as tmp:
            output = Path(tmp) / "shadow"
            with mock.patch.object(Path, "rename", side_effect=OSError("simulated")):
                with self.assertRaises(OSError):
                    replay_tool.write_shadow(output, {}, {}, {"scope_complete": True})
            self.assertFalse(output.exists())

    def test_symlink_output_is_not_followed(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            target, output = root / "target", root / "shadow"
            target.mkdir()
            output.symlink_to(target, target_is_directory=True)
            with self.assertRaises(replay_tool.ReplayError):
                replay_tool.write_shadow(output, {}, {}, {})
            self.assertEqual(list(target.iterdir()), [])

    def test_read_rejects_blank_and_duplicate_keys(self):
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp) / "events"
            for text in ('\n', '{"a":1,"a":2}\n'):
                source.write_text(text)
                with self.assertRaises(replay_tool.ReplayError):
                    list(replay_tool.read_records(source))


if __name__ == "__main__":
    unittest.main()
