from __future__ import annotations

import json
import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "source"
VIEWS = SOURCE / "views"


class OperationalSalvageContractTests(unittest.TestCase):
    def text(self, relative: str) -> str:
        return (ROOT / relative).read_text(encoding="utf-8")

    def test_superseded_schema_layer_is_not_present(self) -> None:
        system = self.text("source/starintel-gserver.asd")
        forbidden = (
            "document-schema-profile",
            "document-v09",
            "document-codec",
            "couchdb-v09",
            "http-api-v09",
            "actors-v09",
        )
        for token in forbidden:
            self.assertNotIn(token, system)
        self.assertIn('(:file "document-access-package")', system)
        self.assertIn('(:file "document-access")', system)

    def test_transport_access_is_raw_json_and_schema_neutral(self) -> None:
        source = self.text("source/document-access.lisp")
        self.assertIn("parse-document-object", source)
        self.assertIn("ensure-document", source)
        self.assertIn("route dtype", source.lower())
        self.assertNotIn("closer-mop", source.lower())
        self.assertNotIn("make-instance", source.lower())
        self.assertNotIn("schema-profile", source.lower())

    def test_consumer_settlement_stays_on_owner_thread(self) -> None:
        source = self.text("source/consumers/consumers.lisp")
        policy = self.text("source/consumers/retry-policy.lisp")
        self.assertNotIn("submit-task", source)
        self.assertNotIn("receive-result", source)
        for token in (
            "consumer-process-delivery",
            "stream-settle",
            "rabbit-stream-owner-thread",
            "assert-rabbit-stream-owner",
            "wrong-stream-owner",
            "consumer-in-flight",
            "consumer-unsettled",
        ):
            self.assertIn(token, source)
        for action in (":ack", ":filtered-ack", ":retry", ":dead-letter", ":reject"):
            self.assertIn(action, source)
        self.assertRegex(
            policy,
            re.compile(
                r"defmethod\s+make-rabbit-worker-consumer\s+"
                r"\(\(consumer\s+retrying-rabbit-consumer\)",
                re.MULTILINE,
            ),
        )

    def test_retry_and_quarantine_are_bounded_and_durable(self) -> None:
        policy = self.text("source/consumers/retry-policy.lisp")
        database = self.text("source/databases/quarantine.lisp")
        rabbit = self.text("source/rabbit.lisp")
        for token in (
            "retry-delay-ms",
            "retry-action-for",
            "x-starintel-attempt",
            "x-starintel-attempt-history",
            "x-starintel-first-seen-at",
            "x-starintel-parent-trace-id",
            "x-starintel-replay-of",
        ):
            self.assertIn(token, policy)
        for token in (
            "couchdb-save-quarantine-record",
            "couchdb-list-quarantine-records",
            "replay-quarantine-record",
        ):
            self.assertIn(token, database)
        self.assertIn("inspect-quarantine", rabbit)
        self.assertIn("replay-quarantined-message", rabbit)
        self.assertNotIn("with-rabbit-recv", rabbit.lower())

    def test_outbox_separates_ingress_from_downstream_events(self) -> None:
        outbox = self.text("source/databases/outbox.lisp")
        rabbit = self.text("source/rabbit.lisp")
        for token in (
            '+outbox-extension-key+ "_server_outbox"',
            '+mutation-ledger-extension-key+ "_server_mutations"',
            "prepare-outbox-mutation",
            "mark-outbox-published",
            "recover-outbox-documents",
        ):
            self.assertIn(token, outbox)
        self.assertIn('+ingest-key+ "documents.ingest.#"', rabbit)
        self.assertIn('+update-key+ "documents.update.#"', rabbit)
        self.assertIn('+new-documents-key+ "documents.new.#"', rabbit)
        self.assertIn('+updated-documents-key+ "documents.updated.#"', rabbit)
        self.assertIn("couchdb-process-outbox-mutation", rabbit)
        self.assertIn("recover-pending-publications", rabbit)

    def test_document_update_is_revision_safe(self) -> None:
        source = self.text("source/databases/document-update.lisp")
        http = self.text("source/frontends/http-document-update.lisp")
        for status in (
            ":created",
            ":updated",
            ":duplicate",
            ":conflict-exhausted",
            ":validation-failed",
        ):
            self.assertIn(status, source)
        for token in (
            "+document-update-persistence-keys+",
            "copy-json-object-excluding",
            "server-private-extension-key-p",
            "loop for attempt from 1 to max-attempts",
        ):
            self.assertIn(token, source)
        self.assertIn('"/document/:id" :method :put', http)
        self.assertIn("couchdb-upsert-document-update", http)

    def test_target_recovery_and_dispatch_are_durable(self) -> None:
        repository = self.text("source/target-repository.lisp")
        recovery = self.text("source/target-recovery.lisp")
        dispatch = self.text("source/target-dispatch.lisp")
        acceptance = self.text("source/databases/target-acceptance.lisp")
        self.assertIn("query-persisted-target-documents", repository)
        self.assertIn("recover-persisted-targets", recovery)
        self.assertIn("target-active-lease-p", recovery)
        self.assertIn("quarantine-invalid-persisted-target", recovery)
        self.assertIn("target-dispatch-envelope", dispatch)
        self.assertIn("target-destination", dispatch)
        self.assertIn("accept-target-delivery", dispatch)
        self.assertIn("documents.target.dispatch", dispatch)
        self.assertIn("sumbit-target", dispatch)
        self.assertIn("couchdb-persist-target-acceptance", acceptance)
        self.assertIn("couchdb-update-target-acceptance", acceptance)

    def test_target_lease_semantics_design_is_complete(self) -> None:
        design = self.text("docs/target-lease-semantics.org")
        index = self.text("docs/index.org")
        for section in (
            "* Canonical lease identity and key",
            "* Lease record contract",
            "* State machine",
            "* Operation contract",
            "* Idempotency",
            "* Time, TTL, and acquisition deadlines",
            "* Fencing enforcement points",
            "* Races and failure modes",
            "* HTTP response contract",
            "* Audit contract",
            "* Split-brain and recovery assumptions",
            "* Verification matrix",
        ):
            self.assertIn(section, design)
        for field in (
            "lock_key",
            "operation_class",
            "lease_id",
            "owner_principal_id",
            "owner_client_id",
            "service_instance_id",
            "fencing_token",
            "acquired_at",
            "renewed_at",
            "expires_at",
            "ttl_ms",
            "maximum_lifetime_ms",
            "execution_id",
            "job_id",
            "trace_id",
            "request_id",
            "active",
            "expired",
            "released",
            "revoked",
        ):
            self.assertIn(field, design)
        for operation in (
            "acquire-if-free",
            "renew-if-owner",
            "release-if-owner",
            "inspect",
            "list-by-owner",
            "list-by-target",
            "list-by-program",
            "force-release",
            "revoke",
        ):
            self.assertIn(operation, design)
        self.assertIn("target-lease-semantics.org", index)

    def test_lease_protocol_is_backend_neutral(self) -> None:
        protocol = self.text("source/leases/protocol.lisp")
        memory = self.text("source/leases/memory-store.lisp")
        for operation in (
            "acquire-lease",
            "renew-lease",
            "release-lease",
            "get-lease",
            "list-leases",
            "revoke-lease",
            "backend-health",
            "close-lease-store",
        ):
            self.assertIn(f"defgeneric {operation}", protocol)
            self.assertIn(f"defmethod {operation}", memory)
        for outcome in (
            ":backend-unavailable",
            ":timeout",
            ":conflict",
            ":stale-token",
            ":not-owner",
            ":expired",
            ":outcome-unknown",
        ):
            self.assertIn(outcome, protocol)
        for relative in (
            "source/actors.lisp",
            "source/target-dispatch.lisp",
            "source/frontends/http-authorization-routes.lisp",
        ):
            source = self.text(relative).lower()
            self.assertNotIn("memory-lease-store", source)
            self.assertNotIn("valkey", source)
            self.assertNotIn("redis", source)

    def test_view_registry_targets_checked_in_design_documents(self) -> None:
        registry = self.text("source/databases/view-registry.lisp")
        designs = {
            path.stem: json.loads(path.read_text(encoding="utf-8"))
            for path in VIEWS.glob("*.json")
        }
        expected = {
            "messages-by-user": ("messages", "messages_by_user"),
            "messages-by-platform": ("messages", "messages_by_platform"),
            "messages-by-group": ("messages", "messages_by_group"),
            "by-channel": ("messages", "by_channel"),
            "count-by-dtype": ("data", "count_by_dtype"),
            "dataset-size": ("data", "dataset_size"),
            "documents-by-dataset": ("data", "by_dataset"),
            "targets-by-actor": ("targets", "by_actor"),
            "users-by-platform": ("users", "by_platform"),
            "timeline-view": ("time", "timeline"),
        }
        for wrapper, (design, view) in expected.items():
            pattern = re.compile(
                rf"register-view-spec\s+'{re.escape(wrapper)}\s+"
                rf'"{re.escape(design)}"\s+"{re.escape(view)}"',
                re.DOTALL,
            )
            self.assertRegex(registry, pattern, wrapper)
            self.assertIn(design, designs)
            self.assertEqual(designs[design]["_id"], f"_design/{design}")
            self.assertIn(view, designs[design].get("views", {}))
        for token in (
            "view-document-result",
            "view-map-result",
            "view-reduced-result",
            "reduced view requests cannot include documents",
            "group/group-level requires reduce=true",
        ):
            self.assertIn(token, registry)

    def test_all_design_documents_are_valid_json(self) -> None:
        for path in VIEWS.glob("*.json"):
            with self.subTest(path=path.name):
                document = json.loads(path.read_text(encoding="utf-8"))
                self.assertTrue(document["_id"].startswith("_design/"))
                self.assertIsInstance(document.get("views", {}), dict)


if __name__ == "__main__":
    unittest.main()
