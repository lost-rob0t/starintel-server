import copy
import json
import os
from pathlib import Path
import stat
import subprocess
import tempfile
import unittest

from starintel_audit import (AuditError, CouchAudit, checkpoint, design_document,
                             document_id, event, validate)


def sample(**change):
    arguments = dict(event_id="evt-1", event_type="billing.payment.succeeded", tenant_id="system",
                     source="stripe:acct_test:test", occurred_at="2026-09-16T20:00:00.000000Z",
                     details={"amount_minor": 2000, "currency": "usd"})
    arguments.update(change)
    return event(**arguments)


class MemoryAudit(CouchAudit):
    def __init__(self):
        super().__init__("http://127.0.0.1:5984/audit_test", "system", "writer", "not-a-real-secret")
        self.docs, self.calls = {}, []

    def request(self, method, path="", **kwargs):
        self.calls.append((method, path, kwargs))
        if path == "/_changes":
            return {"results": [], "last_seq": "8-gopaque-token"}
        if method == "PUT":
            if path in self.docs:
                raise AuditError(409)
            self.docs[path] = copy.deepcopy(kwargs["data"])
            return {"ok": True}
        return dict(self.docs[path], _rev="1-example")


class AuditTests(unittest.TestCase):
    def test_stable_identity_and_tenant_separation(self):
        self.assertEqual(sample()["_id"], sample()["_id"])
        self.assertNotEqual(sample()["_id"], sample(tenant_id="another")["_id"])
        self.assertNotEqual(document_id("a", "b:c", "d"), document_id("a:b", "c", "d"))

    def test_detached_snapshot(self):
        details = {"amount_minor": 123, "currency": "jpy"}
        doc = sample(details=details)
        details["amount_minor"] = 999
        self.assertEqual(doc["details"]["amount_minor"], 123)

    def test_unknown_secret_field_rejected_before_sink(self):
        audit = MemoryAudit()
        for key in ("authorization", "password", "token", "raw_request", "customer_email"):
            with self.subTest(key=key):
                doc = sample()
                doc[key] = "secret-canary"
                with self.assertRaises(ValueError):
                    audit.append(doc)
        self.assertEqual(audit.calls, [])

    def test_unknown_detail_rejected(self):
        with self.assertRaises(ValueError):
            sample(details={"card_number": "secret-canary"})

    def test_invalid_identifiers(self):
        for identifier in ("", "../secret?", "hello\nworld", "@everyone", "x" * 161, None):
            with self.subTest(identifier=identifier), self.assertRaises(ValueError):
                sample(event_id=identifier)

    def test_minor_units_exact_integer(self):
        for amount in (True, -1, 1.5, 9007199254740992):
            with self.subTest(amount=amount), self.assertRaises(ValueError):
                sample(details={"amount_minor": amount, "currency": "usd"})

    def test_utc_and_valid_calendar_date_required(self):
        for stamp in ("2026-09-16", "2026-02-30T00:00:00.000000Z", "2026-09-16T20:00:00.000000+01:00"):
            with self.subTest(stamp=stamp), self.assertRaises(ValueError):
                sample(occurred_at=stamp)

    def test_idempotent_append(self):
        audit = MemoryAudit()
        doc = sample()
        self.assertEqual(audit.append(doc), audit.append(doc))
        self.assertEqual(len(audit.docs), 1)

    def test_reused_id_with_different_data_rejected(self):
        audit = MemoryAudit()
        audit.append(sample())
        with self.assertRaisesRegex(ValueError, "reused"):
            audit.append(sample(details={"amount_minor": 3000, "currency": "usd"}))

    def test_tenant_check_precedes_transport(self):
        audit = MemoryAudit()
        with self.assertRaises(ValueError):
            audit.append(sample(tenant_id="other"))
        self.assertEqual(audit.calls, [])

    def test_mutated_identity_rejected(self):
        doc = sample()
        doc["_id"] = "audit-" + "0" * 64
        with self.assertRaises(ValueError):
            validate(doc)

    def test_credentials_and_remote_http_not_allowed_in_url(self):
        for url in ("http://user:pass@127.0.0.1/audit", "http://example.org/audit",
                    "https://example.org/audit?token=secret", "https://example.org/audit/path"):
            with self.subTest(url=url), self.assertRaises(ValueError):
                CouchAudit(url, "system", "user", "pass")

    def test_transport_errors_do_not_disclose_credentials(self):
        self.assertEqual(str(AuditError(401)), "audit transport failed (status 401)")

    def test_show_refuses_design_document_and_path_escape(self):
        for identifier in ("_design/audit", "../_users", "audit-fake"):
            with self.subTest(identifier=identifier), self.assertRaises(ValueError):
                MemoryAudit().get(identifier)

    def test_changes_cursor_is_opaque(self):
        audit = MemoryAudit()
        cursor = "7-g1AAAopaque+/=token"
        audit.changes(cursor)
        self.assertEqual(audit.calls[-1][2]["params"]["since"], cursor)

    def test_checkpoint_is_private_and_preserves_cursor(self):
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "cursor.json"
            state = {"since": "7-gopaque", "tenant": "system"}
            checkpoint(path, state)
            self.assertEqual(json.loads(path.read_text()), state)
            self.assertEqual(stat.S_IMODE(path.stat().st_mode), 0o600)
            checkpoint(path, dict(state, since="8-other"))
            self.assertEqual(json.loads(path.read_text())["since"], "8-other")

    def test_couchdb_validator_in_javascript(self):
        doc = sample()
        checks = [dict(doc=doc, old=None, roles=["audit-writer:system"], ok=True),
                  dict(doc=dict(doc, _rev="1-new"), old=None, roles=["audit-writer:system"], ok=True),
                  dict(doc=doc, old=doc, roles=["audit-writer:system"], ok=False),
                  dict(doc=dict(doc, _deleted=True), old=None, roles=["audit-writer:system"], ok=False),
                  dict(doc=doc, old=None, roles=["audit-reader:system"], ok=False),
                  dict(doc=dict(doc, tenant_id="other"), old=None, roles=["audit-writer:system"], ok=False),
                  dict(doc=dict(doc, event_id="evt-1\n"), old=None, roles=["audit-writer:system"], ok=False),
                  dict(doc=dict(doc, occurred_at="2026-02-30T00:00:00.000000Z"), old=None, roles=["audit-writer:system"], ok=False),
                  dict(doc=dict(doc, details={"status":"paid\n"}), old=None, roles=["audit-writer:system"], ok=False),
                  dict(doc=dict(doc, password="secret"), old=None, roles=["audit-writer:system"], ok=False),
                  dict(doc=dict(doc, details={"password": "secret"}), old=None, roles=["audit-writer:system"], ok=False)]
        code = "const validate=(" + design_document("system", "audit-writer:system")["validate_doc_update"] + ");\n"
        code += "const checks=" + json.dumps(checks) + ";\n"
        code += "for(const c of checks){let ok=true;try{validate(c.doc,c.old,{roles:c.roles},{});}catch(e){ok=false;}if(ok!==c.ok)process.exit(1);}\n"
        subprocess.run(["node", "-e", code], check=True, timeout=10)


if __name__ == "__main__":
    unittest.main()
