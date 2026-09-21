"""Bounded, append-only operational audit documents; no billing authority."""
from __future__ import annotations

import argparse
import base64
import hashlib
import json
import os
import re
import sys
import tempfile
import time
from datetime import datetime, timezone
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import quote, urlencode, urlsplit
from urllib.request import HTTPRedirectHandler, Request, build_opener

SCHEMA = "starintel.audit.v1"
MAX_BYTES = 16384
TOKEN = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._:/-]{0,159}$")
FIELDS = {"_id", "schema", "event_id", "occurred_at", "event_type", "tenant_id",
          "source", "action", "principal_id", "resource_id", "correlation_id", "outcome", "details"}
DETAILS = {"amount_minor", "currency", "status", "plan_id", "previous_plan_id",
           "guild_id", "channel_id", "operator_role_id", "categories", "enabled"}
OUTCOMES = {"success", "failure", "pending", "allow", "deny", "info"}


def token(value: str) -> str:
    if not isinstance(value, str) or not TOKEN.fullmatch(value):
        raise ValueError("invalid audit identifier")
    return value


def document_id(tenant: str, source: str, event_id: str) -> str:
    identity = "\n".join(map(token, (tenant, source, event_id))).encode()
    return "audit-" + hashlib.sha256(identity).hexdigest()


def event(*, event_id: str, event_type: str, tenant_id: str, source: str,
          occurred_at: str, action: str | None = None, principal_id: str = "system", resource_id: str = "none",
          correlation_id: str = "none", outcome: str = "info", details: dict | None = None) -> dict:
    doc = dict(schema=SCHEMA, event_id=event_id, event_type=event_type,
               tenant_id=tenant_id, source=source, occurred_at=occurred_at,
               action=action or event_type, principal_id=principal_id, resource_id=resource_id,
               correlation_id=correlation_id, outcome=outcome, details=details or {})
    doc["_id"] = document_id(tenant_id, source, event_id)
    return validate(doc)


def validate(doc: dict, tenant: str | None = None) -> dict:
    if not isinstance(doc, dict) or set(doc) != FIELDS or doc.get("schema") != SCHEMA:
        raise ValueError("invalid audit envelope or forbidden field")
    for key in ("event_id", "event_type", "tenant_id", "source", "principal_id",
                "resource_id", "correlation_id", "action"):
        token(doc[key])
    if tenant is not None and doc["tenant_id"] != tenant:
        raise ValueError("audit tenant mismatch")
    if doc["_id"] != document_id(doc["tenant_id"], doc["source"], doc["event_id"]):
        raise ValueError("audit identity mismatch")
    if doc["outcome"] not in OUTCOMES:
        raise ValueError("invalid audit outcome")
    stamp = doc["occurred_at"]
    if not isinstance(stamp, str) or not re.fullmatch(r"\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d\.\d{6}Z", stamp):
        raise ValueError("timestamp must be UTC with six fractional digits")
    datetime.fromisoformat(stamp.replace("Z", "+00:00"))
    details = doc["details"]
    if not isinstance(details, dict) or set(details) - DETAILS:
        raise ValueError("forbidden audit detail")
    if ("amount_minor" in details) != ("currency" in details):
        raise ValueError("amount and currency must occur together")
    for key, value in details.items():
        if key == "amount_minor":
            if type(value) is not int or not 0 <= value <= 9007199254740991:
                raise ValueError("invalid minor-unit amount")
        elif key == "enabled":
            if type(value) is not bool:
                raise ValueError("invalid enabled flag")
        elif key == "currency":
            if not isinstance(value, str) or not re.fullmatch(r"[a-z]{3}", value):
                raise ValueError("invalid currency")
        else:
            token(value)
    encoded = json.dumps(doc, ensure_ascii=True, sort_keys=True, separators=(",", ":"))
    if len(encoded.encode()) > MAX_BYTES:
        raise ValueError("audit event too large")
    return json.loads(encoded)  # Return a detached snapshot, not the caller's mutable dictionary.


def now() -> str:
    return datetime.now(timezone.utc).isoformat(timespec="microseconds").replace("+00:00", "Z")


class AuditError(RuntimeError):
    def __init__(self, status: int):
        self.status = status
        super().__init__(f"audit transport failed (status {status})")


class NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        raise AuditError(code)


class CouchAudit:
    def __init__(self, database_url: str, tenant: str, username: str, password: str,
                 *, allow_insecure: bool = False):
        parsed = urlsplit(database_url)
        if (parsed.scheme not in {"https", "http"} or not parsed.hostname or
                parsed.username or parsed.password or parsed.query or parsed.fragment or
                not re.fullmatch(r"/[a-z][a-z0-9_$()+-]*", parsed.path)):
            raise ValueError("expected a credential-free URL to one audit database")
        if parsed.scheme == "http" and parsed.hostname not in {"localhost", "127.0.0.1", "::1"} and not allow_insecure:
            raise ValueError("audit transport requires TLS or a loopback tunnel")
        if not username or not password or ":" in username:
            raise ValueError("audit credentials are required")
        self.url, self.tenant = database_url, token(tenant)
        self.authorization = "Basic " + base64.b64encode(f"{username}:{password}".encode()).decode()
        self.opener = build_opener(NoRedirect())

    def request(self, method: str, path: str = "", *, data=None, params=None, timeout=10):
        url = self.url + path + ("?" + urlencode(params) if params else "")
        body = None if data is None else json.dumps(data, separators=(",", ":")).encode()
        req = Request(url, data=body, method=method,
                      headers={"Authorization": self.authorization, "Content-Type": "application/json"})
        try:
            with self.opener.open(req, timeout=timeout) as response:
                if response.status not in {200, 201}:
                    raise AuditError(response.status)
                raw = response.read(2 * 1024 * 1024 + 1)
                if len(raw) > 2 * 1024 * 1024:
                    raise AuditError(413)
                return json.loads(raw)
        except HTTPError as exc:
            raise AuditError(exc.code) from None
        except (URLError, TimeoutError, ValueError, OSError):
            raise AuditError(0) from None  # Never expose URLs, credentials or response bodies.

    def append(self, doc: dict) -> str:
        clean = validate(doc, self.tenant)
        path = "/" + quote(clean["_id"], safe="")
        try:
            self.request("PUT", path, data=clean)
        except AuditError as exc:
            if exc.status != 409:
                raise
            existing = self.get(clean["_id"])
            if existing != clean:
                raise ValueError("audit event ID reused with different content") from None
        return clean["_id"]

    def get(self, event_id: str) -> dict:
        if not re.fullmatch(r"audit-[0-9a-f]{64}", event_id):
            raise ValueError("invalid audit document ID")
        doc = self.request("GET", "/" + event_id)
        doc.pop("_rev", None)
        return validate(doc, self.tenant)

    def history(self, limit=50, event_type=None, before=None):
        if not 1 <= limit <= 1000:
            raise ValueError("limit must be 1..1000")
        view = "by_type_time" if event_type else "by_time"
        prefix = [token(event_type)] if event_type else []
        params = {"descending": "true", "include_docs": "true", "limit": limit,
                  "startkey": json.dumps(prefix + ([before, {}] if before else [{}])),
                  "endkey": json.dumps(prefix)}
        result = self.request("GET", f"/_design/audit/_view/{view}", params=params)
        for row in result["rows"]:
            doc = row["doc"]
            doc.pop("_rev", None)
            yield validate(doc, self.tenant)

    def changes(self, since="0", *, follow=True):
        return self.request("GET", "/_changes", timeout=35,
                            params={"feed": "longpoll" if follow else "normal", "since": since,
                                    "include_docs": "true", "limit": 100, "timeout": 25000})

    def initialize(self):
        """Explicit administrator operation; never grant credentials from the bot."""
        created = False
        try:
            self.request("PUT")
            created = True
        except AuditError as exc:
            if exc.status != 412:
                raise
        reader, writer = f"audit-reader:{self.tenant}", f"audit-writer:{self.tenant}"
        security = {"admins": {"names": [], "roles": []},
                    "members": {"names": [], "roles": [reader, writer]}}
        current = self.request("GET", "/_security")
        if not created and current and current != security:
            raise ValueError("existing database security differs; refusing to overwrite")
        self.request("PUT", "/_security", data=security)
        design = design_document(self.tenant, writer)
        try:
            self.request("PUT", "/_design/audit", data=design)
        except AuditError as exc:
            if exc.status != 409:
                raise
            old = self.request("GET", "/_design/audit")
            old.pop("_rev", None)
            if old != design:
                raise ValueError("existing audit design differs; explicit migration required") from None


def design_document(tenant: str, writer: str) -> dict:
    """Server-side defense too: bypassing this Python client must not allow edits."""
    validator = r'''function(n,o,u,s) {
function deny(){throw({forbidden:"invalid or immutable audit document"});}
if(o || n._deleted) deny();
if(u.roles.indexOf(WRITER)<0) deny();
var keys=FIELDS, k;
for(k in n) if(typeof n[k]==="string" && /[\x00-\x1f\x7f]/.test(n[k])) deny();
for(k in n) if(keys.indexOf(k)<0 && k!=="_rev") deny();
for(var i=0;i<keys.length;i++) if(!(keys[i] in n)) deny();
if(n.schema!==SCHEMA || n.tenant_id!==TENANT) deny();
if(!/^audit-[0-9a-f]{64}$/.test(n._id)) deny();
var ids=["event_id","event_type","tenant_id","source","principal_id","resource_id","correlation_id","action"];
for(i=0;i<ids.length;i++) if(typeof n[ids[i]]!=="string" || !/^[A-Za-z0-9][A-Za-z0-9._:/-]{0,159}$/.test(n[ids[i]])) deny();
if(OUTCOMES.indexOf(n.outcome)<0) deny();
if(typeof n.occurred_at!=="string" || !/^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d\.\d{6}Z$/.test(n.occurred_at) || isNaN(Date.parse(n.occurred_at))) deny();
if(new Date(n.occurred_at).toISOString().slice(0,19)!==n.occurred_at.slice(0,19) || n.occurred_at.slice(0,4)==="0000") deny();
if(!n.details || typeof n.details!=="object" || Array.isArray(n.details)) deny();
if(("amount_minor" in n.details)!==("currency" in n.details)) deny();
for(k in n.details){
if(DETAILS.indexOf(k)<0) deny(); var v=n.details[k];
if(typeof v==="string" && /[\x00-\x1f\x7f]/.test(v)) deny();
if(k==="amount_minor"){if(typeof v!=="number" || v<0 || v>9007199254740991 || Math.floor(v)!==v) deny();}
else if(k==="enabled"){if(typeof v!=="boolean") deny();}
else if(k==="currency"){if(typeof v!=="string" || !/^[a-z]{3}$/.test(v)) deny();}
else if(typeof v!=="string" || !/^[A-Za-z0-9][A-Za-z0-9._:/-]{0,159}$/.test(v)) deny();
}
if(JSON.stringify(n).length>16384) deny();
}'''
    for name, value in {"WRITER": writer, "FIELDS": sorted(FIELDS), "SCHEMA": SCHEMA,
                        "TENANT": token(tenant), "OUTCOMES": sorted(OUTCOMES), "DETAILS": sorted(DETAILS)}.items():
        validator = re.sub(r"\b" + name + r"\b", lambda _: json.dumps(value), validator)
    return {"_id": "_design/audit", "language": "javascript", "validate_doc_update": validator,
            "views": {"by_time": {"map": 'function(d){if(d.schema==="starintel.audit.v1")emit([d.occurred_at,d._id],null);}'},
                      "by_type_time": {"map": 'function(d){if(d.schema==="starintel.audit.v1")emit([d.event_type,d.occurred_at,d._id],null);}'}}}


def from_environment() -> CouchAudit:
    return CouchAudit(os.environ["STARINTEL_AUDIT_URL"], os.environ["STARINTEL_AUDIT_TENANT"],
                      os.environ["STARINTEL_AUDIT_USER"],
                      Path(os.environ["STARINTEL_AUDIT_PASSWORD_FILE"]).read_text().strip(),
                      allow_insecure=os.environ.get("STARINTEL_AUDIT_ALLOW_INSECURE") == "1")


def checkpoint(path: Path, state: dict):
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, temporary = tempfile.mkstemp(dir=path.parent)
    try:
        with os.fdopen(fd, "w") as stream:
            json.dump(state, stream)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)


def main(argv=None):
    parser = argparse.ArgumentParser(prog="starintel-audit")
    sub = parser.add_subparsers(dest="command", required=True)
    sub.add_parser("init")
    sub.add_parser("append", help="append one validated JSON event from stdin")
    show = sub.add_parser("show")
    show.add_argument("id")
    history = sub.add_parser("list")
    history.add_argument("--limit", type=int, default=50)
    history.add_argument("--type", dest="event_type")
    history.add_argument("--before")
    tail = sub.add_parser("tail")
    tail.add_argument("--since", default="0")
    tail.add_argument("--cursor", type=Path, required=True)
    tail.add_argument("--type", dest="event_type")
    args = parser.parse_args(argv)
    try:
        audit = from_environment()
        if args.command == "init":
            audit.initialize()
        elif args.command == "append":
            raw = sys.stdin.buffer.read(MAX_BYTES + 1)
            if len(raw) > MAX_BYTES:
                raise ValueError("audit event too large")
            print(audit.append(json.loads(raw)))
        elif args.command == "show":
            print(json.dumps(audit.get(args.id), sort_keys=True))
        elif args.command == "list":
            for doc in audit.history(args.limit, args.event_type, args.before):
                print(json.dumps(doc, sort_keys=True))
        else:
            state = {"database": audit.url, "tenant": audit.tenant, "since": args.since}
            if args.cursor.exists():
                saved = json.loads(args.cursor.read_text())
                if saved.get("database") != audit.url or saved.get("tenant") != audit.tenant:
                    raise ValueError("cursor belongs to a different audit source")
                state = saved
            while True:
                for attempt in range(6):
                    try:
                        batch = audit.changes(state["since"])
                        break
                    except AuditError as exc:
                        if exc.status not in {0, 429, 500, 502, 503, 504} or attempt == 5:
                            raise
                        time.sleep(min(30, 2 ** attempt))
                for row in batch["results"]:
                    if row["id"].startswith("_design/"):
                        continue
                    doc = row.get("doc", {})
                    doc.pop("_rev", None)
                    doc = validate(doc, audit.tenant)
                    if not args.event_type or doc["event_type"] == args.event_type:
                        print(json.dumps(doc, sort_keys=True), flush=True)
                state["since"] = batch["last_seq"]  # Opaque; never increment or parse as an integer.
                checkpoint(args.cursor, state)
    except KeyboardInterrupt:
        return 0
    except (AuditError, ValueError, KeyError, OSError):
        print("Audit operation failed; check configuration, permissions and source health.", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
