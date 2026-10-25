#!/usr/bin/env python3
"""Real disposable Server/Couch/Rabbit acceptance; never a mocked service gate.

The Lisp fixture supplies its embedded native host and synthetic actor. This
driver uses supported authenticated HTTP operations and a real Rabbit queue,
then launches a fresh Lisp process to verify persistence. Missing services,
native libraries, fixture interfaces, or prerequisites are failures, not skips.
"""
import base64
import copy
import json
import os
from pathlib import Path
import re
import secrets
import socket
import subprocess
import sys
import tempfile
import time
from urllib.error import HTTPError, URLError
from urllib.parse import quote, urlsplit
from urllib.request import HTTPRedirectHandler, ProxyHandler, Request, build_opener

ROOT = Path(__file__).resolve().parents[1]


class Failure(RuntimeError):
    pass


class NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, *args, **kwargs):
        return None


class Client:
    def __init__(self, base, authorization=None):
        parsed = urlsplit(base)
        if (parsed.scheme != "http" or parsed.hostname not in ("127.0.0.1", "::1")
                or parsed.username or parsed.password or parsed.query or parsed.fragment):
            raise Failure("only literal loopback HTTP fixture endpoints are allowed")
        self.base = base.rstrip("/")
        self.authorization = authorization
        self.opener = build_opener(ProxyHandler({}), NoRedirect())

    def request(self, method, path, body=None):
        headers = {"Content-Type": "application/json"}
        if self.authorization:
            headers["Authorization"] = self.authorization
        request = Request(self.base + path, method=method, headers=headers,
                          data=None if body is None else json.dumps(body).encode())
        try:
            response = self.opener.open(request, timeout=5)
        except HTTPError as error:
            response = error
        with response:
            raw = response.read(1048577)
            if len(raw) > 1048576:
                raise Failure("HTTP response exceeded fixture byte limit")
            try:
                value = json.loads(raw)
            except (ValueError, UnicodeError):
                value = None
            return response.status, value

    def expect(self, method, path, statuses, body=None):
        status, value = self.request(method, path, body)
        if status not in statuses:
            raise Failure(f"{method} {path.split('?')[0]} returned {status}; expected {sorted(statuses)}")
        return value


def free_port():
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


def private_write(path, text):
    descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    with os.fdopen(descriptor, "w") as stream:
        stream.write(text)


def preserve_diagnostics(state, env):
    destination = env.get("STAR_ROUTER_E2E_EVIDENCE")
    if not destination:
        return
    output = Path(destination)
    output.mkdir(mode=0o700, parents=True, exist_ok=True)
    secret_values = [env.get(name, "") for name in
                     ("COUCHDB_PASSWORD", "RABBITMQ_PASSWORD", "STAR_AUTH_PEPPER", "STAR_AUTH_BOOTSTRAP_SECRET")]
    for name in ("api-key", "denied-key", "pepper", "bootstrap"):
        if (state / name).exists():
            secret_values.append((state / name).read_text().strip())
    for path in state.glob("*.log"):
        with open(path, "rb") as source:
            source.seek(max(0, path.stat().st_size - 1048576))
            text = source.read(1048576).decode(errors="replace")
        for value in sorted(filter(None, secret_values), key=len, reverse=True):
            text = text.replace(value, "[REDACTED]")
        text = re.sub(r"star_sk_v1_[A-Za-z0-9_-]+", "[REDACTED_API_KEY]", text)
        (output / path.name).write_text(text)
        os.chmod(output / path.name, 0o600)


def readback(client, expected, *, wait=30):
    deadline = time.monotonic() + wait
    path = "/api/v1/documents/" + quote(expected["id"], safe="")
    while True:
        status, value = client.request("GET", path)
        if status == 200:
            if not isinstance(value, dict):
                raise Failure("document readback was not a canonical object")
            actual = dict(value)
            revision = actual.pop("rev", None)
            if not isinstance(revision, str) or not revision:
                raise Failure("durable readback has no revision")
            if actual != expected:
                # Do not expose document values in failure logs.
                raise Failure("durable canonical content differs from submitted fixture")
            return revision
        if status != 404 or time.monotonic() >= deadline:
            raise Failure(f"durable readback did not complete (HTTP {status})")
        time.sleep(0.1)


class ServerProcess:
    def __init__(self, state, env, mode):
        self.state = state
        for name in ("ready", "stop", "actor-done", "actor-error"):
            (state / name).unlink(missing_ok=True)
        self.log = open(state / (mode + ".log"), "wb")
        child_env = dict(env, STAR_ROUTER_E2E_MODE=mode)
        self.process = subprocess.Popen(
            ["sbcl", "--noinform", "--no-sysinit", "--no-userinit", "--script",
             str(ROOT / "tests/embedded-router-application.lisp")],
            env=child_env, stdout=self.log, stderr=subprocess.STDOUT)

    def ready(self):
        deadline = time.monotonic() + 90
        while not (self.state / "ready").exists():
            if self.process.poll() is not None:
                raise Failure("Lisp Server fixture exited before readiness")
            if time.monotonic() >= deadline:
                raise Failure("Lisp Server fixture readiness deadline exceeded")
            time.sleep(0.1)

    def stop(self):
        (self.state / "stop").touch(mode=0o600)
        try:
            result = self.process.wait(timeout=15)
        except subprocess.TimeoutExpired:
            self.process.terminate()
            try:
                self.process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.process.kill()
                self.process.wait(timeout=5)
            raise Failure("Lisp Server fixture did not shut down within its bound")
        finally:
            self.log.close()
        if result:
            raise Failure(f"Lisp Server fixture exited unsuccessfully ({result})")


def run():
    if sys.argv[1:] not in ([], ["--rabbit-only"], ["--sc01"]):
        raise Failure("unknown application fixture mode")
    sc01_enabled = sys.argv[1:] == ["--sc01"]
    native_enabled = sys.argv[1:] != ["--rabbit-only"]
    if os.environ.get("STAR_ROUTER_E2E_ACK_DISPOSABLE") != "1":
        raise Failure("set STAR_ROUTER_E2E_ACK_DISPOSABLE=1 for dedicated disposable services")
    required = ("STAR_ROUTER_LIBRARY", "STAR_LANG_SOURCE", "COUCHDB_PASSWORD", "RABBITMQ_PASSWORD")
    if any(not os.environ.get(name) for name in required):
        raise Failure("native library, pinned language source and fixture service credentials required")
    if os.environ.get("COUCHDB_HOST", "127.0.0.1") != "127.0.0.1" or os.environ.get("RABBITMQ_ADDRESS", "127.0.0.1") != "127.0.0.1":
        raise Failure("application gate requires literal loopback service hosts")
    run_id = "embedded-router-" + secrets.token_hex(6)
    with tempfile.TemporaryDirectory(prefix=run_id + "-") as directory:
        state = Path(directory)
        os.chmod(state, 0o700)
        env = dict(os.environ)
        env.update(STAR_ROUTER_TEST_ROOT=str(ROOT), STAR_ROUTER_E2E_STATE=str(state),
                   STAR_ROUTER_E2E_RUN=run_id, COUCHDB_DATABASE=run_id,
                   STAR_AUTH_DATABASE=run_id + "-auth", HTTP_API_LISTEN_ADDRESS="127.0.0.1",
                   HTTP_API_PORT=str(free_port()), STAR_ROUTER_E2E_PORT=str(free_port()),
                   STAR_ROUTER_E2E_PUB_PORT=str(free_port()),
                   STAR_AUTH_PEPPER_FILE=str(state / "pepper"),
                   STAR_AUTH_BOOTSTRAP_SECRET_FILE=str(state / "bootstrap"),
                   STAR_TENANT_DATASET_MAP=run_id + "=" + run_id,
                   STAR_ROUTER_E2E_DATASET=run_id, STAR_ROUTER_E2E_TENANT=run_id)
        private_write(state / "pepper", secrets.token_hex(32))
        private_write(state / "bootstrap", secrets.token_hex(32))
        private_write(state / "api-key", "")
        private_write(state / "denied-key", "")
        private_write(state / "init.lisp", '(in-package :star)\n(setf *public-mode* nil *ingest-workers* 1 *couchdb-event-log-database* "' + run_id + '-events")\n')
        fixtures = json.loads((ROOT / "schemas/starintel-0.10.1/research-fixtures.json").read_text())
        document = copy.deepcopy(next(item["document"] for item in fixtures
                                      if item["name"] == "minimal-operation" and item["valid"]))
        document.update(id=run_id + (":sc01" if sc01_enabled else ":native"), dataset=run_id)
        # The canonical fixture deliberately omits optional extensions. Never
        # normalize them into existence to hide a storage/readback regression.
        private_write(state / "document.json", json.dumps(document))
        base = f"http://127.0.0.1:{env['HTTP_API_PORT']}"
        rabbit_base = f"http://127.0.0.1:{env.get('RABBITMQ_MANAGEMENT_PORT', '15672')}"
        basic = base64.b64encode((env.get("RABBITMQ_USER", "guest") + ":" + env["RABBITMQ_PASSWORD"]).encode()).decode()
        rabbit = Client(rabbit_base, "Basic " + basic)
        queue = run_id + "-actor"
        actor = "star:v1:collector:embedded-fixture"
        rabbit_actor = "star:v1:collector:rabbit-fixture"
        server = None
        queue_created = False
        try:
            server = ServerProcess(state, env, "sc01" if sc01_enabled else ("exercise" if native_enabled else "rabbit"))
            server.ready()
            api = Client(base, "Bearer " + (state / "api-key").read_text().strip())
            denied = Client(base, "Bearer " + (state / "denied-key").read_text().strip())
            target = {"actor": actor, "target": "synthetic:fixture", "dataset": run_id,
                      "idempotency_key": run_id + ":target", "delay": 1, "recurring": False}
            durable_records = []
            if native_enabled:
                Client(base).expect("POST", "/api/v1/targets", {401}, target)
                denied.expect("POST", "/api/v1/targets", {403}, target)
                first = api.expect("POST", "/api/v1/targets", {201}, target)
                repeated = api.expect("POST", "/api/v1/targets", {200}, target)
                if first.get("target_id") != repeated.get("target_id"):
                    raise Failure("Target replay changed logical identity")
                api.expect("POST", "/api/v1/targets", {409}, dict(target, target="synthetic:conflict"))
                revision = readback(api, document)
                denied.expect("GET", "/api/v1/documents/" + quote(document["id"], safe=""), {403})
                deadline = time.monotonic() + 30
                while not (state / "actor-done").exists():
                    if (state / "actor-error").exists() or time.monotonic() >= deadline:
                        raise Failure("synthetic native actor lifecycle/replay proof did not complete")
                    time.sleep(0.1)
                if readback(api, document) != revision:
                    raise Failure("native replay changed stored revision")
                for suffix in ((":denied",) if sc01_enabled else (":denied", ":cancelled")):
                    api.expect("GET", "/api/v1/documents/" + quote(run_id + suffix, safe=""), {404})
                durable_records.append((document, revision))
                print("PASS " + ("SC01" if sc01_enabled else "Actor2Actor") + " Target→actor→document→durable HTTP readback/replay/denial", flush=True)

            if not sc01_enabled:
                rabbit.expect("PUT", "/api/queues/%2F/" + queue, {201, 204},
                              {"durable": False, "auto_delete": False, "arguments": {}})
                queue_created = True
                rabbit.expect("POST", "/api/bindings/%2F/e/documents/q/" + queue, {201, 204},
                              {"routing_key": "documents.target.dispatch." + rabbit_actor, "arguments": {}})
                api.expect("POST", "/api/v1/targets", {201},
                           dict(target, actor=rabbit_actor, idempotency_key=run_id + ":rabbit-target"))
                deadline = time.monotonic() + 30
                while True:
                    messages = rabbit.expect("POST", "/api/queues/%2F/" + queue + "/get", {200},
                                             {"count": 1, "ackmode": "ack_requeue_false", "encoding": "auto", "truncate": 1048576})
                    if messages:
                        delivered = json.loads(messages[0]["payload"])
                        if delivered.get("actor") != rabbit_actor or delivered.get("dataset") != run_id:
                            raise Failure("Rabbit target was misrouted")
                        break
                    if time.monotonic() >= deadline:
                        raise Failure("Rabbit target was not delivered")
                    time.sleep(0.1)
                rabbit_document = dict(document, id=run_id + ":rabbit")
                internal_document = dict(rabbit_document, tenant_id=run_id)
                publication = {"properties": {"content_type": "application/json", "delivery_mode": 2,
                                               "message_id": run_id + ":rabbit-document"},
                               "routing_key": "documents.ingest." + document["dtype"],
                               "payload": json.dumps(internal_document), "payload_encoding": "string"}
                before_ack = rabbit.expect("GET", "/api/queues/%2F/documents.ingest", {200}).get("message_stats", {}).get("ack", 0)
                if rabbit.expect("POST", "/api/exchanges/%2F/documents/publish", {200}, publication).get("routed") is not True:
                    raise Failure("Rabbit ingest publication was not routed")
                rabbit_revision = readback(api, rabbit_document)
                rabbit.expect("POST", "/api/exchanges/%2F/documents/publish", {200}, publication)
                # Wait for both real consumer acknowledgments, not a fixed sleep or
                # an eventually stale queue-depth snapshot. Readback still supplies
                # the independent application storage assertion.
                deadline = time.monotonic() + 30
                while rabbit.expect("GET", "/api/queues/%2F/documents.ingest", {200}).get("message_stats", {}).get("ack", 0) < before_ack + 2:
                    if time.monotonic() >= deadline:
                        raise Failure("Rabbit did not acknowledge both original and replay")
                    time.sleep(0.25)
                if readback(api, rabbit_document) != rabbit_revision:
                    raise Failure("Rabbit duplicate changed stored revision")
                durable_records.append((rabbit_document, rabbit_revision))
                print("PASS real Rabbit Target delivery and document ingest/readback", flush=True)
            server.stop()
            server = ServerProcess(state, env, "readback")
            server.ready()
            api = Client(base, "Bearer " + (state / "api-key").read_text().strip())
            for saved_document, saved_revision in durable_records:
                if readback(api, saved_document) != saved_revision:
                    raise Failure("fresh Lisp process did not preserve acknowledged content/revisions")
            print("PASS fresh Lisp Server process durable readback", flush=True)
        finally:
            try:
                if server is not None:
                    if server.process.poll() is None:
                        server.stop()
                    else:
                        server.log.close()
            finally:
                try:
                    if queue_created:
                        rabbit.expect("DELETE", "/api/queues/%2F/" + queue, {204})
                finally:
                    preserve_diagnostics(state, env)


if __name__ == "__main__":
    try:
        run()
    except (Failure, OSError, ValueError, URLError) as error:
        # No fixture credentials, request/response bodies or process logs.
        print("FAIL embedded application acceptance: " + str(error), file=sys.stderr)
        sys.exit(1)
