"""Opt-in real HTTP + CouchDB + RabbitMQ byte/event conformance.

Requires the disposable authenticated fixture in t/file-api-live-fixture.lisp.
Run with STAR_FILE_LIVE_TESTS=1 and STARINTEL_SERVER_TOKEN_FILE set.
"""
import base64
import hashlib
import json
import os
from pathlib import Path
import time
import unittest
from urllib.error import HTTPError
from urllib.parse import quote
from urllib.request import Request, urlopen
import uuid


@unittest.skipUnless(os.getenv("STAR_FILE_LIVE_TESTS") == "1", "opt-in live file services")
class LiveFileApiTests(unittest.TestCase):
    def setUp(self):
        import pika
        self.pika = pika
        self.token = Path(os.environ["STARINTEL_SERVER_TOKEN_FILE"]).read_text().strip()
        self.base = os.getenv("STAR_FILE_HTTP_URL", "http://127.0.0.1:15985")
        self.connection = pika.BlockingConnection(pika.URLParameters(
            os.getenv("STAR_FILE_RABBIT_URL", "amqp://fixture-user:fixture-password-not-for-production@127.0.0.1:15673/%2F")))
        self.channel = self.connection.channel()
        self.channel.exchange_declare(exchange="documents", exchange_type="topic", durable=True)
        self.queue = self.channel.queue_declare(queue="", exclusive=True).method.queue
        self.channel.queue_bind(queue=self.queue, exchange="documents", routing_key="documents.new.#")
        self.channel.queue_bind(queue=self.queue, exchange="documents", routing_key="documents.updated.#")

    def tearDown(self):
        self.connection.close()

    def request(self, method, path, payload=None, authenticated=True):
        headers = {"Content-Type": "application/json"}
        if authenticated:
            headers["Authorization"] = "Bearer " + self.token
        req = Request(self.base + path, data=None if payload is None else json.dumps(payload).encode(),
                      headers=headers, method=method)
        try:
            response = urlopen(req, timeout=20)
        except HTTPError as err:
            return err.code, err.read(), err.headers
        return response.status, response.read(), response.headers

    def document(self, kind, suffix):
        content = b"\x00\x01\x02\xff"
        doc = {"id": "live-file:" + suffix + ":" + uuid.uuid4().hex,
               "dataset": "live-files", "dtype": kind, "schemaVersion": "0.10.1",
               "bytesHash": hashlib.sha256(content).hexdigest(), "bytesHashAlgorithm": "sha256",
               "sizeBytes": len(content), "filename": "untrusted/../../image.png"}
        return doc, {"document": doc, "contentBase64": base64.b64encode(content).decode()}, content

    def await_document(self, doc_id):
        path = "/api/v1/documents/" + quote(doc_id, safe="")
        deadline = time.monotonic() + 15
        while time.monotonic() < deadline:
            status, body, _ = self.request("GET", path)
            if status == 200:
                return json.loads(body)
            self.assertEqual(status, 404, body)
            self.connection.sleep(.05)
        self.fail("document did not become durable")

    def await_event(self, doc_id, operation):
        deadline = time.monotonic() + 15
        while time.monotonic() < deadline:
            method, properties, body = self.channel.basic_get(queue=self.queue, auto_ack=True)
            if method:
                event = json.loads(body)
                if event.get("id") == doc_id and event["extensions"]["event_operation"] == operation:
                    self.assertNotIn("_attachments", event)
                    self.assertNotIn("_id", event)
                    self.assertEqual(properties.message_id, event["extensions"]["event_id"])
                    # Event is observed only after committed bytes are readable.
                    status, content, _ = self.request("GET", "/api/v1/files/" + quote(doc_id, safe="") + "/content")
                    self.assertEqual(status, 200, content)
                    return event
            else:
                self.connection.sleep(.05)
        self.fail("committed file event was not observed")

    def test_http_and_rabbit_bytes_metadata_updates_and_replay(self):
        for kind in ("file", "image", "picture"):
            doc, envelope, content = self.document(kind, "http")
            status, body, _ = self.request("POST", "/api/v1/files", envelope)
            self.assertEqual(status, 200, body)
            receipt = json.loads(body)
            self.assertIn("rev", receipt)
            self.assertNotIn("_attachments", receipt)
            self.await_event(doc["id"], "new")
            content_path = "/api/v1/files/" + quote(doc["id"], safe="") + "/content"
            status, body, headers = self.request("GET", content_path)
            self.assertEqual(status, 200, body)
            self.assertEqual(body, content)
            self.assertEqual(headers["Content-Type"], "application/octet-stream")
            status, _, _ = self.request("GET", content_path, authenticated=False)
            self.assertEqual(status, 401)
            status, body, _ = self.request("PUT", "/api/v1/documents/" + quote(doc["id"], safe=""),
                                           {"verificationStatus": "confirmed", "notes": "reviewed"})
            self.assertEqual(status, 200, body)
            status, body, _ = self.request("POST", "/api/v1/files", envelope)
            self.assertEqual(status, 200, body)
            self.assertEqual(json.loads(body)["verificationStatus"], "confirmed")
            self.assertEqual(self.request("GET", content_path)[1], content)

            broker_doc, broker_envelope, _ = self.document(kind, "rabbit")
            broker_envelope["document"]["tenant_id"] = "default"
            self.channel.basic_publish(exchange="documents", routing_key="files.ingest." + kind,
                                       body=json.dumps(broker_envelope),
                                       properties=self.pika.BasicProperties(content_type="application/json", delivery_mode=2))
            self.await_event(broker_doc["id"], "new")
            self.assertEqual(self.await_document(broker_doc["id"])["bytesHash"], broker_doc["bytesHash"])

        doc, envelope, content = self.document("picture", "metadata-first")
        doc["verificationStatus"] = "confirmed"
        status, body, _ = self.request("POST", "/api/v1/documents", doc)
        self.assertEqual(status, 200, body)
        stored = self.await_document(doc["id"])
        content_path = "/api/v1/files/" + quote(doc["id"], safe="") + "/content"
        self.assertEqual(self.request("GET", content_path)[0], 404)
        envelope["document"] = stored
        status, body, _ = self.request("POST", "/api/v1/files", envelope)
        self.assertEqual(status, 200, body)
        self.assertEqual(json.loads(body)["verificationStatus"], "confirmed")
        self.assertEqual(self.request("GET", content_path)[1], content)
        self.await_event(doc["id"], "updated")


if __name__ == "__main__":
    unittest.main()
