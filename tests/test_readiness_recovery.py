import importlib.util
import pathlib
import tempfile
import unittest
import http.server
import threading

MODULE = pathlib.Path(__file__).resolve().parents[1] / 'scripts/readiness_recovery.py'
spec = importlib.util.spec_from_file_location('recovery', MODULE)
r = importlib.util.module_from_spec(spec)
spec.loader.exec_module(r)


class RecoveryTest(unittest.TestCase):
    def test_attachment_round_trip(self):
        documents = [{'_id': 'synthetic', '_rev': '1-a', '_attachments': {
            'fixture.txt': {'data': 'Zml4dHVyZQ==', 'content_type': 'text/plain', 'revpos': 1}}}]
        clean = r.normalize(documents)
        self.assertEqual(clean[0]['_attachments']['fixture.txt']['data'], 'Zml4dHVyZQ==')
        self.assertNotIn('_rev', clean[0])
        self.assertEqual(r.digest(clean), r.digest(r.normalize(documents)))

    def test_attachment_stub_fails(self):
        with self.assertRaises(r.RecoveryError):
            r.normalize([{'_id': 'a', '_attachments': {'x': {'stub': True}}}])

    def test_bulk_partial_failure_fails(self):
        for response in ([{'id': 'a', 'error': 'conflict'}], [],
                         [{'id': 'other', 'ok': True}], [{'id': 'a', 'ok': True}, {'id': 'a', 'ok': True}]):
            with self.subTest(response=response), self.assertRaises(r.RecoveryError):
                r.validate_bulk(response, [{'_id': 'a'}])

    def test_existing_database_is_never_written(self):
        calls = []
        def request(method, path, body=None):
            calls.append((method, path))
            return 412, {'error': 'file_exists'}
        with self.assertRaises(r.RecoveryError):
            r.restore(request, 'readiness-existing', [{'_id': 'a'}])
        self.assertEqual(calls, [('PUT', 'readiness-existing')])

    def test_restore_verifies_readback(self):
        calls = []
        def request(method, path, body=None):
            calls.append(method)
            if method == 'PUT': return 201, {'ok': True}
            if method == 'POST': return 201, [{'id': 'a', 'ok': True}]
            return 200, {'rows': [{'doc': {'_id': 'a', '_rev': '1-x'}}]}
        r.restore(request, 'readiness-new', [{'_id': 'a'}])
        self.assertEqual(calls, ['PUT', 'POST', 'GET'])

    def test_wrong_readback_fails(self):
        def request(method, path, body=None):
            if method == 'PUT': return 201, {'ok': True}
            if method == 'POST': return 201, [{'id': 'a', 'ok': True}]
            return 200, {'rows': []}
        with self.assertRaises(r.RecoveryError):
            r.restore(request, 'readiness-new', [{'_id': 'a'}])

    def test_backup_exclusive_private_write(self):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory) / 'backup.json'
            r.write_backup(path, [{'_id': 'a'}])
            self.assertEqual(path.stat().st_mode & 0o777, 0o600)
            with self.assertRaises(FileExistsError):
                r.write_backup(path, [])
            self.assertEqual(r.read_backup(path), [{'_id': 'a'}])

    def test_http_errors_do_not_expose_body_or_follow_redirect(self):
        calls = []
        class Handler(http.server.BaseHTTPRequestHandler):
            def do_GET(self):
                calls.append(self.path)
                self.send_response(302)
                self.send_header('Location', '/secret-target')
                self.end_headers()
                self.wfile.write(b'private-password')
            def log_message(self, *args):
                pass
        server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Handler)
        thread = threading.Thread(target=server.serve_forever)
        thread.start()
        try:
            with tempfile.TemporaryDirectory() as directory:
                password = pathlib.Path(directory) / 'secret'
                password.write_text('private-password')
                request = r.transport('http://127.0.0.1:' + str(server.server_port), 'admin', password)
                self.assertEqual(request('GET', 'readiness-a'), (302, None))
                self.assertEqual(calls, ['/readiness-a'])
        finally:
            server.shutdown()
            thread.join()
            server.server_close()

    def test_backup_corruption_is_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory) / 'backup.json'
            r.write_backup(path, [{'_id': 'a'}])
            path.write_text(path.read_text().replace('"_id": "a"', '"_id": "b"'))
            with self.assertRaises(r.RecoveryError):
                r.read_backup(path)

    def test_scope_rejects_real_database(self):
        with self.assertRaises(r.RecoveryError):
            r.database_name('starintel')

if __name__ == '__main__':
    unittest.main()
