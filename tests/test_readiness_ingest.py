import importlib.util
import pathlib
import unittest
from unittest.mock import patch
import tempfile
import os

spec = importlib.util.spec_from_file_location('readiness_ingest', pathlib.Path(__file__).parents[1] / 'scripts/readiness_ingest.py')
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)

class FixtureTests(unittest.TestCase):
    def test_synthetic_identity(self):
        a = m.fixture_document('run-1', 0)
        self.assertEqual(a, m.fixture_document('run-1', 0))
        self.assertNotEqual(a['id'], m.fixture_document('run-2', 0)['id'])
        self.assertEqual('message', a['dtype'])

    def test_exact_equality_only_revision_exempt(self):
        a = m.fixture_document('run', 1)
        self.assertTrue(m.canonical_equal(a, dict(a, rev='1-x')))
        self.assertFalse(m.canonical_equal(a, dict(a, extensions={})))
        self.assertFalse(m.canonical_equal(a, dict(a, message='changed')))

    def test_bounds(self):
        for run, index in [('bad/identifier', 0), ('run', -1), ('run', True)]:
            with self.assertRaises(ValueError):
                m.fixture_document(run, index)

    def test_confirmed_read_latency(self):
        doc = m.fixture_document('run', 0)
        calls = []
        config = {'run_id': 'run', 'timeout_seconds': 1}
        result = m.run_fixture(config, 0, publish=lambda c,d: calls.append(d), read=lambda c,i,t: dict(doc, rev='1-ok'))
        self.assertEqual([doc], calls)
        self.assertTrue(result['canonical_equal'])
        self.assertGreaterEqual(result['latency_ms'], 0)

    def test_missing_read_polls_but_publish_occurs_once(self):
        document = m.fixture_document('run', 0)
        with patch.object(m.time, 'sleep'):
            reads = iter([None, dict(document, rev='1-ok')])
            published = []
            result = m.run_fixture({'run_id':'run'}, 0,
                                   publish=lambda *a: published.append(a),
                                   read=lambda *a: next(reads))
        self.assertEqual(1, len(published))
        self.assertTrue(result['canonical_equal'])

    def test_secret_file_mode(self):
        with tempfile.NamedTemporaryFile(mode='w') as file:
            file.write('synthetic-secret\n')
            file.flush()
            self.assertEqual('synthetic-secret', m._secret(file.name))
            os.chmod(file.name, 0o644)
            with self.assertRaises(m.FixtureError):
                m._secret(file.name)

    def test_publish_failure_never_reads(self):
        def fail(*args):
            raise m.FixtureError('publish failed')
        def forbidden(*args):
            self.fail('read after failed publish')
        with self.assertRaises(m.FixtureError):
            m.run_fixture({'run_id':'run'}, 0, publish=fail, read=forbidden)

    def test_mismatch_fails(self):
        with self.assertRaises(m.FixtureError):
            m.run_fixture({'run_id':'run'}, 0, publish=lambda *a: None, read=lambda *a: {})

if __name__ == '__main__':
    unittest.main()
