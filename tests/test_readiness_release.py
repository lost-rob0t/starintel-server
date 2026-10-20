"""Verifier contract tests: synthetic manifests are never runtime proof."""
import hashlib
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]

class ReleaseVerifierTests(unittest.TestCase):
    def setUp(self):
        spec = importlib.util.spec_from_file_location('release', ROOT / 'scripts/readiness_release.py')
        self.module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.module)
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.head = 'a' * 40
        for name in self.module.LOCKS:
            path = self.root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text('fixture lock')
        (self.root / 'proof.json').write_text('{}')
        proof = {'path': 'proof.json', 'sha256': hashlib.sha256(b'{}').hexdigest()}
        self.manifest = {
            'format': 1, 'source': self.head,
            'locks': {n: hashlib.sha256(b'fixture lock').hexdigest() for n in self.module.LOCKS},
            'images': {n: 'sha256:' + 'b' * 64 for n in self.module.IMAGES},
            'gates': {n: {'status': 'passed', 'source': self.head, 'execution': 'service-backed',
                          'command': 'synthetic-verifier-test-only', 'assertions': 1,
                          'exitCode': 0, 'evidence': proof} for n in self.module.GATES},
            'ci': {'source': self.head, 'status': 'passed', 'evidence': proof},
            'review': {'source': self.head, 'status': 'approved', 'blocking': False, 'evidence': proof},
        }
        self.manifest['gates']['load']['measurements'] = {
            'durationSeconds': 3600, 'requests': 100, 'p95Milliseconds': 10,
            'peakMemoryBytes': 1000, 'peakQueueBacklog': 2, 'finalQueueBacklog': 0,
            'errors': 0, 'concurrency': 1, 'documents': 100,
        }
        self.manifest['gates']['ingestion']['measurements'] = {
            'publisherConfirms': 3, 'expectedRecords': 3, 'persistedRecords': 3,
            'persistedEquality': True, 'duplicateReplay': True, 'restartReadback': True,
            'schemaRejection': True, 'supportedClientReadback': True,
        }
        self.manifest['gates']['recovery']['measurements'] = {
            'cleanInstance': True, 'identicalRecords': True, 'upgrade': 'passed',
            'rollback': 'passed', 'baselineImage': 'sha256:' + 'c' * 64,
            'candidateImage': 'sha256:' + 'b' * 64,
        }
        self.manifest['gates']['isolation'].update({
            'boundedRequests': True, 'boundedQueryWork': True,
            'coverage': {name: {'status': 'passed', 'assertions': 1}
                         for name in ('reads', 'writes', 'search', 'attachments', 'changes')},
        })
        self.manifest['gates']['load']['testedLimits'] = {'description': 'test fixture, not performance evidence'}

    def verify(self):
        self.module.verify(self.manifest, self.root, self.head)

    def test_structurally_complete_manifest(self):
        self.verify()

    def test_missing_or_fake_gate_fails(self):
        for name in self.module.GATES:
            with self.subTest(name=name):
                gate = self.manifest['gates'].pop(name)
                with self.assertRaises(ValueError): self.verify()
                self.manifest['gates'][name] = gate
        self.manifest['gates']['startup']['execution'] = 'mock'
        with self.assertRaises(ValueError): self.verify()

    def test_wrong_head_and_stale_reviews_fail(self):
        for section in ('ci', 'review'):
            self.manifest[section]['source'] = 'c' * 40
            with self.assertRaises(ValueError): self.verify()
            self.manifest[section]['source'] = self.head
        self.manifest['source'] = 'd' * 40
        with self.assertRaises(ValueError): self.verify()

    def test_tampered_lock_and_proof_fail(self):
        (self.root / 'proof.json').write_text('tampered')
        with self.assertRaises(ValueError): self.verify()
        (self.root / 'proof.json').write_text('{}')
        (self.root / 'flake.lock').write_text('changed')
        with self.assertRaises(ValueError): self.verify()

    def test_missing_measurement_or_unbounded_proof_fails(self):
        del self.manifest['gates']['load']['measurements']['p95Milliseconds']
        with self.assertRaises(ValueError): self.verify()

    def test_path_escape_and_mutable_image_fail(self):
        self.manifest['gates']['startup']['evidence']['path'] = '../outside'
        with self.assertRaises(ValueError): self.verify()
        self.manifest['gates']['startup']['evidence']['path'] = 'proof.json'
        self.manifest['images']['server'] = 'latest'
        with self.assertRaises(ValueError): self.verify()

    def test_failed_run_zero_assertions_and_blocking_review_fail(self):
        gate = self.manifest['gates']['ingestion']
        for key, value in (('exitCode', 1), ('assertions', 0), ('status', 'skipped')):
            original = gate[key]
            gate[key] = value
            with self.assertRaises(ValueError): self.verify()
            gate[key] = original
        self.manifest['review']['blocking'] = True
        with self.assertRaises(ValueError): self.verify()

    def test_invalid_or_failed_load_fails(self):
        measures = self.manifest['gates']['load']['measurements']
        for key, value in (('durationSeconds', 0), ('requests', True),
                           ('p95Milliseconds', float('nan')), ('errors', 1),
                           ('finalQueueBacklog', 1)):
            original = measures[key]
            measures[key] = value
            with self.assertRaises(ValueError): self.verify()
            measures[key] = original

    def test_smoke_cannot_count_as_soak(self):
        self.manifest['gates']['load']['measurements']['durationSeconds'] = 3599
        with self.assertRaisesRegex(ValueError, '3600'): self.verify()

    def test_same_image_rollback_and_wrong_candidate_fail(self):
        recovery = self.manifest['gates']['recovery']['measurements']
        recovery['baselineImage'] = recovery['candidateImage']
        with self.assertRaisesRegex(ValueError, 'same-image'): self.verify()
        recovery['baselineImage'] = 'sha256:' + 'c' * 64
        recovery['candidateImage'] = 'sha256:' + 'd' * 64
        with self.assertRaisesRegex(ValueError, 'candidate server'): self.verify()

    def test_unverified_ingestion_and_inaccessible_isolation_fail(self):
        ingestion = self.manifest['gates']['ingestion']['measurements']
        ingestion['persistedRecords'] = 2
        with self.assertRaises(ValueError): self.verify()
        ingestion['persistedRecords'] = 3
        ingestion['duplicateReplay'] = False
        with self.assertRaises(ValueError): self.verify()
        ingestion['duplicateReplay'] = True
        self.manifest['gates']['isolation']['coverage']['attachments'] = {
            'status': 'inaccessible', 'reason': 'unsupported route'}
        with self.assertRaisesRegex(ValueError, 'attachments'): self.verify()

    def test_unsupported_surface_requires_denial_and_closed_database(self):
        record = {'supported': False, 'status': 'inaccessible', 'assertions': 2,
                  'reason': 'no supported attachment route', 'routeDenied': True,
                  'directDatabaseExposed': False}
        self.manifest['gates']['isolation']['coverage']['attachments'] = record
        self.verify()
        record['directDatabaseExposed'] = True
        with self.assertRaises(ValueError): self.verify()

    def test_version_tags_are_not_published_automatically(self):
        workflow = (ROOT / '.github/workflows/docker.yml').read_text()
        self.assertNotIn('starintel-server:0.1.0"', workflow)
        self.assertNotIn('starintel-couchdb:3.5.2"', workflow)
        self.assertIn('sha-${GITHUB_SHA}', workflow)

if __name__ == '__main__': unittest.main()
