"""CI coverage contracts, not evidence of a running server."""
from pathlib import Path
import unittest
import yaml

ROOT = Path(__file__).resolve().parents[1]
CI_FILES = [
    '.github/workflows/local-persistence.yml',
    'scripts/ci-local-persistence.py', 'tests/test_local_persistence_ci.py',
    'doc/local-persistence-ci.md', 'doc/testing.md',
    '.github/workflows/smoke-tests.yml', '.github/workflows/docker.yml',
    '.github/workflows/doc-coverage.yml', '.github/workflows/starintel-schema-lock.yml',
    '.github/workflows/operational-salvage.yml',
]

class CoverageContract(unittest.TestCase):
    def load(self, path):
        target = ROOT / path
        self.assertTrue(target.is_file(), 'bounded canonical persistence CI is missing')
        return yaml.safe_load(target.read_text())

    def test_one_bounded_secret_free_job(self):
        workflow = self.load(CI_FILES[0])
        events = workflow.get('on', workflow.get(True))
        self.assertEqual(set(events), {'pull_request', 'workflow_dispatch'})
        self.assertEqual(set(events['pull_request']['paths']), set(CI_FILES))
        self.assertEqual(workflow['permissions'], {'contents': 'read'})
        self.assertTrue(workflow['concurrency']['cancel-in-progress'])
        self.assertEqual(len(workflow['jobs']), 1)
        job = workflow['jobs']['canonical-persistence']
        self.assertEqual(job['runs-on'], 'ubuntu-24.04')
        self.assertLessEqual(job['timeout-minutes'], 35)
        self.assertNotIn('strategy', job)
        text = (ROOT / CI_FILES[0]).read_text()
        self.assertNotIn('${{ secrets.', text)
        self.assertIn('ci-local-persistence.py', text)
        self.assertIn('star-unit-tests', text)
        upload = next(s for s in job['steps'] if 'upload-artifact' in s.get('uses', ''))
        self.assertEqual(upload['with']['retention-days'], 1)

    def test_only_exact_ci_files_excluded_from_old_pr_workflows(self):
        for path in CI_FILES[5:]:
            with self.subTest(path=path):
                workflow = self.load(path)
                events = workflow.get('on', workflow.get(True))
                self.assertEqual(set(events['pull_request'].get('paths-ignore', [])), set(CI_FILES))
                self.assertTrue(workflow['jobs'], 'existing jobs must remain')


class HelperContracts(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        import importlib.util
        spec = importlib.util.spec_from_file_location('ci341_helper', ROOT / 'scripts/ci-local-persistence.py')
        cls.helper = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(cls.helper)

    def test_canonical_comparison_rejects_shape_and_content_changes(self):
        fixture = {'id': 'ci341:x', 'dataset': 'ci341-synthetic', 'dtype': 'person',
                   'schemaVersion': '0.10.1', 'fname': 'Synthetic CI fixture'}
        self.helper.assert_document(fixture | {'rev': '1-test'}, fixture)
        for bad in (fixture | {'fname': 'changed'}, fixture | {'_id': 'legacy'}, fixture | {'extensions': {}},
                    {k: v for k, v in fixture.items() if k != 'fname'}, [fixture]):
            with self.assertRaises(RuntimeError): self.helper.assert_document(bad, fixture)

    def test_internal_http_uses_stdin_and_rejects_redirects(self):
        import subprocess
        import tempfile
        from unittest import mock
        with tempfile.TemporaryDirectory() as tmp:
            runner = self.helper.Acceptance(Path(tmp), Path(tmp))
            runner.docker = mock.Mock(return_value=subprocess.CompletedProcess([], 0, b'{"status":"ready"}\n200', b''))
            self.assertEqual(runner.request('owned-server', 5000, '/ready', headers={'Authorization': 'Bearer unit-fixture-token'}), {'status': 'ready'})
            args, kwargs = runner.docker.call_args
            self.assertEqual(args[:5], ('exec', '-i', 'owned-server', 'curl', '--disable'))
            self.assertNotIn('unit-fixture-token', ' '.join(args))
            self.assertIn(b'Bearer unit-fixture-token', kwargs['input'])
            self.assertIn('--max-redirs', args)
            runner.docker.return_value = subprocess.CompletedProcess([], 47, b'{}\n302', b'')
            with self.assertRaises(self.helper.HTTPStatusError): runner.request('owned-server', 5000, '/ready')
            with self.assertRaises(RuntimeError): runner.request('owned-server', 5000, '//other/path')

    def test_single_node_setup_is_operational_and_confirmed(self):
        import tempfile
        from unittest import mock
        with tempfile.TemporaryDirectory() as tmp:
            runner = self.helper.Acceptance(Path(tmp), Path(tmp))
            runner.request = mock.Mock(side_effect=[{'state': 'cluster_enabled'}, {'ok': True}, {'state': 'single_node_enabled'}])
            runner.configure_couchdb('owned-probe')
            self.assertEqual(runner.request.call_args_list[1].args[:4], ('owned-probe', 5984, '/_cluster_setup', 'POST'))
            runner.request = mock.Mock(return_value={'state': 'single_node_enabled'})
            runner.configure_couchdb('owned-probe')
            self.assertTrue(all(len(c.args) == 3 for c in runner.request.call_args_list), 'configured node must not be rewritten')
            runner.request = mock.Mock(return_value={'state': 'cluster_disabled'})
            with self.assertRaises(RuntimeError): runner.configure_couchdb('owned-probe')

    def test_sanitized_evidence_is_bounded_and_removes_credentials(self):
        secret = 'synthetic-secret-value'
        result = self.helper.redacted((secret * 1000).encode(), [secret], limit=80)
        self.assertLessEqual(len(result), 80)
        self.assertNotIn(secret.encode(), result)

    def test_cleanup_checks_labels_and_never_removes_unowned_resources(self):
        import subprocess
        import tempfile
        from unittest import mock
        with tempfile.TemporaryDirectory() as tmp:
            runner = self.helper.Acceptance(Path(tmp), Path(tmp))
            runner.containers = ['ci341-owned-server']
            runner.network, runner.volume = 'ci341-owned-network', 'ci341-owned-data'
            runner.docker = mock.Mock(return_value=subprocess.CompletedProcess([], 0, b'other-owner\n', b''))
            with self.assertRaises(RuntimeError): runner.cleanup()
            self.assertEqual(runner.docker.call_count, 3)
            for call in runner.docker.call_args_list:
                self.assertNotIn('rm', call.args)
            runner.docker.reset_mock()
            runner.cleanup()
            runner.docker.assert_not_called()

    def test_interrupted_diagnostics_still_runs_cleanup(self):
        import tempfile
        from unittest import mock
        for mode in ('blocked-run', 'completed-run'):
            with self.subTest(mode=mode), tempfile.TemporaryDirectory() as tmp:
                runner = self.helper.Acceptance(Path(tmp), Path(tmp))
                runner.docker = mock.Mock(side_effect=RuntimeError('synthetic blocked stage'))
                runner.diagnostics = mock.Mock(side_effect=KeyboardInterrupt())
                runner.cleanup = mock.Mock()
                runner.result['status'] = 'passed'
                with mock.patch('builtins.print'):
                    with self.assertRaises(KeyboardInterrupt):
                        if mode == 'blocked-run': runner.run(Path(tmp) / 'synthetic.tar')
                        else: runner.finish()
                runner.cleanup.assert_called_once()
                self.assertEqual(runner.result['status'], 'failed')

    def test_termination_is_deferred_until_all_owned_cleanup_attempts(self):
        import os, signal, tempfile
        from unittest import mock
        with tempfile.TemporaryDirectory() as tmp:
            runner = self.helper.Acceptance(Path(tmp), Path(tmp))
            runner.containers = ['owned-a', 'owned-b']
            runner.volume, runner.network = 'owned-data', 'owned-network'
            calls = []
            def remove(kind, name):
                calls.append((kind, name))
                if len(calls) == 1: os.kill(os.getpid(), signal.SIGTERM)
            runner.remove_owned = remove
            runner.diagnostics = mock.Mock()
            runner.result['status'] = 'passed'
            previous = signal.signal(signal.SIGTERM, lambda *_: (_ for _ in ()).throw(KeyboardInterrupt()))
            try:
                with mock.patch('builtins.print'):
                    with self.assertRaises(KeyboardInterrupt): runner.finish()
                self.assertEqual(runner.result['status'], 'failed')
                self.assertEqual(calls, [('container', 'owned-b'), ('container', 'owned-a'),
                                         ('volume', 'owned-data'), ('network', 'owned-network')])
            finally: signal.signal(signal.SIGTERM, previous)

    def test_successful_cleanup_uses_only_owned_names(self):
        import subprocess
        import tempfile
        from unittest import mock
        with tempfile.TemporaryDirectory() as tmp:
            runner = self.helper.Acceptance(Path(tmp), Path(tmp))
            runner.containers = ['ci341-test-server']
            runner.network, runner.volume = 'ci341-test-network', 'ci341-test-data'
            runner.docker = mock.Mock(return_value=subprocess.CompletedProcess([], 0, (runner.prefix + '\n').encode(), b''))
            runner.cleanup()
            removes = [call.args for call in runner.docker.call_args_list if 'rm' in call.args]
            self.assertEqual(removes, [('rm', '--force', '--volumes', 'ci341-test-server'),
                                      ('volume', 'rm', 'ci341-test-data'), ('network', 'rm', 'ci341-test-network')])

if __name__ == '__main__': unittest.main()
