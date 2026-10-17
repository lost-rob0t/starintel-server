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

    def test_stack_builds_exact_prerequisite_and_preserves_blocked_fec_hold(self):
        workflow = self.load(CI_FILES[0])
        job = workflow['jobs']['canonical-persistence']
        self.assertEqual(job['env']['CI341_SERVER_COMMIT'], 'c8ac745a6a6c14f33a083a607c556bf08c65220b')
        self.assertEqual(job['env']['CI341_PRO_ACTORS_COMMIT'], '6095facbc005caa1251e86936ce2a1339499f195')
        steps = job['steps']
        names = [step.get('name', '') for step in steps]
        fec = names.index('Preserve blocked FEC prerequisite after independent Server proof')
        persistence = names.index('Real canonical HTTP create/read and server plus CouchDB restart')
        self.assertLess(persistence, fec)
        self.assertNotIn('if', steps[fec])
        self.assertIn('FEC acceptance BLOCKED', steps[fec]['run'])
        self.assertIn('exit 1', steps[fec]['run'])
        self.assertNotIn('starintel-pro-actors', str([step.get('with', {}) for step in steps]))
        build = next(step['run'] for step in steps if step.get('name') == 'Build the existing server and its existing runtime image')
        self.assertIn('cd "$CI341_SERVER_SOURCE"', build)

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

    def test_unchanged_fec_probe_is_synthetic_exact_and_strict(self):
        import json, os, subprocess, tempfile
        from unittest import mock
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            probe = root / 'python/tests/fec_canonical_server_probe.py'
            probe.parent.mkdir(parents=True)
            probe.write_text('# unchanged probe fixture\n')
            runner = self.helper.Acceptance(root, root, probe, root / 'venv/bin/python')
            runner.network = 'owned-network'
            runner.docker = mock.Mock(return_value=subprocess.CompletedProcess([], 0,
                b'{"owned-network":{"IPAddress":"172.20.0.2"}}', b''))
            candidate, actor = 'a' * 40, 'b' * 40
            runner.result['serverSource'] = candidate
            def execute(command, **kwargs):
                environment = kwargs['env']
                self.assertEqual(command, [str(runner.fec_python), str(probe)])
                self.assertEqual(environment['STAR_SERVER_COMMIT'], candidate)
                self.assertEqual(environment['STAR_PRO_ACTORS_COMMIT'], actor)
                self.assertEqual(environment['STAR_SERVER_URL'], 'http://172.20.0.2:5000')
                self.assertFalse(set(environment) & {'FEC_PROVIDER_DOCUMENTS', 'FEC_PROVIDER_PROOF',
                    'FEC_API_KEY', 'STARINTEL_PROBE_FIXTURE', 'HTTPS_PROXY'})
                self.assertNotIn(runner.bootstrap, ' '.join(command))
                proof = {'outcome': 'passed', 'serverCommit': candidate, 'probeCommit': actor,
                    'providerPathRequested': False, 'publisherConfirms': 3,
                    'freshOutputAbsenceChecks': 3, 'cliSubmission': {'status': 'passed'},
                    'phases': ['fec-completed-job-replay-no-extra-effects']}
                (root / 'fec-canonical-server-proof.json').write_text(json.dumps(proof))
                return subprocess.CompletedProcess(command, 0, b'safe synthetic proof', b'')
            with mock.patch.dict(os.environ, {'CI341_PRO_ACTORS_COMMIT': actor,
                    'FEC_PROVIDER_DOCUMENTS': '/must-not-read', 'FEC_API_KEY': 'must-not-use',
                    'STARINTEL_PROBE_FIXTURE': 'must-not-use'}), \
                 mock.patch.object(self.helper.subprocess, 'check_output', return_value=actor), \
                 mock.patch.object(self.helper.subprocess, 'run', side_effect=execute), \
                 mock.patch('builtins.print'):
                runner.run_fec_probe('owned-server', 'owned-rabbit')
                self.assertEqual(runner.result['fecProbe'], 'passed')
                self.assertEqual(runner.result['fecProof']['probeCommit'], actor)
                # A failed actual probe never passes on a fabricated success file.
                with mock.patch.object(self.helper.subprocess, 'run', return_value=
                        subprocess.CompletedProcess([], 1, b'', b'')):
                    with self.assertRaises(RuntimeError): runner.run_fec_probe('owned-server', 'owned-rabbit')
                self.assertEqual(runner.result['fecProbe'], 'failed')

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
