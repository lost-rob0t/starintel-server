"""Filesystem contract tests; these do not claim a running stack."""
import importlib.util
from pathlib import Path
import os
import json
import subprocess
from unittest.mock import patch
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]

class StackUpTests(unittest.TestCase):
    def setUp(self):
        spec = importlib.util.spec_from_file_location('stack_up', ROOT / 'scripts/stack-up.py')
        self.module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.module)
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.path = Path(self.temp.name) / 'credentials'

    def test_private_complete_and_nonoverwriting(self):
        self.module.prepare_secrets(self.path)
        before = {p.name: p.read_bytes() for p in self.path.iterdir()}
        self.assertEqual(set(before), set(self.module.SECRETS))
        self.assertEqual(len(before), 7)
        self.assertTrue(all(len(value.strip()) >= 48 for value in before.values()))
        self.assertEqual(self.path.stat().st_mode & 0o777, 0o700)
        self.assertTrue(all(p.stat().st_mode & 0o777 == 0o600 for p in self.path.iterdir()))
        self.module.prepare_secrets(self.path)
        self.assertEqual(before, {p.name: p.read_bytes() for p in self.path.iterdir()})

    def test_reject_symlink_directory(self):
        self.path.symlink_to(self.temp.name, target_is_directory=True)
        with self.assertRaises(ValueError):
            self.module.prepare_secrets(self.path)

    def test_reject_unsafe_existing_secret_without_writing(self):
        self.path.mkdir(mode=0o700)
        bad = self.path / self.module.SECRETS[-1]
        bad.write_text('existing')
        bad.chmod(0o644)
        with self.assertRaises(ValueError):
            self.module.prepare_secrets(self.path)
        self.assertEqual(list(self.path.iterdir()), [bad])

    def test_reject_symlink_ancestor(self):
        link = Path(self.temp.name) / 'link'
        link.symlink_to(self.temp.name, target_is_directory=True)
        with self.assertRaises(ValueError):
            self.module.prepare_secrets(link / 'new')
        self.assertFalse((Path(self.temp.name) / 'new').exists())

    def test_reject_hardlinked_secret(self):
        self.path.mkdir(mode=0o700)
        source = Path(self.temp.name) / 'source'
        source.write_text('existing')
        source.chmod(0o600)
        os.link(source, self.path / self.module.SECRETS[0])
        with self.assertRaises(ValueError):
            self.module.prepare_secrets(self.path)

    def test_reject_symlink_secret(self):
        self.path.mkdir(mode=0o700)
        (self.path / self.module.SECRETS[0]).symlink_to('/dev/null')
        with self.assertRaises(ValueError):
            self.module.prepare_secrets(self.path)

    def test_launcher_real_stub_processes(self):
        self.module.prepare_secrets(self.path)
        bindir = Path(self.temp.name) / 'bin'
        bindir.mkdir()
        log = Path(self.temp.name) / 'commands.jsonl'
        program = ('#!/usr/bin/env python3\n'
                   'import json, os, sys\n'
                   'with open(os.environ["STUB_LOG"], "a") as f:\n'
                   ' f.write(json.dumps([sys.argv, os.environ["CREDENTIALS_DIR"]]) + "\\n")\n'
                   'if os.environ.get("FAIL_READY") and sys.argv[-1].endswith("/ready"): sys.exit(7)\n')
        for name in ('nix', 'docker'):
            executable = bindir / name
            executable.write_text(program)
            executable.chmod(0o700)
        with patch.dict(os.environ, PATH=str(bindir) + os.pathsep + os.environ['PATH'],
                        STUB_LOG=str(log)):
            self.module.launch(self.path, 5, 5)
            calls = [json.loads(line) for line in log.read_text().splitlines()]
            self.assertEqual(len(calls), 4)
            self.assertEqual(calls[0][0][1:], ['run', '.#load-images'])
            self.assertTrue(all(call[1] == str(self.path) for call in calls))
            self.assertIn('--wait-timeout', calls[2][0])
            self.assertEqual(calls[-1][0][-1], 'http://localhost:5000/ready')
            with patch.dict(os.environ, FAIL_READY='1'):
                with self.assertRaises(subprocess.CalledProcessError):
                    self.module.launch(self.path, 5, 5)

    def test_reject_repo_and_store_paths(self):
        for path in (ROOT / 'secrets', Path('/nix/store/test-secrets')):
            with self.assertRaises(ValueError):
                self.module.prepare_secrets(path)

    def test_reject_empty_secret_and_public_directory(self):
        self.path.mkdir(mode=0o700)
        bad = self.path / self.module.SECRETS[0]
        bad.touch(mode=0o600)
        with self.assertRaises(ValueError):
            self.module.prepare_secrets(self.path)
        bad.write_text('existing')
        self.path.chmod(0o755)
        with self.assertRaises(ValueError):
            self.module.prepare_secrets(self.path)

if __name__ == '__main__':
    unittest.main()
