"""Focused declarative actor contract; requires PyYAML, no Docker daemon."""
import os
from pathlib import Path
import unittest
import yaml

ROOT = Path(os.environ.get('DEPLOYMENT_TEST_ROOT', Path(__file__).resolve().parents[1]))


class ShardDeploymentTests(unittest.TestCase):
    def setUp(self):
        self.compose = yaml.safe_load((ROOT / 'docker-compose.yml').read_text())

    def actor(self):
        self.assertTrue('prolog-shard-actor' in self.compose['services'], 'shard actor service is missing')
        return self.compose['services']['prolog-shard-actor']

    def test_private_hardened_actor(self):
        actor = self.actor()
        self.assertEqual(['actors'], actor['profiles'])
        self.assertEqual('none', actor['network_mode'])
        self.assertNotIn('ports', actor)
        self.assertNotIn('secrets', actor)
        self.assertEqual('10001:10001', actor['user'])
        self.assertIs(True, actor['read_only'])
        self.assertEqual(['/tmp'], actor['tmpfs'])
        self.assertEqual(['ALL'], actor['cap_drop'])
        self.assertEqual(['no-new-privileges:true'], actor['security_opt'])

    def test_image_storage_command_and_bounded_health(self):
        actor = self.actor()
        self.assertEqual('starintel/prolog-shard-actor:0.1.0', actor['image'])
        self.assertEqual(['starintel-prolog-shards', 'serve'], actor['command'])
        self.assertEqual(['prolog-shard-data:/var/lib/starintel-prolog'], actor['volumes'])
        self.assertIn('prolog-shard-data', self.compose['volumes'])
        self.assertEqual(['CMD', 'starintel-prolog-shards', 'healthcheck'], actor['healthcheck']['test'])
        self.assertEqual('10s', actor['healthcheck']['timeout'])
        self.assertEqual(3, actor['healthcheck']['retries'])

    def test_absent_scope_does_not_block_compose_interpolation(self):
        env = self.actor()['environment']
        self.assertEqual('${STARINTEL_TENANT:-}', env['STARINTEL_TENANT'])
        self.assertEqual('${STARINTEL_DATASET:-}', env['STARINTEL_DATASET'])
        self.assertEqual('/var/lib/starintel-prolog/shards', env['STARINTEL_SHARD_ROOT'])
        self.assertEqual('/var/lib/starintel-prolog/spool', env['STARINTEL_SPOOL_ROOT'])

    def test_make_target_uses_actor_package(self):
        makefile = (ROOT / 'Makefile').read_text()
        self.assertTrue('\nprolog-shard-image:\n' in makefile, 'shard image target is missing')
        self.assertIn('$(PROLOG_ACTOR_FLAKE)#prolog-shard-image --no-link --print-out-paths', makefile)
        self.assertIn('github:lost-rob0t/starintel-pro-actors/9ef1caf119fc465f7da7f979775b6bf29a21a148', makefile)
        self.assertIn('docker load < "$$image_path"', makefile)


if __name__ == '__main__':
    unittest.main()
