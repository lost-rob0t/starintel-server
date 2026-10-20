import importlib.util
import pathlib
import unittest
from unittest import mock

SPEC = importlib.util.spec_from_file_location('readiness_load', pathlib.Path(__file__).parents[1] / 'scripts/readiness_load.py')
load = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(load)


class LoadTests(unittest.TestCase):
    def test_percentile_nearest_rank(self):
        self.assertEqual(load.percentile95(list(range(1, 101))), 95)
        self.assertIsNone(load.percentile95([]))
        self.assertEqual(load.percentile95([9]), 9)

    def test_memory_units(self):
        self.assertEqual(load.memory_bytes('1.5MiB / 2GiB'), 1572864)
        self.assertEqual(load.memory_bytes('1kB / 2GB'), 1000)
        with self.assertRaises(ValueError):
            load.memory_bytes('broken')

    def test_all_completions_counted_and_samples_required(self):
        report = load.run_load(lambda i: {'canonical_equal': True, 'publisher_confirmed': True},
                               lambda: {'memory': {}, 'queues': []},
                               count=9, concurrency=3, duration=2, sample_interval=1)
        self.assertEqual(report['completed'], 9)
        self.assertEqual(report['submitted'], 9)
        self.assertTrue(report['passed'])
        self.assertGreaterEqual(len(report['samples']), 2)
        self.assertEqual(report['evidence_class'], 'smoke')

    def test_failures_and_incomplete_verifications_fail(self):
        def fixture(i):
            if i == 0:
                raise RuntimeError('SECRET MUST NOT APPEAR')
            return {'canonical_equal': False, 'publisher_confirmed': True}
        report = load.run_load(fixture, lambda: {}, count=2, concurrency=1,
                               duration=2, sample_interval=1)
        self.assertFalse(report['passed'])
        self.assertEqual(report['completed'], 0)
        self.assertEqual(len(report['failures']), 2)
        self.assertNotIn('SECRET', str(report))

    def test_sampler_failure_fails_run(self):
        def sampler():
            raise RuntimeError('SECRET')
        report = load.run_load(lambda i: {'canonical_equal': True, 'publisher_confirmed': True},
                               sampler, count=1, concurrency=1, duration=2, sample_interval=1)
        self.assertFalse(report['passed'])
        self.assertTrue(report['sample_errors'])
        self.assertNotIn('SECRET', str(report))

    def test_sampler_aggregates_real_command_formats(self):
        outputs = ['a\nb\nc\n',
                   '{"Name":"server","MemUsage":"1MiB / 2GiB"}\n'
                   '{"Name":"db","MemUsage":"2MiB / 2GiB"}\n'
                   '{"Name":"rabbit","MemUsage":"3MiB / 2GiB"}\n',
                   '[{"name":"docs","messages_ready":7,"messages_unacknowledged":2}]']
        with mock.patch.object(load, 'command', side_effect=outputs):
            sample = load.stack_sampler('test', 'docker-compose.yml')()
        self.assertEqual(sample['messages_ready'], 7)
        self.assertEqual(sample['messages_unacknowledged'], 2)
        self.assertEqual(sample['memory_bytes']['server'], 1048576)

    def test_sampler_rejects_missing_containers(self):
        with mock.patch.object(load, 'command', return_value='a\nb\n'):
            with self.assertRaises(ValueError):
                load.stack_sampler('test', 'docker-compose.yml')()

    def test_invalid_limits_rejected(self):
        for overrides in ({'count': 0}, {'concurrency': 0}, {'duration': 0}, {'sample_interval': 0}):
            args = dict(count=1, concurrency=1, duration=1, sample_interval=1)
            args.update(overrides)
            with self.assertRaises(ValueError):
                load.run_load(lambda i: {}, lambda: {}, **args)


if __name__ == '__main__':
    unittest.main()
