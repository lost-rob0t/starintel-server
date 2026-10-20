#!/usr/bin/env python3
"""Bounded synthetic ingestion load evidence for an already-running local stack."""
import argparse
import concurrent.futures
import json
import math
import os
import pathlib
import platform
import re
import subprocess
import threading
import time
from urllib.parse import urlsplit


def percentile95(values):
    """Nearest-rank p95; no fabricated zero for an empty completion set."""
    return sorted(values)[math.ceil(len(values) * .95) - 1] if values else None


def memory_bytes(value):
    match = re.fullmatch(r'([0-9]+(?:\.[0-9]+)?)\s*(B|kB|KB|MB|GB|TB|KiB|MiB|GiB|TiB)', value.split('/')[0].strip())
    if not match:
        raise ValueError('invalid Docker memory sample')
    units = {'B': 1, 'kB': 1000, 'KB': 1000, 'MB': 1000**2, 'GB': 1000**3,
             'TB': 1000**4, 'KiB': 1024, 'MiB': 1024**2, 'GiB': 1024**3, 'TiB': 1024**4}
    return int(float(match[1]) * units[match[2]])


def command(args):
    return subprocess.run(args, check=True, capture_output=True, text=True, timeout=15).stdout


def stack_sampler(project, compose_file):
    compose = ['docker', 'compose', '-f', str(compose_file), '-p', project]

    def sample():
        ids = command(compose + ['ps', '-q', 'star-server', 'couchdb', 'rabbitmq']).split()
        if len(ids) != 3:
            raise ValueError('three running core containers required')
        memory = {}
        for line in command(['docker', 'stats', '--no-stream', '--format', '{{json .}}', *ids]).splitlines():
            item = json.loads(line)
            memory[item['Name']] = memory_bytes(item['MemUsage'])
        if len(memory) != 3:
            raise ValueError('missing container memory')
        queues = json.loads(command(compose + ['exec', '-T', 'rabbitmq', 'rabbitmqctl',
                                              'list_queues', '--formatter', 'json',
                                              'name', 'messages_ready', 'messages_unacknowledged']))
        if not isinstance(queues, list) or not queues:
            raise ValueError('missing RabbitMQ queue samples')
        for queue in queues:
            for field in ('messages_ready', 'messages_unacknowledged'):
                if type(queue.get(field)) is not int or queue[field] < 0:
                    raise ValueError('invalid queue counter')
        return {'memory_bytes': memory, 'queues': queues,
                'messages_ready': sum(q['messages_ready'] for q in queues),
                'messages_unacknowledged': sum(q['messages_unacknowledged'] for q in queues)}
    return sample


def stack_identity(config, project, compose_file):
    """Bind fixture endpoints to the sampled local Compose project."""
    if config.get('rabbit_vhost', '/') != '/':
        raise ValueError('only the default RabbitMQ virtual host is supported')
    local_hosts = {'127.0.0.1', 'localhost', '::1'}
    api = urlsplit(config.get('api_url', 'http://127.0.0.1:5000'))
    if (config.get('rabbit_host', '127.0.0.1') not in local_hosts
            or api.hostname not in local_hosts or api.scheme != 'http'
            or api.username is not None or api.password is not None
            or api.path not in ('', '/') or api.query or api.fragment):
        raise ValueError('fixture must use local mapped stack endpoints')
    compose = ['docker', 'compose', '-f', str(compose_file), '-p', project]
    for service, internal_port, requested_port in (
            ('rabbitmq', 5672, int(config.get('rabbit_port', 5672))),
            ('star-server', 5000, api.port or 80)):
        mappings = command(compose + ['port', service, str(internal_port)]).splitlines()
        ports = {int(mapping.rsplit(':', 1)[1]) for mapping in mappings if mapping.strip()}
        if requested_port not in ports:
            raise ValueError('fixture endpoint does not match sampled stack')
    images = {}
    template = ('{"id":{{json .Id}},"repo_digests":{{json .RepoDigests}},'
                '"source_revision":{{if .Config.Labels}}'
                '{{json (index .Config.Labels "org.opencontainers.image.revision")}}'
                '{{else}}null{{end}}}')
    for service in ('star-server', 'couchdb', 'rabbitmq'):
        ids = command(compose + ['ps', '-q', service]).split()
        if len(ids) != 1:
            raise ValueError('exactly one running container per core service required')
        image_id = command(['docker', 'inspect', '--format', '{{.Image}}', ids[0]]).strip()
        images[service] = json.loads(command(['docker', 'image', 'inspect', '--format', template, image_id]))
        if images[service].get('id') != image_id or not image_id.startswith('sha256:'):
            raise ValueError('running image identity could not be verified')
    return images


def run_load(fixture, sampler, *, count, concurrency, duration, sample_interval, drain_timeout=30):
    """Stop submitting at count or duration; drain all bounded fixture calls."""
    if not 1 <= count <= 1000000 or not 1 <= concurrency <= 128:
        raise ValueError('count must be 1..1000000; concurrency 1..128')
    if not math.isfinite(duration) or not 0 < duration <= 86400:
        raise ValueError('duration must be >0 and <=86400 seconds')
    if not math.isfinite(sample_interval) or not 0 < sample_interval <= 60:
        raise ValueError('sample interval must be >0 and <=60 seconds')
    if not math.isfinite(drain_timeout) or not 0 <= drain_timeout <= 300:
        raise ValueError('drain timeout must be 0..300 seconds')
    start = time.monotonic()
    stop = threading.Event()
    samples, sample_errors, failures, latencies = [], [], [], []

    def take_sample():
        try:
            sample = {'elapsed_seconds': time.monotonic() - start, **sampler()}
            for field in ('messages_ready', 'messages_unacknowledged'):
                if type(sample.get(field)) is not int or sample[field] < 0:
                    raise ValueError('missing or invalid queue totals')
            samples.append(sample)
            return sample
        except Exception as exc:
            sample_errors.append({'elapsed_seconds': time.monotonic() - start,
                                  'error_type': type(exc).__name__})

    def sample_loop():
        while not stop.wait(sample_interval):
            take_sample()

    def invoke(index):
        before = time.monotonic()
        result = fixture(index)
        if result.get('canonical_equal') is not True or result.get('publisher_confirmed') is not True:
            raise ValueError('fixture did not verify confirmed publication and canonical retrieval')
        return (time.monotonic() - before) * 1000

    take_sample()
    thread = threading.Thread(target=sample_loop, daemon=True)
    thread.start()
    submitted = 0
    submission_end = time.monotonic()
    try:
        with concurrent.futures.ThreadPoolExecutor(max_workers=concurrency) as executor:
            pending = {}
            while True:
                while len(pending) < concurrency and submitted < count and time.monotonic() - start < duration:
                    pending[executor.submit(invoke, submitted)] = submitted
                    submitted += 1
                    submission_end = time.monotonic()
                if not pending:
                    break
                done, _ = concurrent.futures.wait(pending, return_when=concurrent.futures.FIRST_COMPLETED)
                for future in done:
                    index = pending.pop(future)
                    try:
                        latencies.append(future.result())
                    except Exception as exc:
                        # Exception messages may contain credentials, URLs, or document bodies.
                        failures.append({'index': index, 'error_type': type(exc).__name__})
    finally:
        stop.set()
        thread.join()
    drain_start = time.monotonic()
    queue_drained = False
    while True:
        sample = take_sample()
        if sample is None:
            break
        if sample['messages_ready'] == 0 and sample['messages_unacknowledged'] == 0:
            queue_drained = True
            break
        remaining = drain_timeout - (time.monotonic() - drain_start)
        if remaining <= 0:
            break
        time.sleep(min(sample_interval, remaining))
    drain_elapsed = time.monotonic() - drain_start
    elapsed = time.monotonic() - start
    active = submission_end - start
    return {'passed': submitted > 0 and len(latencies) == submitted and not failures and not sample_errors and queue_drained,
            'evidence_class': 'sustained-load' if active >= 3600 else 'smoke',
            'queue_drained': queue_drained, 'drain_elapsed_seconds': drain_elapsed,
            'requested_count': count, 'submitted': submitted, 'completed': len(latencies),
            'count_limit_reached': submitted == count, 'elapsed_seconds': elapsed,
            'submission_window_seconds': active, 'completion_p95_ms': percentile95(latencies),
            'completed_per_second': len(latencies) / elapsed,
            'completion_latency_ms': latencies, 'failures': failures,
            'samples': samples, 'sample_errors': sample_errors}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--config', required=True, type=pathlib.Path)
    parser.add_argument('--project', required=True, help='existing isolated Docker Compose project')
    parser.add_argument('--compose-file', type=pathlib.Path, default=pathlib.Path(__file__).resolve().parents[1] / 'docker-compose.yml')
    parser.add_argument('--count', type=int, default=100)
    parser.add_argument('--concurrency', type=int, default=1)
    parser.add_argument('--duration', type=float, default=60)
    parser.add_argument('--sample-interval', type=float, default=5)
    parser.add_argument('--drain-timeout', type=float, default=30)
    parser.add_argument('--output', required=True, type=pathlib.Path)
    args = parser.parse_args()
    try:
        from readiness_ingest import run_fixture
        config = json.loads(args.config.read_text())
        images = stack_identity(config, args.project, args.compose_file)
        report = run_load(lambda index: run_fixture(config, index),
                          stack_sampler(args.project, args.compose_file),
                          count=args.count, concurrency=args.concurrency, duration=args.duration,
                          sample_interval=args.sample_interval, drain_timeout=args.drain_timeout)
        report['settings'] = {'count': args.count, 'concurrency': args.concurrency,
                              'duration_seconds': args.duration, 'sample_interval_seconds': args.sample_interval,
                              'drain_timeout_seconds': args.drain_timeout}
        root = pathlib.Path(__file__).resolve().parents[1]
        report['running_images'] = images
        report['harness_revision'] = command(['git', '-C', str(root), 'rev-parse', 'HEAD']).strip()
        report['worktree_dirty'] = bool(command(['git', '-C', str(root), 'status', '--porcelain']).strip())
        report['machine'] = {'system': platform.system(), 'release': platform.release(),
                             'architecture': platform.machine(), 'logical_cpus': os.cpu_count(),
                             'physical_memory_bytes': os.sysconf('SC_PAGE_SIZE') * os.sysconf('SC_PHYS_PAGES')}
        report['limitations'] = ['Local synthetic fixture only; not a production capacity certification.',
                                'Closed-loop bounded concurrency; p95 is publication-through-retrieval completion latency.',
                                'Docker memory follows docker stats accounting; interval sampling can miss peaks.',
                                'Duration bounds submission, with bounded fixture completion and sampling drain afterward.',
                                'Harness revision does not prove running server source; image source labels may be absent or untrusted.']
        # Exclusive creation prevents clobbering earlier evidence or following a symlink.
        fd = os.open(args.output, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
        with os.fdopen(fd, 'w') as output:
            json.dump(report, output, indent=2)
            output.write('\n')
        print(json.dumps({'passed': report['passed'], 'completed': report['completed'],
                          'submitted': report['submitted'], 'evidence_class': report['evidence_class']}))
        return 0 if report['passed'] else 1
    except Exception as exc:
        print(json.dumps({'passed': False, 'error_type': type(exc).__name__}))
        return 1


if __name__ == '__main__':
    raise SystemExit(main())
