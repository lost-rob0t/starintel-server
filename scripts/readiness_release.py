#!/usr/bin/env python3
"""Check a reviewed RC evidence bundle; never run tests, publish, or create tags."""
import argparse
import hashlib
import json
import math
from pathlib import Path
import re
import subprocess

GATES = ('startup', 'ingestion', 'isolation', 'recovery', 'load', 'integration')
LOCKS = ('flake.lock', 'qlfile.lock', 'schema/starintel-schema.lock.json')
IMAGES = ('server', 'couchdb', 'rabbitmq', 'clouseau', 'valkey')


def require(condition, message):
    if not condition:
        raise ValueError(message)


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def proof(record, root):
    require(isinstance(record, dict), 'missing evidence record')
    name = record.get('path')
    require(isinstance(name, str) and name, 'missing evidence path')
    path = (root / name).resolve()
    require(path.is_relative_to(root.resolve()) and path.is_file(), 'evidence must be a file inside bundle')
    require(digest(path) == record.get('sha256'), 'evidence digest mismatch: ' + name)


def verify(data, root, expected_head, source_root=None):
    """Reject incomplete, stale or tampered evidence; authenticity needs review."""
    source_root = source_root or root
    require(re.fullmatch(r'[0-9a-f]{40}', expected_head) is not None, 'expected head must be full SHA')
    require(data.get('format') == 1 and data.get('source') == expected_head, 'wrong format or source SHA')
    for name in LOCKS:
        require(data.get('locks', {}).get(name) == digest(source_root / name), 'lock mismatch: ' + name)
    for name in IMAGES:
        value = data.get('images', {}).get(name, '')
        require(isinstance(value, str) and re.fullmatch(r'sha256:[0-9a-f]{64}', value), 'immutable image digest required: ' + name)
    gates = data.get('gates', {})
    for name in GATES:
        gate = gates.get(name, {})
        require(gate.get('source') == expected_head and gate.get('status') == 'passed', 'gate missing/stale/failed: ' + name)
        require(gate.get('execution') == 'service-backed', 'actual service-backed execution required: ' + name)
        require(isinstance(gate.get('command'), str) and gate['command'].strip(), 'command required: ' + name)
        require(type(gate.get('assertions')) is int and gate['assertions'] > 0, 'nonzero assertions required: ' + name)
        require(type(gate.get('exitCode')) is int and gate['exitCode'] == 0, 'successful exit required: ' + name)
        proof(gate.get('evidence'), root)
    load = gates['load']
    measures = load.get('measurements', {})
    for name in ('durationSeconds', 'requests', 'p95Milliseconds', 'peakMemoryBytes',
                 'peakQueueBacklog', 'finalQueueBacklog', 'errors', 'concurrency', 'documents'):
        value = measures.get(name)
        require(type(value) in (int, float) and math.isfinite(value) and value >= 0, 'missing/invalid measurement: ' + name)
    for name in ('durationSeconds', 'requests', 'peakMemoryBytes', 'concurrency', 'documents'):
        require(measures[name] > 0, 'positive measurement required: ' + name)
    require(measures['errors'] == 0 and measures['finalQueueBacklog'] == 0, 'load errors or undrained backlog')
    require(isinstance(load.get('testedLimits'), dict) and load['testedLimits'].get('description'), 'honest tested limits required')
    for name, status in (('ci', 'passed'), ('review', 'approved')):
        record = data.get(name, {})
        require(record.get('source') == expected_head and record.get('status') == status, 'latest-head ' + name + ' required')
        proof(record.get('evidence'), root)
    require(data['review'].get('blocking') is False, 'blocking review must be explicitly false')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('manifest', type=Path)
    parser.add_argument('--expected-head', required=True)
    parser.add_argument('--source-root', type=Path, default=Path.cwd())
    args = parser.parse_args()
    try:
        actual = subprocess.check_output(['git', '-C', str(args.source_root), 'rev-parse', 'HEAD'], text=True).strip()
        require(actual == args.expected_head, 'checkout does not match expected head')
        dirty = subprocess.check_output(['git', '-C', str(args.source_root), 'status', '--porcelain', '--untracked-files=normal'], text=True)
        require(not dirty.strip(), 'source checkout must be clean; keep evidence outside checkout')
        verify(json.loads(args.manifest.read_text()), args.manifest.parent, args.expected_head, args.source_root)
    except (ValueError, OSError, subprocess.CalledProcessError, TypeError, AttributeError) as error:
        parser.exit(1, 'RC evidence rejected: ' + str(error) + '\n')
    print('RC evidence structurally verified; independently review logs and live CI/reviews before tagging. No release published.')

if __name__ == '__main__':
    main()
