#!/usr/bin/env python3
"""Synthetic confirmed Rabbit ingress -> repository Common Lisp client proof."""
import argparse
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
import time


class FixtureError(RuntimeError):
    """Sanitized operational failure; never include credentials or payloads."""


def fixture_document(run_id, index, dataset='readiness'):
    if not isinstance(run_id, str) or not re.fullmatch(r'[a-zA-Z0-9_-]{1,64}', run_id):
        raise ValueError('run_id must be 1..64 ASCII letters, digits, dash or underscore')
    if type(index) is not int or not 0 <= index <= 100000000:
        raise ValueError('index outside fixture bounds')
    if not isinstance(dataset, str) or not re.fullmatch(r'[a-zA-Z0-9_-]{1,64}', dataset):
        raise ValueError('invalid synthetic dataset')
    return {'id': f'readiness:{run_id}:{index}', 'dataset': dataset,
            'dtype': 'message', 'schemaVersion': '0.10.1',
            'message': f'synthetic readiness fixture {index}', 'platform': 'synthetic'}


def canonical_equal(expected, actual):
    return isinstance(actual, dict) and expected == {k: v for k, v in actual.items() if k != 'rev'}


def _secret(path):
    p = Path(path)
    if p.stat().st_mode & 0o077:
        raise FixtureError('credential file must be owner-only')
    value = p.read_text().rstrip('\r\n')
    if not value or len(value) > 16384:
        raise FixtureError('credential file is empty or too large')
    return value


def publish_document(config, document):
    """Only returns after broker confirmation; mandatory rejects unroutable work."""
    try:
        import pika
        timeout = float(config.get('timeout_seconds', 30))
        connection = pika.BlockingConnection(pika.ConnectionParameters(
            host=config.get('rabbit_host', '127.0.0.1'), port=int(config.get('rabbit_port', 5672)),
            virtual_host=config.get('rabbit_vhost', '/'),
            credentials=pika.PlainCredentials(config.get('rabbit_user', 'guest'), _secret(config['rabbit_password_file'])),
            connection_attempts=1, socket_timeout=timeout, stack_timeout=timeout + 1,
            blocked_connection_timeout=timeout, heartbeat=30))
        try:
            channel = connection.channel()
            channel.confirm_delivery()
            channel.basic_publish(exchange='documents', routing_key='documents.ingest.message',
                                  body=json.dumps(document, separators=(',', ':')).encode(), mandatory=True,
                                  properties=pika.BasicProperties(content_type='application/json', delivery_mode=2,
                                                                  message_id=document['id']))
        finally:
            connection.close()
    except Exception:
        raise FixtureError('confirmed Rabbit publish failed; inspect private service diagnostics') from None


# This is an adapter to the repository-supported client, not a second HTTP client.
CLIENT_SCRIPT = '''(require :asdf)
(asdf:load-system :starintel-gserver-client)
(handler-case
    (let* ((client (star.api.client:client-with-api-key
                    (star.api.client:make-star-client
                     :base-url (uiop:getenv "READINESS_API_URL")
                     :default-timeout-ms (parse-integer (uiop:getenv "READINESS_TIMEOUT_MS")))
                    (string-right-trim '(#\\Return #\\Newline)
                     (uiop:read-file-string (uiop:getenv "READINESS_KEY_FILE"))))))
      (multiple-value-bind (value response)
          (star.api.client:call-operation client "documents.get"
           :path-parameters (list (cons "id" (uiop:getenv "READINESS_DOCUMENT_ID"))))
        (declare (ignore value))
        (with-open-file (out (uiop:getenv "READINESS_RESULT") :direction :output :if-exists :supersede)
          (write-string (star.api.client:client-response-body response) out))))
  (star.api.client:client-http-error (e)
    (if (eql (star.api.client:client-http-error-status e) 404)
        (uiop:quit 44)
        (uiop:quit 45)))
  (error () (uiop:quit 46)))
'''


def read_document(config, document_id, timeout):
    _secret(config['api_key_file'])  # enforce owner-only file before starting client
    with tempfile.TemporaryDirectory(prefix='star-readiness-client-') as tmp:
        script, result = Path(tmp) / 'client.lisp', Path(tmp) / 'result.json'
        script.write_text(CLIENT_SCRIPT)
        env = dict(os.environ, READINESS_API_URL=config.get('api_url', 'http://127.0.0.1:5000'),
                   READINESS_KEY_FILE=str(Path(config['api_key_file']).resolve()),
                   READINESS_DOCUMENT_ID=document_id, READINESS_RESULT=str(result),
                   READINESS_TIMEOUT_MS=str(max(1, int(timeout * 1000))))
        try:
            completed = subprocess.run([*config.get('client_command', ['sbcl', '--script']), str(script)],
                                       env=env, timeout=timeout, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        except (OSError, subprocess.TimeoutExpired):
            raise FixtureError('supported client unavailable or deadline exceeded') from None
        if completed.returncode == 44:
            return None
        if completed.returncode or not result.exists() or result.stat().st_size > 1048576:
            raise FixtureError('supported client retrieval failed')
        try:
            return json.loads(result.read_text())
        except (ValueError, OSError):
            raise FixtureError('supported client returned invalid JSON') from None


def run_fixture(config, index, *, publish=publish_document, read=read_document):
    """Return confirmed publish-to-canonical-read latency; failure never counts."""
    timeout = float(config.get('timeout_seconds', 30))
    if not 1 <= timeout <= 300:
        raise ValueError('timeout_seconds must be 1..300')
    document = fixture_document(config['run_id'], index, config.get('dataset', 'readiness'))
    started = time.monotonic()
    publish(config, document)
    while True:
        remaining = timeout - (time.monotonic() - started)
        if remaining <= 0:
            raise FixtureError('canonical read deadline exceeded')
        actual = read(config, document['id'], remaining)
        if actual is not None:
            if not canonical_equal(document, actual):
                raise FixtureError('persisted canonical document differs from fixture')
            return {'id': document['id'], 'latency_ms': (time.monotonic() - started) * 1000,
                    'canonical_equal': True, 'publisher_confirmed': True, 'revision': actual.get('rev')}
        time.sleep(min(0.1, remaining))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--config', required=True)
    parser.add_argument('--index', type=int, default=0)
    parser.add_argument('--duplicate', action='store_true', help='Repeat same confirmed mutation and require stable revision')
    args = parser.parse_args()
    try:
        config = json.loads(Path(args.config).read_text())
        first = run_fixture(config, args.index)
        if args.duplicate:
            second = run_fixture(config, args.index)
            if not first['revision'] or first['revision'] != second['revision']:
                raise FixtureError('duplicate changed or omitted persisted revision')
            first['duplicate_revision_equal'] = True
        print(json.dumps(first))
    except (FixtureError, ValueError, KeyError, OSError):
        print(json.dumps({'status':'failed', 'reason':'fixture or runtime prerequisite failed'}))
        return 1
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
