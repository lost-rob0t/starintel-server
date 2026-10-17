#!/usr/bin/env python3
"""Synthetic Linux CI acceptance; does not establish Android/PRoot support."""
import argparse
import json
import os
from pathlib import Path
import secrets
import signal
import subprocess
import tempfile
import time
import urllib.parse

ROOT = Path(__file__).resolve().parents[1]
COUCH_IMAGE = 'couchdb@sha256:09c35f535e4df3c4dc789aca8ce0b4cfe6e56619edb873a041d35dec68df9e18'
RABBIT_IMAGE = 'rabbitmq@sha256:36703420d34df0701f441730ef0ee5a28efc27611ca5b4b48a2bf6571c9b2854'
SERVER_IMAGE = 'starintel/server:0.1.0'


class HTTPStatusError(RuntimeError):
    def __init__(self, code):
        self.code = code
        super().__init__('HTTP request failed: ' + str(code))


def assert_document(actual, expected):
    if not isinstance(actual, dict) or set(actual) - set(expected) - {'rev'}:
        raise RuntimeError('readback is not the canonical fixture plus optional revision')
    if {key: actual.get(key) for key in expected} != expected:
        raise RuntimeError('canonical fixture identity/content changed')


def redacted(data, values, limit=262144):
    for value in values:
        data = data.replace(value.encode(), b'[REDACTED]')
    return data[-limit:]


class Acceptance:
    def __init__(self, evidence, private):
        self.evidence, self.private = evidence, private
        self.prefix = 'ci341-' + secrets.token_hex(6)
        self.containers = []
        self.network = None
        self.volume = None
        self.secret_values = [secrets.token_urlsafe(32) for _ in range(4)]
        self.couch_password, self.rabbit_password, self.pepper, self.bootstrap = self.secret_values
        self.result = {'platform': 'Linux Docker CI; not Android/PRoot',
                       'source': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
                       'stage': 'initialize', 'status': 'running', 'serverHttp': 'not-run',
                       'restartReadback': 'not-run', 'actor': 'not-run (subsequent existing unit step)'}
        self.save()

    def save(self):
        (self.evidence / 'persistence.json').write_text(json.dumps(self.result, indent=2) + '\n')

    def stage(self, name):
        self.result['stage'] = name
        self.result.pop('lastProbeError', None)
        self.save()
        print('CI341 stage=' + name, flush=True)

    def docker(self, *args, timeout=40, check=True, input=None):
        result = subprocess.run(['docker', *map(str, args)], capture_output=True, timeout=timeout, input=input)
        if check and result.returncode:
            raise RuntimeError('Docker ' + args[0] + ' failed with exit ' + str(result.returncode))
        return result

    def env_file(self, name, values):
        path = self.private / name
        path.write_text(''.join(key + '=' + str(value) + '\n' for key, value in values.items()))
        path.chmod(0o600)
        return path

    def run_container(self, role, image, env, extra=(), command=()):
        name = self.prefix + '-' + role
        # Register the known owned name before the interruptible Docker request.
        self.containers.append(name)
        self.docker('run', '--detach', '--name', name, '--network', self.network,
                    '--network-alias', role, '--label', 'ci341.run=' + self.prefix, '--cpus', '2', '--memory', '1g', '--pids-limit', '512',
                    '--log-opt', 'max-size=1m', '--log-opt', 'max-file=2',
                    '--env-file', self.env_file(role + '.env', env), *extra, image, *command, timeout=90)
        return name

    def request(self, container, port, path, method='GET', value=None, headers=None, host='127.0.0.1'):
        # Keep the network internal. Curl speaks actual container-loopback HTTP;
        # credentials/body stay in stdin, never Docker/curl process arguments.
        if host not in ('127.0.0.1', 'couchdb'):
            raise RuntimeError('HTTP host must be the owned local service')
        if not path.startswith('/') or path.startswith('//'):
            raise RuntimeError('HTTP path must be local and relative')
        lines = ['url = ' + json.dumps('http://' + host + ':' + str(port) + path),
                 'request = ' + json.dumps(method)]
        for key, item in {'Content-Type': 'application/json', **(headers or {})}.items():
            if any(ord(char) < 32 or ord(char) == 127 for char in key + item):
                raise RuntimeError('invalid HTTP header')
            lines.append('header = ' + json.dumps(key + ': ' + item))
        if value is not None:
            lines.append('data = ' + json.dumps(json.dumps(value)))
        result = self.docker('exec', '-i', container, 'curl', '--disable', '--silent',
            '--show-error', '--globoff', '--proto', '=http', '--noproxy', '*',
            '--location', '--max-redirs', '0', '--max-time', '3',
            '--write-out', '\n%{http_code}', '--config', '-',
            input=('\n'.join(lines) + '\n').encode(), check=False, timeout=10)
        body, _, code = result.stdout.rpartition(b'\n')
        if not code.isdigit() or code == b'000':
            raise OSError('internal HTTP transport unavailable: ' + redacted(result.stderr, self.secret_values, 500).decode('utf-8', 'replace'))
        if not 200 <= int(code) < 300:
            raise HTTPStatusError(int(code))
        if result.returncode:
            raise OSError('container-loopback curl failed')
        return json.loads(body)

    def wait(self, operation, seconds=60):
        deadline = time.monotonic() + seconds
        while True:
            try:
                value = operation()
                if value is not False:
                    return value
            except HTTPStatusError as error:
                if error.code not in (404, 429, 500, 502, 503, 504):
                    raise RuntimeError('HTTP authorization/request failed: ' + str(error.code)) from None
            except OSError as error:
                self.result['lastProbeError'] = redacted(str(error).encode(), self.secret_values, 500).decode('utf-8', 'replace')
            if time.monotonic() >= deadline:
                raise RuntimeError('readiness/persistence deadline exceeded at ' + self.result['stage'])
            time.sleep(1)

    def diagnostics(self):
        for container in self.containers:
            try:
                logs = self.docker('logs', '--tail', '1000', container, check=False, timeout=10)
                (self.evidence / (container.rsplit('-', 1)[-1] + '.log')).write_bytes(
                    redacted(logs.stdout + logs.stderr, self.secret_values))
            except (OSError, subprocess.TimeoutExpired):
                pass

    def remove_owned(self, kind, name):
        label = '{{index .Config.Labels "ci341.run"}}' if kind == 'container' else '{{index .Labels "ci341.run"}}'
        command = ('inspect', '--type', 'container') if kind == 'container' else (kind, 'inspect')
        info = self.docker(*command, '--format', label, name, check=False, timeout=10)
        if info.returncode:
            if b'No such' in info.stderr:
                return
            raise RuntimeError('cannot verify cleanup ownership')
        if info.stdout.decode().strip() != self.prefix:
            raise RuntimeError('refusing cleanup of an unowned ' + kind)
        command = ('rm', '--force', '--volumes', name) if kind == 'container' else (kind, 'rm', name)
        self.docker(*command, timeout=15)

    def cleanup(self):
        errors = []
        for container in reversed(self.containers):
            try:
                self.remove_owned('container', container)
            except (OSError, RuntimeError, subprocess.TimeoutExpired):
                errors.append('container cleanup failed')
        self.containers.clear()
        for kind in ('volume', 'network'):
            name = getattr(self, kind)
            if name:
                try:
                    self.remove_owned(kind, name)
                except (OSError, RuntimeError, subprocess.TimeoutExpired):
                    errors.append(kind + ' cleanup failed')
                setattr(self, kind, None)
        if errors:
            raise RuntimeError('; '.join(errors))

    def run(self, archive):
        try:
            self.stage('load-existing-server-image')
            self.docker('load', '--input', archive, timeout=120)
            for image in (COUCH_IMAGE, RABBIT_IMAGE):
                self.docker('pull', image, timeout=120)
            self.network = self.prefix + '-network'
            self.docker('network', 'create', '--internal', '--label', 'ci341.run=' + self.prefix, self.network)
            self.volume = self.prefix + '-data'
            self.docker('volume', 'create', '--label', 'ci341.run=' + self.prefix, self.volume)
            self.stage('real-dependency-readiness')
            single_node = self.private / 'single-node.ini'
            single_node.write_text('[couchdb]\nsingle_node = true\n[cluster]\nn = 1\n')
            single_node.chmod(0o644)
            couch = self.run_container('couchdb', COUCH_IMAGE, {
                'COUCHDB_USER': 'ci', 'COUCHDB_PASSWORD': self.couch_password,
                'ERL_FLAGS': '+S 2:2'}, ('--mount', 'type=volume,src=' + self.volume + ',dst=/opt/couchdb/data',
                '--mount', 'type=bind,src=' + str(single_node) + ',dst=/opt/couchdb/etc/local.d/50-ci-single.ini,readonly'))
            probe = self.run_container('probe', SERVER_IMAGE, {}, ('--entrypoint', '/bin/sleep'), ('600',))
            self.stage('couchdb-readiness')
            self.wait(lambda: self.request(probe, 5984, '/_up', host='couchdb').get('status') == 'ok')
            rabbit = self.run_container('rabbitmq', RABBIT_IMAGE, {
                'RABBITMQ_DEFAULT_USER': 'ci', 'RABBITMQ_DEFAULT_PASS': self.rabbit_password,
                'RABBITMQ_SERVER_ADDITIONAL_ERL_ARGS': '+S 2:2'})
            self.stage('rabbitmq-readiness')
            self.wait(lambda: self.docker('exec', rabbit, 'rabbitmq-diagnostics', '-q', 'ping',
                                         check=False, timeout=10).returncode == 0)
            for name, value in zip(('couchdb_password', 'rabbitmq_password', 'auth_pepper', 'auth_bootstrap_secret'), self.secret_values):
                path = self.private / name
                path.write_text(value + '\n')
                path.chmod(0o600)
            server_env = {
                'COUCHDB_HOST': 'couchdb', 'COUCHDB_USER': 'ci', 'COUCHDB_DATABASE': 'ci341',
                'STAR_AUTH_DATABASE': 'ci341-auth', 'RABBITMQ_ADDRESS': 'rabbitmq',
                'RABBITMQ_USER': 'ci', 'HTTP_API_LISTEN_ADDRESS': '0.0.0.0',
                'STAR_AUTH_MODE': 'api-key', 'STAR_AUTH_DEV_BYPASS': 'false',
                'STAR_LEASE_STORE_BACKEND': 'memory'}
            for key, name in zip(('COUCHDB_PASSWORD', 'RABBITMQ_PASSWORD', 'STAR_AUTH_PEPPER', 'STAR_AUTH_BOOTSTRAP_SECRET'),
                                 ('couchdb_password', 'rabbitmq_password', 'auth_pepper', 'auth_bootstrap_secret')):
                server_env[key + '_FILE'] = '/run/secrets/' + name
            self.stage('server-readiness')
            server = self.run_container('server', SERVER_IMAGE, server_env,
                ('--mount', 'type=bind,src=' + str(self.private) + ',dst=/run/secrets,readonly'))
            self.wait(lambda: self.request(server, 5000, '/ready').get('status') == 'ready')
            self.result['imageIds'] = {name.rsplit('-', 1)[-1]: self.docker('inspect', '--format', '{{.Image}}', name).stdout.decode().strip() for name in self.containers}
            self.stage('canonical-http-create-read')
            response = self.request(server, 5000, '/auth/bootstrap', 'POST', {'owner': 'ci341-synthetic'},
                                     {'X-Star-Bootstrap-Secret': self.bootstrap})
            token = response['api_key']
            if not isinstance(token, str) or not token.startswith('star_sk_v1_'):
                raise RuntimeError('bootstrap did not return an API credential')
            self.secret_values.append(token)
            headers = {'Authorization': 'Bearer ' + token}
            fixture = {'id': 'ci341:' + secrets.token_hex(12), 'dataset': 'ci341-synthetic',
                       'dtype': 'person', 'schemaVersion': '0.10.1', 'fname': 'Synthetic CI fixture'}
            assert_document(self.request(server, 5000, '/api/v1/documents', 'POST', fixture, headers), fixture)
            path = '/api/v1/documents/' + urllib.parse.quote(fixture['id'], safe='')
            def readback():
                actual = self.request(server, 5000, path, headers=headers)
                assert_document(actual, fixture)
                return actual
            before = self.wait(readback, seconds=45)
            self.result['serverHttp'] = 'passed'
            self.stage('restart-server-and-couchdb')
            self.docker('stop', '--time', '20', server)
            exit_code = self.docker('inspect', '--format', '{{.State.ExitCode}}', server).stdout.strip()
            if exit_code not in (b'0', b'143'):
                raise RuntimeError('server did not stop gracefully')
            self.docker('restart', '--time', '20', couch)
            self.wait(lambda: self.request(probe, 5984, '/_up', host='couchdb').get('status') == 'ok')
            self.docker('start', server)
            self.wait(lambda: self.request(server, 5000, '/ready').get('status') == 'ready')
            if readback() != before:
                raise RuntimeError('restart changed canonical document/revision')
            self.result['restartReadback'] = 'passed'
            self.result['status'] = 'passed'
            self.stage('persistence-complete')
        except BaseException as error:
            self.result['status'] = 'failed'
            self.result['failure'] = redacted(str(error).encode(), self.secret_values, 1000).decode('utf-8', 'replace')
            raise
        finally:
            self.diagnostics()
            try:
                self.cleanup()
            except BaseException:
                self.result['status'] = 'failed'
                self.result['cleanup'] = 'failed'
                raise
            finally:
                self.save()
                print(json.dumps(self.result), flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--image-archive', type=Path, required=True)
    parser.add_argument('--evidence', type=Path, required=True)
    args = parser.parse_args()
    os.umask(0o077)
    args.evidence.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='ci341-private-') as private:
        Acceptance(args.evidence, Path(private)).run(args.image_archive.resolve())


if __name__ == '__main__':
    signal.signal(signal.SIGTERM, lambda *_: (_ for _ in ()).throw(KeyboardInterrupt()))
    try:
        main()
    except (Exception, KeyboardInterrupt):
        raise SystemExit('CI341 acceptance failed; inspect sanitized stage evidence')
