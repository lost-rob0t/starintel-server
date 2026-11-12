#!/usr/bin/env python3
"""Launch the existing Linux Compose stack with private runtime secrets."""
import argparse
import os
from pathlib import Path
import secrets
import stat
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
SECRETS = ('couchdb_password', 'couchdb_secret', 'erlang_cookie',
           'rabbitmq_password', 'valkey_password', 'auth_pepper',
           'auth_bootstrap_secret')


def prepare_secrets(directory):
    """Validate all existing inputs before creating missing, owner-only files."""
    directory = Path(os.path.abspath(Path(directory).expanduser()))
    if directory.is_relative_to(ROOT) or directory.is_relative_to('/nix/store'):
        raise ValueError('credentials must be outside the checkout and Nix store')
    for part in reversed((directory, *directory.parents)):
        if part.is_symlink():
            raise ValueError('credential directory must not contain symlinks')
    directory.mkdir(mode=0o700, parents=True, exist_ok=True)
    info = directory.stat()
    if (not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid()
            or stat.S_IMODE(info.st_mode) != 0o700):
        raise ValueError('credential directory must be owned by this user with mode 0700')
    # Open relative to the verified directory; never follow secret symlinks.
    dirfd = os.open(directory, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
    try:
        opened = os.fstat(dirfd)
        if ((opened.st_dev, opened.st_ino) != (info.st_dev, info.st_ino)
                or opened.st_uid != os.getuid()
                or stat.S_IMODE(opened.st_mode) != 0o700):
            raise ValueError('credential directory changed during validation')
        missing = []
        for name in SECRETS:
            try:
                fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK,
                             dir_fd=dirfd)
            except FileNotFoundError:
                missing.append(name)
                continue
            except OSError:
                raise ValueError('unsafe existing secret: ' + name) from None
            try:
                info = os.fstat(fd)
                if (not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid()
                        or stat.S_IMODE(info.st_mode) != 0o600 or info.st_nlink != 1
                        or not 1 <= info.st_size <= 4096
                        or not os.read(fd, 4096).strip()):
                    raise ValueError('existing secret must be private, regular and nonempty: ' + name)
            finally:
                os.close(fd)
        for name in missing:
            value = secrets.token_hex(32)
            if name == 'erlang_cookie':
                value = value.upper()
            fd = os.open(name, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
                         0o600, dir_fd=dirfd)
            with os.fdopen(fd, 'w') as stream:
                stream.write(value + '\n')
                stream.flush()
                os.fsync(stream.fileno())
        os.fsync(dirfd)
    finally:
        os.close(dirfd)
    return directory


def launch(directory, timeout, build_timeout):
    """Use the repository image builder and Compose health gates unchanged."""
    environment = dict(os.environ, CREDENTIALS_DIR=str(directory))
    # Keep output free of arbitrary application logs or expanded configuration.
    # A failed command is identified without printing potentially sensitive output.
    def run(command, seconds):
        subprocess.run(command, cwd=ROOT, env=environment, check=True,
                       timeout=seconds, stdout=subprocess.DEVNULL,
                       stderr=subprocess.DEVNULL)
    print('Building and loading pinned images...', flush=True)
    run(['nix', 'run', '.#load-images'], build_timeout)
    compose = ['docker', 'compose', '--project-directory', str(ROOT)]
    run(compose + ['config', '--quiet'], 30)
    print('Starting services and waiting for health...', flush=True)
    run(compose + ['up', '--detach', '--wait', '--wait-timeout', str(timeout)], timeout + 30)
    run(compose + ['exec', '-T', 'star-server', 'curl', '--fail', '--silent',
                   '--max-time', '10', 'http://localhost:5000/ready'], 15)
    print('Stack readiness passed. Persistent volumes and credentials are retained.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--credentials-dir', type=Path,
                        default=Path.home() / '.local/state/starintel/credentials')
    parser.add_argument('--timeout', type=int, default=300,
                        help='Compose readiness timeout in seconds (default: 300)')
    parser.add_argument('--build-timeout', type=int, default=1800)
    parser.add_argument('--prepare-only', action='store_true',
                        help='prepare secrets without building or starting services')
    args = parser.parse_args()
    if args.timeout <= 0 or args.build_timeout <= 0:
        parser.error('timeouts must be positive')
    try:
        directory = prepare_secrets(args.credentials_dir)
        if args.prepare_only:
            print('Runtime secrets prepared; existing secrets retained.')
        else:
            launch(directory, args.timeout, args.build_timeout)
    except subprocess.TimeoutExpired:
        print('Startup command timed out; services and volumes retained for inspection.', file=sys.stderr)
        return 1
    except subprocess.CalledProcessError as error:
        print('Startup command failed: ' + ' '.join(error.cmd) +
              ' (exit ' + str(error.returncode) + '). Services and volumes retained.', file=sys.stderr)
        return 1
    except (OSError, ValueError) as error:
        print('Startup failed: ' + str(error), file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
