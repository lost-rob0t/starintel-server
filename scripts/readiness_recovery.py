#!/usr/bin/env python3
"""Synthetic, quiesced CouchDB recovery drill; never restores over a database."""
import argparse
import base64
import hashlib
import json
import os
import pathlib
import re
import sys
import urllib.error
import urllib.parse
import urllib.request


class RecoveryError(Exception):
    pass


def database_name(value):
    if not re.fullmatch(r'readiness-[a-z0-9-]+', value):
        raise RecoveryError('only readiness- synthetic databases are allowed')
    return value


def normalize(documents):
    result = []
    seen = set()
    for original in documents:
        doc = json.loads(json.dumps(original))
        identifier = doc.get('_id')
        if not isinstance(identifier, str) or not identifier or identifier in seen:
            raise RecoveryError('missing or duplicate document identity')
        seen.add(identifier)
        if identifier.startswith('_design/'):
            raise RecoveryError('design documents are outside synthetic drill scope')
        for field in ('_rev', '_revisions', '_conflicts', '_deleted_conflicts'):
            doc.pop(field, None)
        for attachment in doc.get('_attachments', {}).values():
            if 'data' not in attachment:
                raise RecoveryError('attachment body missing')
            try:
                base64.b64decode(attachment['data'], validate=True)
            except (ValueError, TypeError):
                raise RecoveryError('invalid attachment encoding') from None
            # CouchDB regenerates these transport fields on restore.
            for field in ('revpos', 'digest', 'length', 'stub', 'encoding', 'encoded_length'):
                attachment.pop(field, None)
        result.append(doc)
    return sorted(result, key=lambda doc: doc['_id'])


def digest(documents):
    return hashlib.sha256(json.dumps(normalize(documents), sort_keys=True,
                                    separators=(',', ':')).encode()).hexdigest()


def snapshot(request, database):
    status, body = request('GET', database_name(database) + '/_all_docs?include_docs=true&attachments=true')
    if status != 200 or not isinstance(body, dict) or not isinstance(body.get('rows'), list):
        raise RecoveryError('database snapshot failed')
    try:
        return normalize([row['doc'] for row in body['rows']])
    except (KeyError, TypeError):
        raise RecoveryError('incomplete database snapshot') from None


def validate_bulk(response, documents):
    expected = {doc['_id'] for doc in documents}
    if not isinstance(response, list) or len(response) != len(expected):
        raise RecoveryError('bulk restore result count mismatch')
    seen = set()
    for item in response:
        if not isinstance(item, dict) or item.get('ok') is not True or 'error' in item:
            raise RecoveryError('bulk restore contains failed item')
        identifier = item.get('id')
        if identifier not in expected or identifier in seen:
            raise RecoveryError('bulk restore identity mismatch')
        seen.add(identifier)


def restore(request, database, documents):
    database = database_name(database)
    documents = normalize(documents)
    status, body = request('PUT', database)
    if status != 201 or not isinstance(body, dict) or body.get('ok') is not True:
        raise RecoveryError('fresh database creation failed; existing database will not be changed')
    status, body = request('POST', database + '/_bulk_docs', {'docs': documents})
    if status != 201:
        raise RecoveryError('bulk restore request failed')
    validate_bulk(body, documents)
    if digest(snapshot(request, database)) != digest(documents):
        raise RecoveryError('restored document or attachment contents differ')


def write_backup(path, documents):
    documents = normalize(documents)
    descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    with os.fdopen(descriptor, 'w') as stream:
        json.dump({'format': 1, 'sha256': digest(documents), 'documents': documents}, stream)
        stream.flush()
        os.fsync(stream.fileno())


def read_backup(path):
    body = json.loads(pathlib.Path(path).read_text())
    if body.get('format') != 1 or digest(body['documents']) != body.get('sha256'):
        raise RecoveryError('invalid backup format or checksum')
    return normalize(body['documents'])


def transport(url, username, password_file):
    parsed = urllib.parse.urlsplit(url)
    if parsed.scheme not in ('http', 'https') or parsed.username or parsed.password or parsed.query or parsed.fragment:
        raise RecoveryError('invalid endpoint; credentials must come from a file')
    if parsed.hostname not in ('localhost', '127.0.0.1', '::1'):
        raise RecoveryError('recovery drill requires a local endpoint')
    password = pathlib.Path(password_file).read_text().strip()
    if not password:
        raise RecoveryError('empty password file')
    authorization = 'Basic ' + base64.b64encode((username + ':' + password).encode()).decode()
    # Never forward authentication to a redirect target.
    class NoRedirect(urllib.request.HTTPRedirectHandler):
        def redirect_request(self, req, fp, code, msg, headers, newurl):
            return None
    opener = urllib.request.build_opener(NoRedirect)
    def request(method, path, body=None):
        data = None if body is None else json.dumps(body).encode()
        req = urllib.request.Request(url.rstrip('/') + '/' + path, data=data, method=method,
                                     headers={'Authorization': authorization, 'Content-Type': 'application/json'})
        try:
            with opener.open(req, timeout=30) as response:
                return response.status, json.load(response)
        except urllib.error.HTTPError as error:
            # Response bodies may contain credentials or document data.
            return error.code, None
        except (OSError, ValueError):
            raise RecoveryError('database transport failed') from None
    return request


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('operation', choices=['backup', 'restore'])
    parser.add_argument('--url', required=True)
    parser.add_argument('--database', required=True, type=database_name)
    parser.add_argument('--username', default='admin')
    parser.add_argument('--password-file', required=True)
    parser.add_argument('--backup-file', required=True)
    args = parser.parse_args()
    try:
        request = transport(args.url, args.username, args.password_file)
        if args.operation == 'backup':
            documents = snapshot(request, args.database)
            write_backup(args.backup_file, documents)
        else:
            documents = read_backup(args.backup_file)
            restore(request, args.database, documents)
        print(json.dumps({'operation': args.operation, 'records': len(documents),
                          'sha256': digest(documents), 'status': 'passed'}))
    except (RecoveryError, OSError, ValueError, KeyError, TypeError):
        print('recovery failed; no service proof recorded; inspect local state before retry', file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
