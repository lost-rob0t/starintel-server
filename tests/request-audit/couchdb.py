"""Destructive integration test restricted to a fresh, disposable loopback test DB."""
import base64
import json
import os
import sys
import uuid
from urllib.error import HTTPError
from urllib.request import Request, urlopen
from test_request_audit import audit, fixture

base = 'http://127.0.0.1:5984'
password = os.environ['REQUEST_AUDIT_TEST_ADMIN_PASSWORD']
suffix = uuid.uuid4().hex[:12]
db = 'test_request_audit_' + suffix
admin = audit.RequestAudit(f'{base}/{db}', 'tenant-a', 'admin', password)


def raw(method, path, data=None, user='admin', secret=password):
    encoded = None if data is None else json.dumps(data).encode()
    auth = base64.b64encode(f'{user}:{secret}'.encode()).decode()
    req = Request(base + path, data=encoded, method=method,
                  headers={'Authorization':'Basic '+auth, 'Content-Type':'application/json'})
    try:
        with urlopen(req, timeout=10) as response:
            body=response.read(1024*1024)
            return response.status, json.loads(body) if body else None
    except HTTPError as exc:
        return exc.code, None


admin.initialize()
admin.initialize()  # exact idempotent provisioning, without broadening permissions
assert raw('PUT', '/_users')[0] in (201, 412)
accounts = {}
for role, tenant in [('writer','tenant-a'), ('reader','tenant-a'), ('other','tenant-b')]:
    name=f'request-audit-{role}-{suffix}'
    roles=[f'request-audit-{"writer" if role in ["writer","other"] else "reader"}:{tenant}']
    status,_ = raw('PUT', '/_users/org.couchdb.user:'+name,
                   dict(name=name, password='disposable-test-password', roles=roles, type='user'))
    assert status == 201
    accounts[role] = audit.RequestAudit(f'{base}/{db}', tenant, name, 'disposable-test-password')

writer, reader = accounts['writer'], accounts['reader']
a=fixture(local_signals=['identity-linking'], flags=[], rule_ids=[])
b=fixture(event_id='01ARZ3NDEKTSV4RRFFQ69G5FAW', local_signals=['sensitive-details'], flags=[], rule_ids=[])
c=fixture(event_id='01ARZ3NDEKTSV4RRFFQ69G5FAX')
for doc in [a,b,c]:
    assert writer.append(doc) == writer.append(doc)
assert len(list(reader.history())) == 3
assert len(list(reader.history(flag='potential-doxxing-setup'))) == 1
assert sum(row['value'] for row in reader.aggregate('counts')) == 1
assert sum(row['value'] for row in reader.aggregate('decisions')) == 3
rows=reader.aggregate('signals_by_subject_hour')
assert len(rows)==1 and rows[0]['value']['events']==3
assert audit.setup_flags(rows[0]['value']) == ['potential-doxxing-setup']

checks = [
    lambda: reader.append(fixture(event_id='01ARZ3NDEKTSV4RRFFQ69G5FAY')),
    lambda: accounts['other'].request('GET','/'+a['_id']),
    lambda: writer.request('PUT','/_design/evil',data={'_id':'_design/evil','views':{}}),
    lambda: writer.request('PUT','/audit-'+'f'*64,data=dict(a,raw_request='do not persist')),
]
old=writer.request('GET','/'+a['_id'])
checks += [lambda: writer.request('PUT','/'+a['_id'],data=dict(old,decision='would-refuse')),
           lambda: writer.request('DELETE','/'+a['_id'],params={'rev':old['_rev']})]
for check in checks:
    try:
        check()
    except audit.AuditError as exc:
        assert exc.status in (401,403), exc.status
    else:
        raise AssertionError('forbidden operation unexpectedly accepted')

status,_=raw('GET',f'/{db}/'+a['_id'],user='anonymous',secret='invalid')
assert status in (401,403)
print('CouchDB integration passed: provision, replay, roles, append-only, history, _count, _sum, staged review')
