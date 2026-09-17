"""Dedicated request-risk audit DB and reviewer CLI; never billing authority."""
from __future__ import annotations
import argparse
import hashlib
import json
import os
import re
import stat
import sys
from datetime import datetime
from pathlib import Path

# Reuse the transport in the parent audit PR, including redirect/size bounds.
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'audit'))
from starintel_audit import CouchAudit, AuditError, token  # noqa: E402

SCHEMA = 'starintel.request-audit.v1'
SIGNALS = ['identity-linking', 'sensitive-details', 'location-tracking',
           'exposure-intent', 'harassment-intent', 'protective-context']
FLAGS = ['potential-doxxing-setup', 'targeted-location-risk', 'sensitive-data-exposure',
         'model-review', 'assessment-unavailable', 'custom-policy']
STATUSES = ['assessed', 'rules-only', 'no-input', 'unavailable', 'input-too-large', 'policy-error']
ACTIONS = ['search:read', 'documents:read', 'documents:write', 'documents:delete',
           'documents:bulk', 'targets:dispatch', 'targets:read', 'targets:lease',
           'targets:force-release', 'events:write', 'events:replay', 'views:read',
           'identity:read', 'credentials:read', 'credentials:create', 'credentials:rotate',
           'credentials:revoke', 'credentials:disable', 'principals:manage', 'audit:read']
FIELDS = ['_id', 'schema', 'event_id', 'occurred_at', 'tenant_id', 'source', 'action',
          'principal_ref', 'subject_ref', 'correlation_ref', 'key_id', 'policy_version',
          'local_signals', 'model_signals', 'flags', 'rule_ids', 'assessment_status', 'mode', 'decision']
RULE = re.compile(r'[a-z0-9.-]{1,64}\Z')
REF = re.compile(r'(?:none|hmac-[0-9a-f]{64})\Z')
ULID = re.compile(r'[0-7][0-9A-HJKMNP-TV-Z]{25}\Z')
STAMP = re.compile(r'\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d\.\d{6}Z\Z')


def document_id(doc: dict) -> str:
    text = f"{doc['tenant_id']}\nrequest-audit\n{doc['event_id']}\n{doc['policy_version']}"
    return 'audit-' + hashlib.sha256(text.encode()).hexdigest()


def validate(doc: dict, tenant: str) -> dict:
    """Reject unknown fields rather than attempting to redact arbitrary payloads."""
    if not isinstance(doc, dict) or set(doc) != set(FIELDS):
        raise ValueError('invalid request audit envelope')
    if doc['schema'] != SCHEMA or doc['source'] != 'starintel-server' or doc['tenant_id'] != tenant:
        raise ValueError('request audit scope mismatch')
    token(tenant)
    for key, pattern in [('event_id', ULID), ('occurred_at', STAMP), ('key_id', RULE),
                         ('policy_version', RULE), ('principal_ref', REF), ('subject_ref', REF),
                         ('correlation_ref', REF)]:
        if not isinstance(doc[key], str) or not pattern.fullmatch(doc[key]):
            raise ValueError('invalid request audit field')
    datetime.fromisoformat(doc['occurred_at'].replace('Z', '+00:00'))
    if doc['_id'] != document_id(doc) or doc['action'] not in ACTIONS or doc['assessment_status'] not in STATUSES:
        raise ValueError('invalid request audit identity or status')
    for key, allowed in [('local_signals', SIGNALS), ('model_signals', SIGNALS), ('flags', FLAGS)]:
        values = doc[key]
        if (type(values) is not list or len(values) > len(allowed) or
                any(type(v) is not str or v not in allowed for v in values) or len(set(values)) != len(values)):
            raise ValueError('invalid request audit signals')
    rules = doc['rule_ids']
    if type(rules) is not list or len(rules) > 16 or any(type(v) is not str or not RULE.fullmatch(v) for v in rules):
        raise ValueError('invalid request audit rules')
    if doc['mode'] not in ['audit', 'enforce'] or doc['decision'] not in ['allow', 'would-refuse', 'refuse']:
        raise ValueError('invalid request audit decision')
    if ((doc['mode'] == 'audit' and doc['decision'] == 'refuse') or
            (doc['mode'] == 'enforce' and doc['decision'] == 'would-refuse')):
        raise ValueError('request audit mode/decision mismatch')
    if len(json.dumps(doc, ensure_ascii=True).encode()) > 16384:
        raise ValueError('request audit event exceeds limit')
    return json.loads(json.dumps(doc))


def design_document(tenant: str) -> dict:
    token(tenant)
    validator = r'''function(n,o,u,s){
function deny(){throw({forbidden:"invalid or immutable request audit event"});}
if(o || n._deleted || u.roles.indexOf(WRITER)<0) deny();
var keys=FIELDS, k, i;
for(k in n) if(keys.indexOf(k)<0 && k!=="_rev") deny();
for(i=0;i<keys.length;i++) if(!(keys[i] in n)) deny();
if(n.schema!==SCHEMA || n.source!=="starintel-server" || n.tenant_id!==TENANT) deny();
if(typeof n._id!=="string" || !/^audit-[0-9a-f]{64}$/.test(n._id)) deny();
if(typeof n.event_id!=="string" || !/^[0-7][0-9A-HJKMNP-TV-Z]{25}$/.test(n.event_id)) deny();
if(typeof n.occurred_at!=="string" || !/^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d\.\d{6}Z$/.test(n.occurred_at) || isNaN(Date.parse(n.occurred_at))) deny();
if(new Date(n.occurred_at).toISOString().slice(0,19)!==n.occurred_at.slice(0,19) || n.occurred_at.slice(0,4)==="0000") deny();
for(i=0;i<keys.length;i++) if(typeof n[keys[i]]==="string" && /[\x00-\x1f\x7f]/.test(n[keys[i]])) deny();
var refs=["principal_ref","subject_ref","correlation_ref"];
for(i=0;i<refs.length;i++) if(typeof n[refs[i]]!=="string" || !/^(none|hmac-[0-9a-f]{64})$/.test(n[refs[i]])) deny();
if(typeof n.key_id!=="string" || !/^[a-z0-9.-]{1,64}$/.test(n.key_id)) deny();
if(typeof n.policy_version!=="string" || !/^[a-z0-9.-]{1,64}$/.test(n.policy_version)) deny();
if(ACTIONS.indexOf(n.action)<0 || STATUSES.indexOf(n.assessment_status)<0) deny();
function labels(a,allowed){
if(!Array.isArray(a) || a.length>allowed.length) deny();
for(var j=0;j<a.length;j++) if(typeof a[j]!=="string" || allowed.indexOf(a[j])<0 || a.indexOf(a[j])!==j) deny();
}
labels(n.local_signals,SIGNALS); labels(n.model_signals,SIGNALS); labels(n.flags,FLAGS);
if(!Array.isArray(n.rule_ids) || n.rule_ids.length>16) deny();
for(i=0;i<n.rule_ids.length;i++) if(typeof n.rule_ids[i]!=="string" || !/^[a-z0-9.-]{1,64}$/.test(n.rule_ids[i]) || /[\x00-\x1f\x7f]/.test(n.rule_ids[i])) deny();
if(["audit","enforce"].indexOf(n.mode)<0 || ["allow","would-refuse","refuse"].indexOf(n.decision)<0) deny();
if((n.mode==="audit" && n.decision==="refuse") || (n.mode==="enforce" && n.decision==="would-refuse")) deny();
if(JSON.stringify(n).length>16384) deny();
}'''
    substitutions = dict(WRITER=f'request-audit-writer:{tenant}', TENANT=tenant, FIELDS=FIELDS,
                         SCHEMA=SCHEMA, ACTIONS=ACTIONS, STATUSES=STATUSES, SIGNALS=SIGNALS, FLAGS=FLAGS)
    validator = re.sub(r'\b(' + '|'.join(substitutions) + r')\b',
                       lambda match: json.dumps(substitutions[match.group()]), validator)
    guard = f'if(d.schema!=={json.dumps(SCHEMA)} || d.tenant_id!=={json.dumps(tenant)}) return;'
    def view(body: str, reduce: str | None = None) -> dict:
        result = {'map': 'function(d){' + guard + body + '}'}
        if reduce:
            result['reduce'] = reduce
        return result
    return {'_id': '_design/request_audit', 'language': 'javascript', 'validate_doc_update': validator,
            'views': {
                'by_time': view('emit([d.occurred_at,d._id],null);'),
                'by_flag_time': view('d.flags.forEach(function(f){emit([f,d.occurred_at,d._id],null);});'),
                'by_decision_time': view('emit([d.decision,d.occurred_at,d._id],null);'),
                'counts': view('d.flags.forEach(function(f){emit([d.occurred_at.slice(0,10),f,d.decision],1);});', '_count'),
                'decisions': view('emit([d.occurred_at.slice(0,10),d.decision],1);', '_count'),
                'signals_by_subject_hour': view(
                    'if(d.subject_ref==="none" || d.principal_ref==="none") return;'
                    'var v={events:1};'
                    f'{json.dumps(SIGNALS)}.forEach(function(s){{v[s]=(d.local_signals.indexOf(s)>=0 || d.model_signals.indexOf(s)>=0)?1:0;}});'
                    'emit([d.occurred_at.slice(0,13),d.key_id,d.policy_version,d.principal_ref,d.subject_ref],v);', '_sum')}}


class RequestAudit(CouchAudit):
    def initialize(self):
        """Provision a NEW dedicated DB. Never overwrite an unrelated DB or its ACL."""
        design = design_document(self.tenant)
        security = {'admins': {'names': [], 'roles': []}, 'members': {'names': [], 'roles':
                    [f'request-audit-reader:{self.tenant}', f'request-audit-writer:{self.tenant}']}}
        try:
            self.request('PUT')
        except AuditError as exc:
            if exc.status != 412:
                raise
            old = self.request('GET', '/_design/request_audit')
            old.pop('_rev', None)
            if self.request('GET', '/_security') != security or old != design:
                raise ValueError('existing request audit DB differs; explicit migration required') from None
            return
        # Do not expose a newly created database to runtime writers before validation exists.
        self.request('PUT', '/_security', data={'admins': {'names': [], 'roles': []},
                                              'members': {'names': [], 'roles': ['_admin']}})
        self.request('PUT', '/_design/request_audit', data=design)
        self.request('PUT', '/_security', data=security)

    def append(self, doc: dict) -> str:
        clean = validate(doc, self.tenant)
        try:
            self.request('PUT', '/' + clean['_id'], data=clean)
        except AuditError as exc:
            if exc.status != 409:
                raise
            if self.get(clean['_id']) != clean:
                raise ValueError('request audit event identity collision') from None
        return clean['_id']

    def get(self, event_id: str) -> dict:
        if not re.fullmatch(r'audit-[0-9a-f]{64}', event_id):
            raise ValueError('invalid event id')
        doc = self.request('GET', '/' + event_id)
        doc.pop('_rev', None)
        return validate(doc, self.tenant)

    def history(self, limit: int = 50, flag: str | None = None, decision: str | None = None):
        if type(limit) is not int or not 1 <= limit <= 1000 or (flag and decision):
            raise ValueError('invalid history options')
        if flag and flag not in FLAGS:
            raise ValueError('unknown flag')
        if decision and decision not in ['allow', 'would-refuse', 'refuse']:
            raise ValueError('unknown decision')
        prefix = [flag or decision] if (flag or decision) else []
        view = 'by_flag_time' if flag else 'by_decision_time' if decision else 'by_time'
        rows = self.request('GET', f'/_design/request_audit/_view/{view}', params={
            'descending': 'true', 'include_docs': 'true', 'limit': limit,
            'startkey': json.dumps(prefix + [{}]), 'endkey': json.dumps(prefix)})['rows']
        for row in rows:
            doc = row['doc']
            doc.pop('_rev', None)
            yield validate(doc, self.tenant)

    def aggregate(self, view: str, limit: int = 100):
        if view not in ['counts', 'decisions', 'signals_by_subject_hour'] or not 1 <= limit <= 1000:
            raise ValueError('invalid aggregation')
        return self.request('GET', f'/_design/request_audit/_view/{view}', params={
            'group': 'true', 'descending': 'true', 'limit': limit})['rows']


def setup_flags(counts: dict) -> list[str]:
    """Reviewer hints ONLY. Same tenant/principal/subject/hour; never an enforcement input."""
    identity = counts.get('identity-linking', 0) > 0
    sensitive = counts.get('sensitive-details', 0) > 0
    location = counts.get('location-tracking', 0) > 0
    hostile = counts.get('exposure-intent', 0) > 0 or counts.get('harassment-intent', 0) > 0
    result = []
    if identity and (sensitive or location):
        result.append('potential-doxxing-setup')
    if location and (identity or hostile):
        result.append('targeted-location-risk')
    if hostile and (identity or sensitive or location):
        result.append('sensitive-data-exposure')
    return result


def read_secret(path: str) -> str:
    file = Path(path)
    info = file.stat()
    if not stat.S_ISREG(info.st_mode) or info.st_mode & 0o077 or info.st_size > 4096:
        raise ValueError('password file must be a private regular file, at most 4096 bytes')
    value = file.read_text().rstrip('\r\n')
    if not value:
        raise ValueError('password file is empty')
    return value


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--url', required=True, help='dedicated tenant audit database URL')
    parser.add_argument('--tenant', required=True)
    parser.add_argument('--user', required=True)
    parser.add_argument('--password-file', default=os.getenv('STAR_REQUEST_AUDIT_PASSWORD_FILE'), required=not bool(os.getenv('STAR_REQUEST_AUDIT_PASSWORD_FILE')))
    subs = parser.add_subparsers(dest='command', required=True)
    subs.add_parser('init', help='administrator: create a new dedicated database')
    history = subs.add_parser('list')
    history.add_argument('--limit', type=int, default=50)
    history.add_argument('--flag', choices=FLAGS)
    history.add_argument('--decision', choices=['allow', 'would-refuse', 'refuse'])
    show = subs.add_parser('show'); show.add_argument('id')
    for command in ['counts', 'decisions', 'setups']:
        sub = subs.add_parser(command); sub.add_argument('--limit', type=int, default=100)
    args = parser.parse_args(argv)
    try:
        client = RequestAudit(args.url, args.tenant, args.user, read_secret(args.password_file))
        if args.command == 'init':
            client.initialize(); print(json.dumps({'initialized': True})); return 0
        if args.command == 'show':
            rows = [client.get(args.id)]
        elif args.command == 'list':
            rows = client.history(args.limit, args.flag, args.decision)
        else:
            rows = client.aggregate('signals_by_subject_hour' if args.command == 'setups' else args.command, args.limit)
            if args.command == 'setups':
                rows = [dict(row, potential_flags=flags, review_only=True) for row in rows if (flags := setup_flags(row['value']))]
        for row in rows:
            print(json.dumps(row, sort_keys=True))
        return 0
    except (AuditError, ValueError, OSError, KeyError, TypeError):
        print('request audit operation failed; check configuration, permissions and database availability', file=sys.stderr)
        return 1


if __name__ == '__main__':
    raise SystemExit(main())
