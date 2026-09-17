import copy
import importlib.util
import json
from pathlib import Path
import subprocess
import unittest

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location('request_audit', ROOT / 'tools/request-audit/request_audit.py')
audit = importlib.util.module_from_spec(spec)
spec.loader.exec_module(audit)


def fixture(**changes):
    doc = dict(schema=audit.SCHEMA, event_id='01ARZ3NDEKTSV4RRFFQ69G5FAV',
               occurred_at='2026-09-16T20:00:00.000000Z', tenant_id='tenant-a', source='starintel-server',
               action='targets:dispatch', principal_ref='hmac-' + 'a'*64, subject_ref='hmac-' + 'b'*64,
               correlation_ref='none', key_id='k1', policy_version='local-v1',
               local_signals=['identity-linking', 'sensitive-details'], model_signals=[],
               flags=['potential-doxxing-setup'], rule_ids=['identity-plus-personal.v1'],
               assessment_status='rules-only', mode='audit', decision='allow')
    doc.update(changes)
    doc['_id'] = audit.document_id(doc)
    return doc


class Cases(unittest.TestCase):
    def test_detached_snapshot(self):
        doc = fixture(); result = audit.validate(doc, 'tenant-a')
        result['local_signals'].append('exposure-intent')
        self.assertEqual(len(doc['local_signals']), 2)

    def test_reject_unallowlisted_information(self):
        for field in ['raw_request', 'address', 'authorization', 'model_output', '_attachments']:
            doc = fixture(); doc[field] = 'must-never-log-this'
            with self.assertRaises(ValueError): audit.validate(doc, 'tenant-a')

    def test_closed_model_observations(self):
        for value in [['grant-admin'], ['sensitive-details']*2, {'private': 'data'}, 'explanation']:
            with self.assertRaises(ValueError): audit.validate(fixture(model_signals=value), 'tenant-a')

    def test_tenant_and_reference_isolation(self):
        with self.assertRaises(ValueError): audit.validate(fixture(), 'tenant-b')
        with self.assertRaises(ValueError): audit.validate(fixture(principal_ref='a-person-name'), 'tenant-a')
        doc = fixture(); doc['_id'] = 'audit-'+'0'*64
        with self.assertRaises(ValueError): audit.validate(doc, 'tenant-a')

    def test_modes_are_explicit(self):
        for mode, decision in [('audit', 'refuse'), ('enforce', 'would-refuse')]:
            with self.assertRaises(ValueError): audit.validate(fixture(mode=mode, decision=decision), 'tenant-a')
        audit.validate(fixture(mode='enforce', decision='refuse'), 'tenant-a')

    def test_setup_combination_is_review_only(self):
        self.assertFalse(audit.setup_flags({'sensitive-details': 1}))
        self.assertIn('potential-doxxing-setup', audit.setup_flags({'identity-linking': 1, 'sensitive-details': 1}))
        self.assertIn('targeted-location-risk', audit.setup_flags({'location-tracking': 1, 'harassment-intent': 1}))

    def test_view_security_scope_and_provisioning_order(self):
        class Fake(audit.RequestAudit):
            def __init__(self): self.tenant='tenant-a'; self.calls=[]
            def request(self, method, path='', **kw): self.calls.append((method, path, kw)); return {}
        client=Fake(); client.initialize()
        self.assertEqual([x[1] for x in client.calls], ['', '/_security', '/_design/request_audit', '/_security'])
        self.assertEqual(client.calls[1][2]['data']['members']['roles'], ['_admin'])
        self.assertEqual(client.calls[-1][2]['data']['members']['roles'],
                         ['request-audit-reader:tenant-a', 'request-audit-writer:tenant-a'])

    def test_idempotent_append_and_collision(self):
        class Fake(audit.RequestAudit):
            def __init__(self): self.tenant='tenant-a'; self.saved=None
            def request(self, method, path='', **kw):
                if self.saved: raise audit.AuditError(409)
                self.saved=kw['data']; return {}
            def get(self, id): return self.saved
        client=Fake(); doc=fixture()
        self.assertEqual(client.append(doc), client.append(doc))
        changed=fixture(flags=[])
        with self.assertRaises(ValueError): client.append(changed)

    def test_history_rejects_cross_tenant_response(self):
        class Fake(audit.RequestAudit):
            def __init__(self): self.tenant='tenant-a'
            def request(self, *a, **kw): return {'rows':[{'doc':fixture(tenant_id='tenant-b')}]}
        with self.assertRaises(ValueError): list(Fake().history())

    def test_javascript_validator_and_map_functions(self):
        cases = []
        def case(doc, old=None, roles=None, allowed=False):
            cases.append(dict(doc=doc, old=old, roles=roles or [], allowed=allowed))
        writer=['request-audit-writer:tenant-a']
        case(fixture(), roles=writer, allowed=True)
        case(fixture(), roles=['request-audit-reader:tenant-a'])
        case(fixture(), roles=['request-audit-writer:tenant-b'])
        case(fixture(), old=fixture(), roles=writer)
        case(dict(fixture(), _deleted=True), roles=writer)
        for field in ['raw_request','_attachments','model_output','password']:
            case(dict(fixture(), **{field:'secret'}), roles=writer)
        for changes in [dict(tenant_id='tenant-b'), dict(flags=['private-address']),
                        dict(mode='audit',decision='refuse'), dict(local_signals=['identity-linking']*2),
                        dict(rule_ids=['rule\n']), dict(occurred_at='2026-02-31T00:00:00.000000Z'),
                        dict(model_signals={'signals': []}), dict(subject_ref='raw-identifier')]:
            case(fixture(**changes), roles=writer)
        js = r'''
const vm=require('node:vm'),fs=require('node:fs');
const p=JSON.parse(fs.readFileSync(0,'utf8'));
const v=vm.runInNewContext('('+p.design.validate_doc_update+')', {}, {timeout:1000});
for(const c of p.cases){let ok=true;try{v(c.doc,c.old,{roles:c.roles},{});}catch(e){ok=false;}
if(ok!==c.allowed)throw new Error('validation mismatch');}
for(const d of p.designs)vm.runInNewContext('('+d.validate_doc_update+')',{}, {timeout:1000});
let rows=[];
function map(name,doc){rows=[];vm.runInNewContext('('+p.design.views[name].map+')(doc)',
 {doc,emit:(k,v)=>rows.push({key:k,value:v})},{timeout:1000});return rows;}
if(map('by_flag_time',p.doc)[0].key[0]!=='potential-doxxing-setup')throw Error('flag key');
if(map('by_time',{...p.doc,tenant_id:'tenant-b'}).length)throw Error('cross tenant');
if(map('signals_by_subject_hour',{...p.doc,subject_ref:'none'}).length)throw Error('unscoped subject');
const stages=map('signals_by_subject_hour',p.doc);
if(stages[0].value['identity-linking']!==1||stages[0].key[4]!==p.doc.subject_ref)throw Error('stages');
if(p.design.views.counts.reduce!=='_count'||p.design.views.signals_by_subject_hour.reduce!=='_sum')throw Error('reduce');
console.log(JSON.stringify({validator_cases:p.cases.length,maps:'passed'}));
'''
        result=subprocess.run(['node','-e',js], input=json.dumps(dict(design=audit.design_document('tenant-a'),
            designs=[audit.design_document('FIELDS'),audit.design_document('WRITER')],cases=cases,doc=fixture())),
            text=True,capture_output=True,check=True,timeout=10)
        self.assertEqual(json.loads(result.stdout)['validator_cases'], len(cases))


if __name__ == '__main__': unittest.main()
