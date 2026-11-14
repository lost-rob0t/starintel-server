% Public read boundary, verified against source and the pinned 0.10.1 lock.
:- discontiguous invariant/2.
:- discontiguous method/2.
:- discontiguous root_cause/3.
:- discontiguous blocker/2.
invariant(public_read_tenant_scope,
          'Anonymous search uses the server-owned public tenant and an explicit dataset allowlist; missing or wildcard configuration fails closed.').
invariant(public_search_query_grouping,
          'Escape caller Lucene syntax before wrapping it with server-owned tenant and dataset clauses, so caller parentheses cannot escape the scope.').
invariant(public_search_tenant_alias_agreement,
          'Canonical public search documents with both tenant_id and tenant must agree; contradictory non-null values fail the request before disclosure.').
root_cause(public_search_conflicting_tenants, first_alias_wins,
           'Selecting (or tenant_id tenant) silently accepted contradictory public/private declarations; PR422 regression demonstrated public content disclosure from that ambiguous canonical record.').
invariant(public_stats_tenant_scope,
          'Anonymous stats use only tenant and dataset keyed aggregate view rows; global data views are not public-safe in a mixed-tenant database.').
method(public_read_activation,
       'Set STAR_PUBLIC_SEARCH_DATASETS to reviewed dataset names and install the data design document before enabling anonymous reads. Never set a wildcard in a mixed-tenant database.').
root_cause(public_read_cross_tenant_exposure, wildcard_trusted_context,
           'The prior public search context used tenant:* and default dataset:*, while public stats queried global aggregate views; both could span tenants.').
planned_scope(authenticated_user, own_tenant, read).
planned_scope(authenticated_user, public_tenant, read).
planned_denial(authenticated_user, peer_private_tenant, read).
implementation_status(authenticated_user_own_plus_public_scope, not_implemented).
planned_in_issue(authenticated_user_own_plus_public_scope, 28).

invariant(public_stats_matches_scoped_view,
          'For every allowlisted dataset, anonymous stats totals must match the tenant=public keyed CouchDB reduce view after persisted writes; a zero total is not evidence of an empty public tenant without this check.').
blocker(public_stats_count_mismatch,
        'https://git.starintel.actor/starintel-labs/starintel-server/issues/30').

%% --- Reddit public tenant lane, verified live 2026-10-31 ---

% Verified by grep: exported, never CALLed anywhere in source/.
fact(tenant_adaptation_unused,
     'STAR_TENANT_DATASET_MAP parses into *tenant-dataset-map* and
tenant-adaptation-for/1 is exported but has no caller; stored documents
do NOT get tenant_id from the map at ingest. The working path for lane
tenancy is publisher-side: pro-actors DocumentPublisher stamps
tenant_id when STARINTEL_TENANT is set, and decode-rabbit-document
(rabbit.lisp) preserves transport-level tenant_id into persistence.').

invariant(lucene_scope_clause_shape,
          'dataset/tenant scope clauses compile to (field:"v1" OR field:"v2") with quoted lucene-terms; verified against the live fts index that both quoted and unquoted forms match (141933 rows) - clause shape is NOT the open #359 defect.').

verified_working(public_search_underlayers,
                 'fts design+index, default-field text, dataset/tenant keyword analyzers, couch pool auth, documents GET, and the allowlist module load are all individually verified working on prod 2026-10-31; only the composed public search/stats routes return empty (issue #359).').
