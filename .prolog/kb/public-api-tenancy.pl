% Public read boundary, verified against source and the pinned 0.10.1 lock.
invariant(public_read_tenant_scope,
          'Anonymous search uses the server-owned public tenant and an explicit dataset allowlist; missing or wildcard configuration fails closed.').
invariant(public_search_query_grouping,
          'Escape caller Lucene syntax before wrapping it with server-owned tenant and dataset clauses, so caller parentheses cannot escape the scope.').
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
