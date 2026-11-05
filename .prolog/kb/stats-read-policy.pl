% Issue #406: HTTP stats must not wait for view indexing under continuous ingest.
root_cause(stats_view_timeout, synchronous_index_catch_up,
    "Stats inherited query-view update=true. During CouchDB compaction and ingest,
the HTTP request waited for index catch-up until its database socket timed out;
health and document reads could remain ready throughout.").
method(stats_nonblocking_aggregate,
    "Pass :update :lazy only in handle-public-stats-route. This serves available
indexed aggregates and requests asynchronous advancement; do not change the
general query-view default or use permanently non-advancing update=false.").
invariant(stats_public_scope_preserved,
    "Keep the public tenant and approved dataset key ranges, limit 1000,
aggregate-row validation and missing/wildcard dataset rejection unchanged.").
invariant(stats_generation_time_is_not_index_watermark,
    "generated_at is response generation time. Lazy index reads are eventually
consistent, not a guarantee of zero-lag counts or bounded index staleness.").
stats_regression('t/http-capabilities-test.lisp',
    [api_v1_public_stats_does_not_wait_for_index_catch_up,
     api_v1_public_stats_rejects_unconfigured_scope_before_transport]).
