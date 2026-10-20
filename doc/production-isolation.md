# Private tenant isolation probe

Issue #343, child of #52. Run only against a disposable local private-mode
stack (`STAR_PUBLIC_MODE=false`). Public search intentionally supports public
datasets and is not a private tenant boundary.

Configure `STAR_TENANT_DATASET_MAP=readiness-a=tenant-a,readiness-b=tenant-b`.
Provide two fresh canonical synthetic message fixtures (distinct IDs prefixed
`readiness:` or `readiness-`), one per dataset. The ingestion harness's
`fixture_document(run_id, index, dataset)` creates suitable fixtures. Do not add
`tenant_id`: the server stamps its dataset mapping. Use an empty disposable
database so search's first 100 results contain these fixtures.

Provision one credential for each tenant using `/auth/credentials`. Each must
have exactly these scopes, replacing `tenant-a` with `tenant-b` for the second:

```json
["identity:read", "documents:read", "documents:write", "documents:delete", "search:read", "dataset:*", "tenant:tenant-a"]
```

The wildcard dataset permission is deliberate: an unauthorized request must
fail because of tenant scope, not because its dataset differs. Keep keys in
untracked mode-0600 files outside the checkout and Nix store. The probe verifies
these scopes before writing and does not follow HTTP redirects or print bodies.

```sh
python3 scripts/check-starintel-schema-lock.py schema/starintel-schema.lock.json
python3 scripts/readiness_isolation.py --url http://127.0.0.1:8080 \
  --key-file /tmp/readiness-secrets/a --key-file /tmp/readiness-secrets/b \
  --fixture /tmp/readiness-a.json --fixture /tmp/readiness-b.json \
  --tenant tenant-a --tenant tenant-b
```

The probe requires own-tenant ingest/read/search success, cross-tenant
read/create/update/delete/search denials, and inaccessible attachment and
changes-feed URL candidates. Those URLs are unsupported surfaces, not feature
implementations. It checks over-limit result and JSON body rejection. It leaves
the two fixtures for inspection; destroy the disposable stack/volumes and keys
after evidence collection. Fresh IDs are required for each run.

The Python unit tests exercise probe guardrails only:

```sh
python3 -m unittest discover -s tests -p test_readiness_isolation.py -v
```

No live-server pass is implied by those unit tests. This probe does not prove
raw CouchDB access is isolated: infrastructure credentials and network exposure
must remain separate. It does not prove transport memory bounds (the existing
JSON size check runs after body buffering), backend query complexity limits,
timeouts, deep search pagination, or anonymous public-mode privacy. Authenticated
search now rejects query text longer than 512 characters before authorization
query compilation or backend work, matching public search's existing length
limit. Sort and bookmark bounds remain unverified. A text length bound does not
establish a backend complexity or execution-time bound.

## Query length regression evidence

The tests-only commit `bffa109679875d2286b6657f5cf7a63fd0ffc312` ran the native
`nix run .#star-unit-tests` gate in GitHub Actions run `39346895223`, job
`118381554821`. The build succeeded. `HTTP-AUTH-TESTS` discovered and executed
14 tests: 13 passed, one failed, none skipped. The new regression produced four
expected assertion failures: on both `/search` and `/api/v1/documents/search`,
a 513-character query reached the query-scoping sentinel once and returned
`probe_query_reached` instead of stopping with `search_query_too_long`.
Both 512-character positive controls passed. This establishes a behavior RED,
not a dependency or zero-discovery failure.

The fix checks length before constructing the authorized query. Its exact-head
hosted GREEN remains required; this document does not claim that run completed.
