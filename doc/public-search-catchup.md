# Public search during index catch-up

`GET /api/v1/search` reads candidate IDs from the existing full-text index with
`stale=ok`, without waiting for ongoing ingestion or index rebuilding to catch up.
New or changed matches can therefore appear later. Authenticated search is unchanged.

Index rows are not disclosure authority. Before returning a candidate, the server
fetches its current document from the configured canonical database and checks its
identity, public tenant, and existing approved dataset allowlist. Deleted, missing,
private, or no-longer-approved documents are omitted. Indexed documents, stored
fields, and snippets are never returned in place of the current document. Tenant
metadata is stripped from the current document as before; responses remain `no-store`.

Each request examines at most its requested candidate limit, without retries or
page draining to fill filtered results. The index bookmark is preserved;
`total_rows` counts only the current eligible documents returned on this page,
not an authoritative total of all query matches. Malformed candidates/current
documents and canonical storage failures fail the request rather than returning
an apparently successful empty page or falling back to indexed content.

The [anonymous read boundary](public-read-boundary.org) still applies. This change
does not widen the dataset allowlist, authorize a retired database fallback, or
grant anonymous by-ID document access. Regression coverage lives in
`t/http-capabilities-test.lisp` under `http-boundary-tests` (issue #420).
