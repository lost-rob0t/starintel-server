# StarIntel server v0.9 migration

The server now treats the canonical StarIntel v0.9 envelope as its ingestion boundary.

- RabbitMQ and HTTP reject incomplete envelopes and route/body dtype mismatches.
- Missing `schema_org` metadata receives deterministic JSON-LD defaults.
- CouchDB views read dtype-specific values from `data` with a temporary legacy top-level fallback.
- Design documents are upserted at startup using their current CouchDB `_rev`.
- Timeline indexes use `date_added` ISO-8601 values.
- `GET /document/:id/schema-org` returns the stored Schema.org JSON-LD metadata.

The server branch depends on the matching `star-cl` v0.9 runtime branch for canonical dtype aliases, Schema.org mappings, and document encoding.
