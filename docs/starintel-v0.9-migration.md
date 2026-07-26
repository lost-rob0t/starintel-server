# StarIntel server v0.9 migration

The server treats the StarIntel v0.9 envelope as its only write boundary.

- RabbitMQ and HTTP reject incomplete envelopes, unknown top-level keys, legacy
  write attempts, and route/body dtype mismatches.
- Missing `schema_org` metadata receives deterministic JSON-LD defaults.
- Version handling uses a CLOS schema-profile strategy.
- The v0.9 profile is read/write.
- The v0.8 profile is read/index-only and acts as an anti-corruption adapter.
- The adapter uses `closer-mop` slot metadata to derive legacy camelCase
  flat-field mappings into v0.9 snake_case `data`.
- v0.8 index projections preserve `schema_version: 0.8.0` and declare
  `extensions.index_schema_version: 0.9.0`; they are not silent migrations.
- CouchDB views keep temporary top-level fallback support while stored v0.8
  records remain in the database.
- Design documents are upserted at startup using their current CouchDB `_rev`.
- Timeline indexes use `date_added` ISO-8601 values.
- `GET /document/:id/schema-org` returns stored Schema.org JSON-LD metadata.

The language-neutral fixture corpus lives under
`test/fixtures/starintel/`. The Docker conformance stage executes the CLOS/MOP
profile tests and the v0.9 encode/decode round-trip tests. The v0.9 server branch
depends on the matching `star-cl` v0.9 runtime branch.
