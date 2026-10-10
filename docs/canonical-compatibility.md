# Canonical and retained legacy boundaries

The default star-cl API is generated StarIntel 0.10.1. Canonical ingress,
validation, persistence and readback use that authority. Historical CLOS
constructors and fixtures explicitly load `starintel-legacy` and name
`STARINTEL.LEGACY`; the retired `SPEC` alias is not restored.

Legacy HTTP envelope normalization retains its historical schema version.
The targets.create boundary constructs generated flat 0.10.1 Target documents
with object-valued options. Recovery distinguishes canonical identity from
explicit historical reads. Canonical dispatch removes CouchDB metadata and
validates the outgoing document. Durable retries compare typed canonical
content, including dataset and recursively ordered options, rather than relying
on historical digests. Existing legacy fingerprint behavior is retained. InvestigationTarget retains
its separately generated array-options contract.

HTTP request identities use structured principal/key framing. Before creating
a canonical acceptance, the route checks the exact historical acceptance key.
For a valid canonical request, any existing historical record yields HTTP 409
`target_idempotency_version_conflict`, without creating, rewriting, resuming or
scheduling work. Invalid old-format payloads can fail validation with 422 before this lookup.
Cross-version automatic replay is unsupported; malformed or
ambiguous historical ownership also fails closed without exposing its receipt.
Stop old-version HTTP writers before cutover: the read-only compatibility guard
cannot atomically exclude a concurrent old writer creating its historical key.
Use a deliberately new idempotency key only when new work is intended.

Issue #319 completion additionally requires the isolated real actor probe to
prove authenticated persisted canonical output readback; unit acceptance and
broker confirmation alone do not establish that result.

The historical URL extractor retains its old URL/relation output and routing.
It explicitly logs and rejects canonical input before construction/publication.
Canonical matcher support is unsupported and remains issue #44, which requires
its own implementation approval. This dependency adoption does not claim that
all active producers are canonical. The CLI MOP generator also remains an
explicitly legacy document generator.

Qlot 1.6 natively normalizes the cl+ssl lock label from ql-2024-10-12 to
ql-2023-10-21. Both dated distributions have identical cl+ssl release and
system/dependency metadata and the same 92,364-byte archive, SHA256
`554e8cf79771221cf3991085da9b7cb659a78fd7ff71c81ae56d524f2d1eec46`.
This exact label normalization is checked separately from the maintained
star-cl update; other dependency entries remain unchanged.

Canonical HTTP document and search readback preserve JSON false/null and empty
collections through injective parsing. Public projection removes only the
server-owned top-level extension keys `_server_outbox` and `_server_mutations`,
retains caller extension content and canonical revision, and does not alter the
stored evidence or add an absent extensions object.
