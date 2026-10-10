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
on historical digests. Existing legacy fingerprint behavior is retained.

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
