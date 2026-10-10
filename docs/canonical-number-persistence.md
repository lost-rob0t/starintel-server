# Canonical number persistence

Canonical JSON ingress and output use the pinned star-cl exact JSON codec. Fractional, exponent and signed-zero tokens retain their spelling; integers retain arbitrary precision. The server's JSOWN adapter preserves existing array and literal contracts without coercing canonical number tokens through floats.

## CouchDB boundary

CouchDB materializes JSON fractions as binary64. A raw service test demonstrates that 0.12345678901234567890123456789 returns as 0.12345678901234568. Numeric fields remain JSON numbers in storage, preserving existing indexing and ordering behavior. Numeric view equality and range queries therefore retain CouchDB's approximate precision; they are not exact canonical-number queries.

Each supported atomic save derives bounded typed paths and exact tokens in the existing private extensions._server_exact_numbers namespace. The evidence includes a digest of the complete document structure and nonnumeric content, excluding CouchDB's assigned revision and the evidence itself. Object keys are sorted; array order and literal distinctions remain significant. Paths distinguish object keys from array indices.

Complete document reads verify the digest, paths and each numeric leaf against its exact token or supported finite binary64 projection before restoring canonical tokens. Malformed or stale evidence fails closed. Projected or reduced view values are not restored without complete-document context. Every supported update, including outbox publication marking, regenerates evidence atomically. Incoming private evidence is discarded; it cannot author canonical values. Public projection removes the private state.

This is fidelity metadata for supported server writes, not tamper authentication against privileged out-of-band database writers. An external numeric edit within the same binary64 rounding bucket cannot be distinguished by that projection. External tools must not edit documents behind the server's revision-aware write boundary. Values without a supported finite CouchDB projection are rejected rather than substituted with null.

Unmarked historical records keep their existing values. Already lost digits and token spelling cannot be reconstructed. Stored pending outbox event identifiers, routing keys and payloads remain historical evidence and must not be recomputed from current public content.

## Retry compatibility

New canonical outbox entries mark their content encoding as exact-json-v1. Their hashes retain exact number spelling, so 1.0 and 1e0 are distinct mutation content. Explicit mutation keys remain unchanged. Existing exact identity matches are duplicates; reusing an explicit key with different exact content conflicts.

An implicit update that collides with an unmarked historical rounded identity fails closed instead of appending a duplicate or claiming lost precision was recovered. A fresh explicit mutation key deliberately requests a new precision-correcting update. The old serializer is used only for this read-only collision probe, never for canonical persistence or emission. Historical entries, hashes, event IDs and saved pending payloads are not rewritten.
