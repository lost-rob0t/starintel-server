# StarIntel document conformance corpus

`v0.9` is the write contract. `v0.8` is retained only as a read/index input.

The server selects a `document-schema-profile` strategy from the wire version.
The v0.8 profile is an anti-corruption adapter: it uses the Common Lisp MOP to
derive legacy flat-field to v0.9 `data` mappings from the active CLOS document
classes. It never rewrites the stored source document and it cannot pass the
write boundary.

Consumers must preserve these JSON distinctions:

- absent key
- `null`
- `false`
- empty array
- empty string
- nested object

The fixture corpus is language-neutral. Common Lisp executes the CLOS/MOP and
codec tests in `tests/run-document-conformance.lisp`; Python validates the
corpus and key contract in `tests/test_document_conformance.py`. Nim and
JavaScript implementations should consume the same JSON files and compare
semantic JSON values rather than object key order.
