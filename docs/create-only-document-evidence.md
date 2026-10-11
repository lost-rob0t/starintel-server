# Issue 290: bounded create-only document writes

Starting source: `491b89dad31e15b4172d52a03ab525b6c92fd637`, branch
`feat/290-create-only-document`, child of #279; consumer starintel-labs/star-kb#23.
Schema preflight passed: generated 0.10.1 at
`96d26c338078676ec1dc6928755901636c9f7ef0`. No schema changes.

All five required independent votes returned NARROW: reality, ownership,
research, verification, YAGNI. The intersection is an optional HTTP
`If-None-Match: *` precondition on the existing authorized PUT and existing
bounded optimistic update loop. Existing documents return 412 without mutation,
including after a lost insertion race. Unsupported conditional values return
400. Ordinary PUT retains its existing update contract. Capabilities advertise
`document_create_only: if-none-match-star/1`. No generic idempotency registry,
receipt protocol, outbox or issue #47 implementation is introduced.

Local research consulted:
- `/workspace/starintel-auto-research/roam/design/star-server/STAR-ADR-002-hybrid-logic-runtime-model.org`
- `/workspace/starintel-auto-research/roam/research/star-kb/STAR-KB-RESEARCH-004-portable-rule-reuse-across-logic-actors.org`

## Behavioral evidence

Consumer harness: `service/readback_integration.py` in Star-KB PR #23.
With STARLANG_ROOT, STAR_KB_SBCL and STARLANG_SWI_EXECUTABLE configured, run:

```
STAR_KB_SERVER_ROOT=/path/to/server python3 service/readback_integration.py
```

Untouched baseline, `-k test_00`: **RED**, one test ran, expected 412 but got 200
on the second conditional write. This is a behavior failure, not startup failure.
Patched worktree: **GREEN**, five tests passed in 28.800 seconds, using real
server HTTP, CouchDB 3.5.2, RabbitMQ 4.3.4 and SBCL/SWI. Tests cover unchanged
original content, unsupported header, ordinary update, exact replay, conflicting
replay, concurrent insertion, denied foreign scope, canonical extension
preservation, server/store restart and fresh Prolog query with stored provenance.

The local proof loads the four changed production files through the existing
trusted init hook over image digest
`sha256:9f889150082971212a3b04196aca0c7d014476f6d61e1574cd909ef81b9cc97d`.
The VFS Docker driver required flattening its 100 layers without changing the
filesystem entries; local imported image identity is
`sha256:72189a14df35dc0b49707253d455f04ebbe6efe7b792fa5557022ae679b1f46d`.
Expected redefinition warnings are muffled only during test overlay loading.
This is **not** a native build of this branch. Exact-head native Nix unit and
integration CI remain required. The two new Lisp unit cases await that gate.

No deployment. Graceful restart is not commit-before-ack crash proof; generic
exactly-once semantics and cross-document transactions are not claimed.
