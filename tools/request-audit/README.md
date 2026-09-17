# Request-risk auditing and opt-in refusal

Refs #200, stacked on #195 / #194. This is an **optional addon**, not a deployment or
an approval of the rest of the observability roadmap. The existing StarIntel schema
lock stays unchanged: these are operational `starintel.request-audit.v1` records,
not canonical collected documents. PostgreSQL remains the billing authority.

## Trust and behavior

The existing authorization policy runs first. A denied request stays denied and is
not sent to the classifier. After authorization, pure Common Lisp predicates derive
review flags from local intent signals and optional model observations. The default
policy requires local personal-data/identity/location signals **and** local
exposure/harassment intent before proposing refusal. Model-only evidence can flag
human review, never independently refuse under the default policy. A custom trusted
Lisp policy can be stricter; it must return a bounded `policy-result`.

Loading the addon does not install the policy, create a worker, resolve credentials,
create a database, or send a request. `install` is an explicit trusted startup action.
`refusals-enabled` defaults to **NIL** and accepts only NIL or T. With NIL, even a
refusal proposal or unavailable classifier cannot turn an authorized request into
a denial. The event records `would-refuse` instead of `refuse`.

When enforcement is explicitly enabled, unavailable/malformed assessments default
to denial (`:on-assessment-error :deny`). `:on-audit-error` separately chooses
`:allow` (default) or `:deny`; the latter only blocks in enforce mode. A refusal that
was already decided never becomes an allow because a sink failed. Sink failure is
reported through a sanitized callback and the normal authorization event, **not**
claimed as a committed CouchDB record. This synchronous sink is best effort unless
strict failure behavior is explicitly selected; it is not an atomic outbox for the
subsequent application operation.

Internal conditions distinguish `request_refused` and `request_audit_unavailable`.
The existing HTTP boundary intentionally still renders its generic **403
access_denied** plus correlation ID. Detailed rule IDs stay in the restricted audit
stream, not the public response. No capability, scope, tenant boundary, or admin
permission is added by this addon.

## Install/configure from Lisp

Make `starintel-request-audit.asd` discoverable alongside `starintel-audit.asd` and
the existing server system. Load `example_configs/request-audit-init.lisp` from your
trusted `STAR_SERVER_INIT_FILE` **before** workers and HTTP start. It contains the
complete audit-only example. Runtime reconfiguration must be performed at a quiescent
startup boundary; the function-cell adapters are not a concurrent hot-swap protocol.
The returned uninstall closure restores only hooks/engine entries it still owns.
Repeated installation without uninstalling is rejected rather than nesting hooks.

Provision the dedicated database first (administrator credentials, not runtime ones):

```sh
python3 tools/request-audit/request_audit.py \
  --url http://127.0.0.1:5984/starintel_request_audit_default \
  --tenant default --user YOUR_COUCHDB_ADMIN \
  --password-file /run/secrets/couchdb-admin-password init
```

Use a new database, **not** the collected-document or general-audit database. Runtime
writer role: `request-audit-writer:default`; reviewer role:
`request-audit-reader:default`. Create distinct CouchDB users with those roles via
your existing credential provisioning. The CLI creates no users. Use mode 0400/0600
secret files, a fresh random 32–64-byte HMAC key encoded as hex for the Lisp example,
and the least-privileged writer. The CLI refuses group/world-readable password files.
No provider, key, guild, or server host is guessed or enabled by this change.

Core configuration is:

```lisp
(star.request-audit:install
 :writers tenant-writers          ; exact tenant IDs -> append callbacks
 :hmac-key audit-hmac-key         ; 32–64 octets; secret, never a tenant name
 :key-id "k1"
 :version "operator-v1"
 :classifier nil                 ; Lisp-only, or an explicit bounded classifier
 :policy #'star.request-audit:default-policy
 :refusals-enabled nil)           ; change to T only for deliberate enforcement
```

Custom Lisp rule, still audit-only until you opt in:

```lisp
(defun operator-policy (local model)
  (let ((base (star.request-audit:default-policy local model)))
    (if (and (member "identity-linking" local :test #'string=)
             (member "sensitive-details" local :test #'string=))
        (star.request-audit:make-policy-result
         :flags (adjoin "custom-policy"
                        (star.request-audit:policy-result-flags base) :test #'string=)
         :rule-ids (adjoin "operator-personal-profile.v1"
                           (star.request-audit:policy-result-rule-ids base) :test #'string=)
         :refuse-p t)
        base)))
```

Pass `:policy #'operator-policy`. Rules are ordinary trusted Lisp, not a new rule
language, a model-generated program, or client-controlled configuration. Bump the
policy version whenever rules, model/configuration, or extraction semantics change.

## Basic local LLM harness

`make-local-classifier` accepts an explicit loopback OpenAI-compatible
`/v1/chat/completions` endpoint and model. It has an 8,192-character input ceiling,
32-KiB response ceiling, 2,048-character inner result ceiling, bounded JSON depth and
strings, at most 256 generated tokens, a four-second SBCL wall-clock deadline, and
1–8 configurable concurrent calls (default 2). Capacity exhaustion is an unavailable
assessment, not an unbounded queue. No subprocess, tool call, Lisp reader/evaluator,
redirect, remote fallback, or persistent worker is created. The model returns only
`{"signals":[...]}` from a closed vocabulary; unknown/duplicate keys, unknown or
duplicate labels, unfinished generations, extra content, or tool calls are rejected.

The local model receives **bounded intent text**, which may still contain personal
information. The CouchDB record does not. Disable prompt/request logging in the
model service; local transport alone is not a guarantee about that service's logs.
No external model is selected by default. For another provider, supply a trusted
bounded callback and explicitly review its data handling; request parameters cannot
choose an endpoint. Prompt instructions are one precaution, not a claimed prompt-
injection proof: the important boundary is untrusted typed evidence versus trusted
Lisp decision logic. Never treat a request's claim of consent as verified permission.

## Coverage and limits

Explicit installation wraps the existing `authorize-document!`,
`authorized-update-document`, `authorized-search-query` and condition adapter
`authorize!` service functions and composes the current CLOS policy engine. No
production source function is overwritten at file load.

Search text is inspected. For `target`, `operation`, and `research-node` documents,
only `data.objective`, `mission`, `query`, `prompt`, and `instructions` are treated as
intent. Incoming update patches are inspected using the existing document's dtype
when omitted; the old document's text is not mistaken for the newly submitted intent.
Bulk authorization remains a preflight before application publication. Raw posts,
normal document bodies, arbitrary headers, target identifiers and credentials are
**not** fed to the detector. Existing authorization auditing handles other actions.

This is NOT comprehensive traffic inspection. Missing intent fields produce no risk
assessment (or `no-input` for a patch); they do not certify safety. Direct backend
access, code bypassing these authorization services, scheduled jobs outside the
wrapped call, and response/exfiltration inspection are not covered. A trusted
additional adapter can bind `star.request-audit:*input-text*`, `*request-tenant*`
and an optional `*subject-key*` around its normal authorization call. Never populate
those bindings from unauthenticated headers or client assertions of trust.

The initial lexical detector is a small **English heuristic**. It can miss obfuscated,
multilingual, gradual or contextual misuse, and can flag quotations/negations or
legitimate research. No precision/recall score is claimed. Review false positives
before enabling enforcement. Findings mean **potential risk**, not proven doxxing,
malicious intent, or a basis for automatic account punishment.

The classifier is synchronous (up to four seconds); the reused CouchDB transport can
add up to five seconds. This is a bounded basic integration, not a throughput claim
or an asynchronous actor scheduler. Apply existing admission/rate controls. Base
policy work such as quota accounting may precede the refusal layer; protected
publication/update callbacks do not run after refusal in the tested service paths.

## Audit data, map/reduce, and viewing

Records contain event/time/action, exact configured tenant, HMAC-keyed
principal/subject/correlation references, key/policy version, local/model signals,
flags, rule IDs, status, mode and final proposed enforcement decision. No raw request,
social post, password, URL, address, model rationale or model prose is permitted.
HMACs are tenant/domain separated; they are linkable pseudonyms, not anonymous data.
An event's SHA-256 ID supports collision/replay checks, not signatures or WORM proof.

Use the same global CLI arguments with these subcommands:

```sh
# Latest risk flags, and what would have been refused in audit mode.
python3 tools/request-audit/request_audit.py ... list --flag potential-doxxing-setup
python3 tools/request-audit/request_audit.py ... list --decision would-refuse
python3 tools/request-audit/request_audit.py ... counts
python3 tools/request-audit/request_audit.py ... decisions
python3 tools/request-audit/request_audit.py ... setups
python3 tools/request-audit/request_audit.py ... show audit-EVENT_SHA256
```

Replace `...` with the URL, tenant, reviewer username and private password-file
arguments shown above; it is not a literal executable argument. JSONL output is
bounded by `--limit` (default 50 for history, 100 for aggregation; max 1,000). Counts
are **authorization-assessment events**, not unique API requests or customers.
`counts` is grouped by day/flag/decision and can count multiple flags per event.
`decisions` counts each event once by day/decision. Results are limited, not an
unbounded/full-history total.

Design `_design/request_audit` includes `by_time`, `by_flag_time`,
`by_decision_time`, `_count` views `counts`/`decisions`, and `_sum` view
`signals_by_subject_hour`. `setups` derives reviewer hints when stages such as
identity linking and personal-detail collection appear for the **same tenant,
principal, pseudonymous subject, key/policy version and hour**. Records lacking a
subject are excluded instead of combining unrelated people. It combines local and
unverified model observations; it is **review-only**, not an enforcement input or
proof of one coherent plan. Different-hour/subject records are not joined. Without
a trusted shared subject key, resource IDs generally correlate only the same resource.

Create-only validation prevents runtime writers from modifying/deleting events or
adding unallowlisted fields; membership and distinct writer/reader roles are
per-database. Views and client-side tenant filters are **not row-level security**.
CouchDB administrators can change the design/security/database; this is not
administrator-resistant immutability. Retention/compaction/backups remain explicit
operator tasks; the addon creates no automatic deletion schedule. Protect reviewer
access and the HMAC key independently; rotate keys with a new `key-id`.

## Verification

`sbcl --script tests/request-audit/core.lisp` exercises pure predicates and hostile
classifier results. `tests/request-audit/runtime.lisp` loads the **real existing
policy/services source** with fake auth/JSON/I/O ports and exercises audit-only,
opt-in refusal, delegated-denial preservation, partial patches, zero-effect bulk
preflight, outages, strict Booleans and uninstall. It is not a full ASDF/HTTP or
cryptographic test. Python/Node fixtures exercise validation, projections,
idempotency and tenant/mode rejection. The separate CouchDB service test exercises
real membership, append-only behavior, replay, and map/reduce results in a disposable
loopback database. The workflow contains no production credentials or deployment.

The full server ASDF/schema/native gate, actual optional Lisp transport/JSON/model
integration, local-model quality tests, production staging and independent five-voter
review remain required before promoting the draft. Static parsing and unit tests
must not be described as those gates. No merge, billing activation or production
refusal opt-in is performed by this change.

Primary design references: Apache CouchDB design documents and database security
(https://docs.couchdb.org/en/stable/ddocs/ddocs.html,
https://docs.couchdb.org/en/stable/api/database/security.html); Jzon API
(https://github.com/Zulu-Inuoe/jzon); OWASP prompt injection prevention
(https://cheatsheetseries.owasp.org/cheatsheets/LLM_Prompt_Injection_Prevention_Cheat_Sheet.html).
