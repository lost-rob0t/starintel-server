#!/usr/bin/env python3
"""Author docstrings for every undocumented exported symbol and patch them
into the source via insert-docstrings.py.

Mechanical accessor docs are derived from the parsed defstruct/defclass maps;
everything else is hand authored below, keyed by bare lowercase symbol.
"""
import json
import subprocess
import sys

D = {}

# ---------------------------------------------------------------- star.actors
D.update({
 '*active-target-schedules*': 'Table of live target schedules keyed by schedule id.',
 '*actor-event-receiver*': 'Function called with every actor event for external observability.',
 '*producer-agent*': 'Sento agent pinning the RabbitMQ producer connection to one thread.',
 '*target-max-delay-seconds*': 'Upper bound on a scheduled target delay.',
 'accept-target-delivery': 'Record an acceptance decision for a target delivery.',
 'actor-event': 'Event record emitted by the actor runtime for observability.',
 'canonical-target-routing-key': 'Compute the canonical RabbitMQ routing key for a target.',
 'compatibility-target-routing-keys': 'Legacy routing keys accepted alongside the canonical key.',
 'couchdb-delete-request': 'Deferred CouchDB DELETE command executed by the couchdb service actor.',
 'couchdb-delete-request-revision': 'Optimistic concurrency revision for the deferred delete.',
 'couchdb-get-request': 'Deferred CouchDB GET command executed by the couchdb service actor.',
 'couchdb-get-request-revision': 'Expected revision carried by the deferred get, or nil.',
 'couchdb-insert-request': 'Deferred CouchDB insert command executed by the couchdb service actor.',
 'couchdb-insert-request-document': 'The JSON document payload for the deferred insert.',
 'couchdb-result': 'Result envelope returned by couchdb service actor handlers.',
 'couchdb-result-error-message': 'Human readable failure reason, when the result failed.',
 'define-actor': 'Define an actor message handler and register it in the actor index.',
 'encode-actor-event': 'Serialize an actor event to its wire JSON representation.',
 'get-dest-actor': 'Resolve a destination actor symbol by its registered name.',
 'handle-event-message': 'Process one RabbitMQ event message inside the event consumer.',
 'invalid-actor-event': 'Signalled when an actor event fails validation.',
 'invalid-persisted-target': 'Signalled when a persisted target document cannot be reloaded safely.',
 'invalid-target-dispatch': 'Signalled when a target dispatch envelope fails validation.',
 'log-actor-event': 'Emit an actor event through the configured receiver and log it.',
 'make-actor-event': 'Construct a validated actor event record.',
 'make-couchdb-delete-handler': 'Actor handler for deferred CouchDB deletes.',
 'make-couchdb-delete-request': 'Build a deferred CouchDB delete command.',
 'make-couchdb-get-handler': 'Actor handler for deferred CouchDB reads.',
 'make-couchdb-get-request': 'Build a deferred CouchDB read command.',
 'make-couchdb-insert-handler': 'Actor handler for deferred CouchDB inserts.',
 'make-couchdb-insert-request': 'Build a deferred CouchDB insert command.',
 'make-target-dispatch-envelope': 'Build a validated dispatch envelope for a target.',
 'start-couchdb-deletes': 'Start the actor handling deferred CouchDB deletes.',
 'target-acceptance-document': 'The persisted acceptance document for a target.',
 'target-acceptance-equivalent-p': 'True when two acceptance documents describe the same target.',
 'target-acceptance-id': 'Stable identifier of a target acceptance record.',
 'target-command': 'Command message consumed by target actors.',
 'target-destination-handle': 'Resolved destination for a target dispatch (kind + address).',
 'target-destination-handle-compatibility-routing-keys': 'Legacy routing keys kept for the destination handle.',
 'target-destination-unavailable': 'Signalled when the resolved destination cannot accept work.',
 'target-dispatch-envelope': 'Envelope carrying one target dispatch through the runtime.',
 'target-dispatch-envelope-deadline': 'Latest acceptable execution time for the envelope.',
 'target-dispatch-outcome': 'Outcome record of one dispatch attempt.',
 'target-ingress-overloaded': 'Signalled when the target ingress is above its load threshold.',
 'target-outcome-success-p': 'True when a dispatch outcome counts as successful.',
 'target-record': 'A scheduled target: what to run, where, and when.',
 'target-record-lease-expires-at': 'Expiration of the lease guarding this target record.',
 'validate-actor-event': 'Validate an actor event; signal invalid-actor-event on failure.',
 'with-json': 'Macro exposing =val=, =dataset=, =dtype= readers over a JSON object.',
})

# ---------------------------------------------------------------- star.auth
D.update({
 '*auth-clock*': 'Function returning the current universal time; swapped in tests.',
 '*credential-store*': 'Global credential store used when no explicit store is passed.',
 '*request-security-context*': 'Dynamic binding of the current request authentication context.',
 '*verifier-compare-function*': 'Constant-time comparison function; injection point for tests.',
 '+api-key-prefix+': 'Prefix of every presented API key string.',
 'administrator-principal-p': 'True when the principal is an administrator type.',
 'api-key-metadata-json': 'Serialize API key metadata (no secrets) to JSON.',
 'api-key-record': 'Persisted record of one API key credential.',
 'api-key-record-revision': 'Optimistic concurrency revision of the record.',
 'auth-now': 'Current universal time from =*auth-clock*=.',
 'authenticate-oauth-access-token': 'Validate a presented OAuth access token; return its record.',
 'authenticate-user-password': 'Verify a username/password pair; return the user record.',
 'authentication-error': 'Uniform failure signalled for every rejected credential.',
 'bearer-token': 'Extract the token from an =Authorization: Bearer= header value.',
 'constant-time-secret=': 'Constant-time string comparison for presented secrets.',
 'couchdb-credential-store': 'Credential store backed by the CouchDB auth database.',
 'create-oauth-client': 'Register an OAuth client with its redirect URIs and scopes.',
 'create-user': 'Create a human user with a hashed password.',
 'credential-lifecycle-error': 'Signalled for invalid API key lifecycle operations.',
 'credential-store': 'Storage protocol for API key credentials.',
 'credential-store-count': 'Number of credentials in the store.',
 'credential-store-get': 'Fetch the credential record for CREDENTIAL-ID, or nil.',
 'credential-store-list': 'List every credential record in the store.',
 'credential-store-put': 'Insert a new credential record.',
 'credential-store-update': 'Update an existing credential record.',
 'current-principal-id': 'The id of the principal bound to this request, or nil.',
 'current-request-principal': 'The principal bound to this request, or nil.',
 'current-service-call-context': 'Serializable view of the current request context for service calls.',
 'ensure-initial-user': 'Create the bootstrapped user if the store is empty.',
 'exchange-oauth-authorization-code': 'Redeem an authorization code for an access token.',
 'issue-oauth-authorization-code': 'Issue a short-lived authorization code for a client.',
 'list-api-key-metadata': 'Metadata (no secrets) for every stored API key.',
 'list-user-metadata': 'Metadata for every stored user.',
 'make-couchdb-credential-store': 'Build a CouchDB backed credential store.',
 'make-memory-credential-store': 'Build an in-memory credential store (tests and tools).',
 'memory-credential-store': 'In-memory credential store.',
 'normalize-oauth-scopes': 'Normalize a scope list into its canonical string form.',
 'normalize-username': 'Canonicalize a username; signal on invalid characters.',
 'oauth-access-token-record': 'Issued OAuth access token and its metadata.',
 'oauth-authorization-code-record': 'Issued OAuth authorization code and its constraints.',
 'oauth-client-metadata-json': 'Serialize OAuth client metadata (no secrets) to JSON.',
 'oauth-client-record': 'Registered OAuth client application.',
 'oauth-error': 'Signalled for OAuth protocol failures.',
 'pkce-s256-challenge': 'Derive the PKCE S256 challenge for a verifier.',
 'request-principal': 'Who is calling: id, type, scopes and credential id.',
 'request-security-context': 'Authentication facts bound to one request.',
 'revoke-oauth-access-token': 'Revoke an issued OAuth access token.',
 'scope-granted-p': 'True when the current principal holds SCOPE (or admin).',
 'service-call-context': 'The principal context propagated across service boundaries.',
 'signal-authentication-failure': 'Signal the uniform authentication error with CODE and MESSAGE.',
 'user-metadata-json': 'Serialize user metadata (no secrets) to JSON.',
 'user-record': 'Persisted record of one human user.',
 'user-store-count': 'Number of users in the store.',
 'user-store-get': 'Fetch a user record by normalized username, or nil.',
 'user-store-list': 'List every user record in the store.',
 'user-store-put': 'Insert a new user record.',
 'user-store-update': 'Update an existing user record.',
 'valid-https-redirect-uri-p': 'True when a redirect URI is acceptable for the client.',
})

# ---------------------------------------------------------------- star.authorization
D.update({
 '*authorization-audit-sink*': 'Function called with every authorization audit event.',
 '*current-authorization-decision*': 'Dynamic binding of the in-flight authorization decision.',
 '*policy-engine*': 'Global policy engine consulted for every authorization check.',
 '*trusted-authorization-context*': 'Dynamic binding for service-to-service trusted contexts.',
 'authorization-audit-json': 'Serialize an authorization audit event to JSON.',
 'authorization-decision': 'Result of one authorization check: allow/deny plus reasons.',
 'authorization-error': 'Signalled for malformed authorization requests.',
 'authorization-request': 'One authorization question: principal, action, resource.',
 'authorization-resource': 'The resource side of an authorization request.',
 'authorize': 'Evaluate POLICY-ENGINE against REQUEST; return a decision.',
 'authorize!': 'Like =authorize= but signals on denial.',
 'authorize-document!': 'Authorize an action on a document resource.',
 'capability-granted-p': 'True when the principal holds the requested capability.',
 'decision-rabbit-headers': 'RabbitMQ headers encoding an authorization decision.',
 'default-deny-policy-engine': 'Policy engine that denies unless a rule explicitly allows.',
 'evaluate-authorization': 'Run the policy engine; returns the decision without signalling.',
 'make-authorization-resource': 'Build an authorization resource description.',
 'make-trusted-authorization-context': 'Build a trusted context for internal service calls.',
 'policy-engine': 'Protocol object answering authorization questions.',
 'principal-has-capability-p': 'True when the principal record grants CAPABILITY.',
 'principal-has-wildcard-dataset-p': 'True when the principal may access any dataset.',
 'resource-from-document': 'Derive an authorization resource from a document.',
 'scope-values': 'Expand a scope string into its component values.',
 'trusted-authorization-context': 'Pre-authenticated context for internal call chains.',
 'validate-grant-scopes': 'Check that requested grant scopes are well formed.',
 'with-trusted-authorization-context': 'Evaluate BODY with a trusted context bound.',
})

# ---------------------------------------------------------------- star.consumers
D.update({
 '*retry-sleep-function*': 'Function used to sleep between retries; swapped in tests.',
 'close-stream': 'Protocol: release the transport resources of STREAM.',
 'conflict-delivery-error': 'Signalled when a delivery conflicts with committed state.',
 'consume': 'Protocol: deliver one decoded message to the consumer.',
 'consumer-cleanup': 'Protocol: release consumer resources on shutdown.',
 'consumer-metrics': 'In-flight, unsettled and failure counters for a consumer.',
 'consumer-read': 'Protocol: read the next delivery from the consumer stream.',
 'consumer-settlement': 'Settlement action applied to one delivery (ack/reject/...).',
 'consumer-settlement-condition': 'Signalled to settle the delivery currently being processed.',
 'consumer-settlement-count': 'Number of settlements applied per settlement kind.',
 'consumer-update': 'Protocol: persist a new consumer state snapshot.',
 'consumer-update-state': 'Update the consumer state under its lock.',
 'create-rabbit-consumer': 'Create and register a RabbitMQ backed consumer.',
 'delivery-attempt': 'Which attempt number this delivery is on.',
 'delivery-error-class': 'Classify an error into a delivery error condition class.',
 'delivery-error-retryable-p': 'True when the delivery error permits another attempt.',
 'delivery-first-seen-at': 'Timestamp when this delivery was first observed.',
 'delivery-message-id': 'Stable message identity used for deduplication.',
 'delivery-processing-error': 'Base condition for failures while processing a delivery.',
 'delivery-trace-id': 'Trace id propagated with the delivery.',
 'internal-delivery-error': 'Signalled when the consumer itself fails (retryable).',
 'make-consumer': 'Construct a consumer over a stream with retry and settlement policy.',
 'make-rabbit-worker-consumer': 'Create one worker consumer instance for a multi-worker consumer.',
 'make-retry-policy': 'Build a bounded exponential retry policy.',
 'open-stream': 'Protocol: acquire the transport resources of STREAM.',
 'permanent-delivery-error': 'Signalled for deliveries that must never be retried.',
 'quarantine-record': 'Structured record persisted when a message is quarantined.',
 'rabbit-header': 'RabbitMQ message header accessor.',
 'rabbit-property': 'RabbitMQ message property accessor.',
 'retry-action-for': 'Decide the retry action for a failed delivery attempt.',
 'retry-policy': 'Bounded exponential backoff parameters for redeliveries.',
 'retry-properties': 'RabbitMQ properties carrying retry metadata across republish.',
 'retrying-rabbit-consumer': 'RabbitMQ consumer with bounded retry and quarantine.',
 'retrying-rabbit-queue-stream': 'Queue stream abstraction the retrying consumer reads from.',
 'schema-invalid-delivery-error': 'Signalled when a delivery fails schema validation (permanent).',
 'settlement-ack': 'Settle a delivery as acknowledged.',
 'settlement-dead-letter': 'Settle a delivery by sending it to the dead letter queue.',
 'settlement-filtered-ack': 'Settle a delivery as filtered (ack without processing).',
 'settlement-reject': 'Settle a delivery as rejected.',
 'settlement-retry': 'Settle a delivery by scheduling a retry.',
 'start-consumer': 'Start a consumer and its worker threads.',
 'stop-consumer': 'Stop a consumer and join its worker threads.',
 'stream-read': 'Protocol: read one delivery from the stream.',
 'stream-settle': 'Protocol: apply a settlement to one delivery on the stream.',
 'transient-delivery-error': 'Signalled for transient failures (retryable).',
 'unauthorized-delivery-error': 'Signalled when a delivery fails authorization (permanent).',
 'with-consumer-lock': 'Evaluate BODY while holding the consumer lock.',
 'wrong-stream-owner': 'Signalled when a worker touches a stream owned by another worker.',
})

# ---------------------------------------------------------------- star.databases.couchdb
D.update({
 '*couchdb-pool*': 'Connection pool of CouchDB clients.',
 'as-json': 'Serialize a spec object into its JSON document form.',
 'by-channel': 'View query for messages grouped by channel.',
 'couchdb-get-quarantine-record': 'Load one quarantine record by id.',
 'couchdb-load-target-acceptance': 'Load the acceptance state for a target.',
 'couchdb-pending-outbox-documents': 'Outbox documents whose publication is still pending.',
 'couchdb-process-outbox-mutation': 'Apply one outbox mutation to the document store.',
 'couchdb-update-target-acceptance': 'Apply an update to persisted target acceptance state.',
 'couchdb-view-request': 'A fully built CouchDB view HTTP request.',
 'couchdb-view-request-body': 'JSON body of the view request, or nil for GET.',
 'count-by-dtype': 'Count documents grouped by their =dtype=.',
 'dataset-size': 'Number of documents in one dataset.',
 'document-outbox-entries': 'List outbox entries attached to a document.',
 'document-update-outcome': 'Result of applying one document update.',
 'document-update-outcome-code': 'Outcome code such as =:applied= or =:conflict=.',
 'document-update-outcome-json': 'Serialize a document update outcome to JSON.',
 'document-update-store-conflict': 'Signalled when a document update loses a revision race.',
 'document-update-validation-error': 'Signalled when a document update payload is invalid.',
 'documents-by-dataset': 'View query listing documents of one dataset.',
 'find-outbox-entry': 'Find a specific outbox entry on a document.',
 'format-key': 'Convert a slot name into its CouchDB JSON key.',
 'from-json': 'Populate a spec object from its JSON document form.',
 'get-targets*': 'Fetch target documents, optionally filtered by actor.',
 'groups': 'Grouped reduce rows from a view.',
 'mark-quarantine-replayed': 'Mark a quarantine record as successfully replayed.',
 'messages-by-group': 'View query for messages belonging to a group.',
 'messages-by-platform': 'View query for messages belonging to a platform.',
 'messages-by-user': 'View query for messages belonging to a user.',
 'missing-document-for-update': 'Signalled when an update targets a nonexistent document.',
 'mutation-conflict': 'Signalled when an outbox mutation conflicts with store state.',
 'orgs-by-country': 'View query for organizations grouped by country.',
 'orgs-by-name': 'View query for organizations by name.',
 'outbox-entry-mutation-id': 'Stable id of the mutation carried by the outbox entry.',
 'outbox-entry-published-p': 'True when the outbox entry has been published.',
 'outbox-entry-sequence': 'Per-document monotonic sequence of the outbox entry.',
 'outbox-store-conflict': 'Signalled when writing an outbox entry conflicts.',
 'persist-outbox-mutation': 'Persist a mutation into the document outbox before publishing.',
 'persons-by-name': 'View query for persons by name.',
 'persons-by-region': 'View query for persons grouped by region.',
 'recover-couchdb-outbox': 'Replay every pending outbox mutation after a crash.',
 'registered-view-names': 'Names of all views known to the registry.',
 'registered-view-spec': 'The registered spec for a view name, or nil.',
 'relations-edges': 'View query returning relation edges.',
 'relations-incoming-count': 'Count of incoming relations for a node.',
 'relations-outgoing-count': 'Count of outgoing relations for a node.',
 'social-posts-by-group': 'View query for social posts belonging to a group.',
 'social-posts-by-platform': 'View query for social posts on a platform.',
 'social-posts-by-user': 'View query for social posts by a user.',
 'target-acceptance-store-conflict': 'Signalled when writing target acceptance state conflicts.',
 'targets-actor-counts': 'Count of targets grouped by owning actor.',
 'targets-by-actor': 'View query for targets owned by an actor.',
 'targets-target-count': 'Count of targets for a given target id.',
 'update-quarantine-record': 'Update a quarantine record in place.',
 'users-by-platform': 'View query for users on a platform.',
 'view-document-result': 'View result holding included documents.',
 'view-document-result-rows': 'Raw rows of a document view result.',
 'view-map-result': 'View result holding map rows (key/value pairs).',
 'view-map-result-rows': 'Raw rows of a map view result.',
 'view-query-error': 'Signalled when a view query fails.',
 'view-reduced-result': 'View result holding reduced values.',
 'view-reduced-result-rows': 'Raw rows of a reduced view result.',
 'view-registry-error': 'Base condition for view registry failures.',
 'view-registry-matrix': 'Allowed keyword/verb combinations per view kind.',
 'view-result-value': 'Extract the value cell of a view result row.',
 'view-spec': 'Validated specification of one CouchDB view.',
 'view-spec-accepted-keywords': 'Keywords accepted for a given view kind.',
})

# ---------------------------------------------------------------- star.documents
D.update({
 'canonical-dtype': 'Canonical string form of a document dtype.',
 'clone-document-object': 'Deep copy a parsed document object.',
 'document-data': 'The =data= payload of a document object.',
 'document-dataset': 'The =dataset= field of a document object.',
 'document-date-added': 'The =dateAdded= field of a document object.',
 'document-date-updated': 'The =dateUpdated= field of a document object.',
 'document-dtype': 'The =dtype= field of a document object.',
 'document-id': 'The =_id= of a document object.',
 'document-schema-validation-error': 'Signalled when a document fails v0.9 schema validation.',
 'document-transient-p': 'True when the document is marked transient.',
 'document-value': 'Generic accessor into a document object cell.',
 'object-has-key-p': 'True when a parsed JSON object contains KEY.',
 'object-keys': 'Keywords of a parsed JSON object.',
 'object-value': 'Fetch the value at KEY of a parsed JSON object.',
 'parse-document-object': 'Parse a document payload into the internal JSOWN form.',
 'utc-now': 'Current UTC timestamp as an ISO-8601 string.',
})

# ---------------------------------------------------------------- star.http.contract
D.update({
 'all-http-operations': 'Every operation in the client contract manifest.',
 'client-manifest-document': 'The full client manifest as a JSON document.',
 'client-manifest-json': 'The full client manifest serialized to JSON.',
 'find-http-operation': 'Look up one contract operation by id.',
 'http-operation': 'One documented HTTP operation of the client contract.',
 'http-operation-authority': 'Authority (host base) of the operation.',
 'http-operation-client-name': 'Client library this operation belongs to.',
 'http-operation-id': 'Stable id of the operation.',
 'http-operation-idempotency': 'Idempotency classification of the operation.',
 'http-operation-method': 'HTTP method of the operation.',
 'http-operation-path': 'Path template of the operation.',
 'http-operation-path-parameters': 'Path parameters and their constraints.',
 'http-operation-request-schema': 'JSON schema of the request body, or nil.',
 'http-operation-responses': 'Response schema/status map of the operation.',
 'http-operation-scopes': 'Scopes required to call the operation.',
 'http-operation-summary': 'Human summary of the operation.',
 'http-operation-tags': 'Tags grouping the operation.',
 'openapi-document': 'The OpenAPI document for the client contract.',
 'openapi-json': 'The OpenAPI document serialized to JSON.',
 'openapi-path': 'Path entry of the OpenAPI document.',
 'operation-request-symbol-name': 'Client symbol name generated for the operation.',
})

# ---------------------------------------------------------------- star.leases
D.update({
 '+lease-identifier-max-bytes+': 'Maximum UTF-8 size of a lease identity component.',
 '+lease-metadata-max-bytes+': 'Maximum UTF-8 size of the lease metadata document.',
 '+lease-metadata-max-keys+': 'Maximum number of keys in lease metadata.',
 '+lease-outcome-codes+': 'Every outcome code the lease protocol may return.',
 '+lease-reason-max-bytes+': 'Maximum UTF-8 size of a lease revoke reason.',
 '+lease-record-version+': 'Schema version of persisted lease records.',
 '+retryable-lease-outcome-codes+': 'Outcome codes callers may retry under bounded backoff.',
 'acquire-lease': 'Protocol: acquire a lease for IDENTITY on STORE.',
 'backend-health': 'Protocol: probe the health of the lease backend.',
 'canonical-target-lock-key': 'Deterministic lock key derived from a lease identity.',
 'close-lease-runtime': 'Shut down a lease runtime; idempotent.',
 'close-lease-store': 'Protocol: release backend resources of STORE.',
 'deserialize-lease-record': 'Decode a persisted lease record from its JSON form.',
 'get-lease': 'Protocol: fetch the current lease for IDENTITY, if any.',
 'lease-identity': 'Canonical identity of a lease (tenant, program, target, actor, ...).',
 'lease-identity-operation-class': 'Operation class component of the lease identity.',
 'lease-outcome': 'Unified result of any lease protocol call.',
 'lease-outcome-detail': 'Free-form detail attached to the outcome.',
 'lease-record': 'Persisted state of one lease, including fencing token.',
 'lease-record-state': 'Lifecycle state of the lease record.',
 'lease-runtime': 'Store plus lifecycle flag shared by lease users.',
 'lease-store': 'Protocol base class for lease backends.',
 'list-leases': 'Protocol: list leases filtered by owner/target/program.',
 'make-lease-identity': 'Build and normalize a lease identity.',
 'make-lease-outcome': 'Build a lease outcome; validates the code.',
 'make-lease-record': 'Build a persisted lease record.',
 'make-lease-runtime': 'Wrap a store into a lease runtime.',
 'make-valkey-lease-store': 'Build a Valkey backed lease store.',
 'memory-lease-store': 'In-memory lease store for tests and tools.',
 'release-lease': 'Protocol: release a lease using its fencing token.',
 'renew-lease': 'Protocol: extend a lease; returns a new fencing token.',
 'retryable-lease-outcome-code-p': 'True when the outcome code is retryable.',
 'revoke-lease': 'Protocol: administratively revoke a lease with a reason.',
 'serialize-lease-record': 'Encode a lease record into its persisted JSON form.',
 'utf-8-byte-length': 'Length of STRING in UTF-8 bytes.',
 'valkey-lease-store': 'Valkey (Redis compatible) lease backend.',
})

# ---------------------------------------------------------------- star.producers
D.update({
 'make-producer': 'Build a RabbitMQ producer bound to an exchange and broker.',
 'producer-connect': 'Open (or reuse) the producer connection.',
 'publish': 'Publish one message body through the producer.',
 'with-producer-lock': 'Evaluate BODY while holding the producer lock.',
})

# ---------------------------------------------------------------- star.rabbit
D.update({
 '+documents-exchange+': 'Name of the durable documents topic exchange.',
 '+documents-exchange-type+': 'Exchange type for documents (topic).',
 '+ingest-fmt-key+': 'Format-specific ingest routing key.',
 '+ingest-key+': 'Canonical ingest routing key.',
 '+ingest-queue+': 'Queue consuming canonical new-document events.',
 '+new-documents-fmt-key+': 'Format-specific new-document routing key.',
 '+new-documents-key+': 'Canonical new-document routing key.',
 '+targets-key+': 'Canonical target routing key.',
 '+targets-queue+': 'Queue consuming target messages.',
 '+update-key+': 'Canonical update routing key.',
 '+updated-documents-fmt-key+': 'Format-specific update routing key.',
 '+updated-documents-key+': 'Canonical updated-document routing key.',
 '+updates-queue+': 'Queue consuming document update events.',
 'handle-target': 'Process one RabbitMQ target message.',
 'handle-update-document': 'Ingest consumer entry point: process MESSAGE as =:updated=.',
 'inspect-quarantine': 'List or show quarantined messages for operator inspection.',
 'replay-quarantined-message': 'Replay one quarantined message through its original route.',
})

# ---------------------------------------------------------------- star (server settings)
D.update({
 '*oauth-access-token-seconds*': 'Lifetime of issued OAuth access tokens.',
 '*oauth-authorization-code-seconds*': 'Lifetime of issued OAuth authorization codes.',
})

# ---------------------------------------------------------------- addons / http-api
D.update({
 'addon-error': 'Signalled when an addon fails to load or start.',
 'addon-state': 'Runtime state of one loaded addon.',
 'addon-state-last-error': 'The last error raised by the addon, or nil.',
 'addon-status': 'Status keyword of the addon (loaded, failed, ...).',
 'list-addons': 'List every registered addon and its state.',
 '*app*': 'The Ningle application instance serving the HTTP API.',
 '*default-headers*': 'Default response headers applied to API responses.',
 'bounded-query-integer': 'Parse an integer query parameter clamped to sane bounds.',
 'bulk-ingest-job': 'One bulk ingest job: documents plus options.',
 'bulk-request-mode': 'How a bulk request behaves on failure (atomic or best effort).',
 'execute-bulk-job': 'Apply every document of a bulk job; returns per-document outcomes.',
 'http-input-error': 'Signalled for malformed HTTP input payloads.',
 'json-array-p': 'True when the parsed payload is a JSON array.',
 'json-object-p': 'True when the parsed payload is a JSON object.',
 'parse-json-octets': 'Parse a request body (octets) into a JSON value.',
 'validate-document-input': 'Validate a document payload from HTTP input.',
})


def mechanical_accessors():
    """Build docstrings for struct/class accessors from the parse map."""
    plan = json.load(open('/tmp/opencode/docplan.json'))
    accessors = plan['accessors']
    # A hand doc beats a mechanical one.
    docs = {}
    for name, kind in plan['hit']:
        if name in D:
            docs[name] = D[name]
            continue
        slot, record, _path = accessors[name]
        docs[name] = f"The ={slot}= slot of ={record}=."
    return docs


def main():
    docs = mechanical_accessors()
    for k, v in D.items():
        docs[k.lower()] = v
    json.dump(docs, open('/tmp/opencode/alldocs.json', 'w'), indent=1)
    print(f"total doc entries: {len(docs)}")
    if '--patch' in sys.argv:
        result = subprocess.run(
            [sys.executable, 'scripts/insert-docstrings.py',
             '/tmp/opencode/alldocs.json', 'source/**/*.lisp',
             'source/*.lisp'],
            capture_output=True, text=True)
        print(result.stdout[-3000:])
        if result.returncode != 0:
            print(result.stderr[-2000:])
            sys.exit(1)


if __name__ == '__main__':
    main()
