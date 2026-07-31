;; [[file:../source.org::*Namespace setup][Namespace setup:2]]
(uiop:define-package :starintel-gserver
  (:nicknames :star)
  (:use :cl)
  (:export
   #:*rabbit-password*
   #:*rabbit-user*
   #:*rabbit-port*
   #:*rabbit-address*
   #:*http-scheme*
   #:*http-key-file*
   #:*http-cert-file*
   #:*http-api-base-path*
   #:*http-api-port*
   #:*http-api-address*
   #:*http-cors-allowed-origins*
   #:*http-cors-allowed-methods*
   #:*http-cors-allowed-headers*
   #:*couchdb-default-database*
   #:*couchdb-auth-database*
   #:*couchdb-host*
   #:*couchdb-port*
   #:*couchdb-user*
   #:*couchdb-password*
   #:*couchdb-scheme*
   #:*auth-mode*
   #:*auth-pepper*
   #:*auth-bootstrap-secret*
   #:*auth-dev-bypass*
   #:*auth-key-secret-bytes*
   #:*auth-salt-bytes*
   #:*auth-rotation-overlap-max-seconds*
   #:*auth-default-request-timeout-ms*
   #:*auth-max-request-timeout-ms*
   #:*auth-public-paths*
   #:main
   #:reload
   #:start-debugger
   #:*slynk-port*
   #:*actors-start-hook*
   #:*document-patterns*
   #:*ingest-workers*
   #:*couchdb-event-log-database*
   #:*couchdb-views*
   #:*star-server-version*
   #:ensure-init-file-exists
   #:load-init-file
   #:safe-load-init
   #:*bulk-max-documents*
   #:repl/main))

(uiop:define-package :star.databases.couchdb
  (:use :cl-couch :cl :star #:lparallel)
  (:export
   #:init-db
   #:init-views
   #:init-event-db
   #:get-targets*
   #:get-view-docs
   #:query-view
   #:map-view-results
   #:get-neighbors
   #:search-fts
   #:sort-docs-by-date
   #:messages-by-user
   #:messages-by-platform
   #:messages-by-group
   #:social-posts-by-user
   #:social-posts-by-group
   #:by-channel
   #:export-by-dataset*
   #:count-by-dtype
   #:dataset-size
   #:total-documents-since
   #:orgs-by-country
   #:orgs-by-name
   #:persons-by-name
   #:persons-by-region
   #:relations-edges
   #:relations-incoming-count
   #:relations-outgoing-count
   #:targets-actor-counts
   #:targets-by-actor
   #:targets-target-count
   #:users-by-platform
   #:users-by-name
   #:as-json
   #:format-key
   #:from-json
   #:*couchdb-pool*
   #:groups
   #:lazy
   #:events-by-actor
   #:document-events
   #:target-events
   #:hosts-by-ip
   #:hosts-by-port
   #:hosts-by-service
   #:emails-by-email
   #:emails-by-domain
   #:emails-with-password
   #:domains-by-record
   #:domains-by-resolved-address
   #:networks-by-asn
   #:networks-by-org
   #:breaches-by-size
   #:urls-by-url
   #:urls-by-path
   #:urls-by-domain)
  (:documentation "CouchDB persistence and query helpers."))
;; Namespace setup:2 ends here

(uiop:define-package :star.auth
  (:use :cl)
  (:export
   #:+api-key-prefix+
   #:authentication-error
   #:authentication-error-code
   #:authentication-error-message
   #:credential-lifecycle-error
   #:credential-lifecycle-error-code
   #:credential-lifecycle-error-message
   #:request-principal
   #:request-principal-id
   #:request-principal-type
   #:request-principal-scopes
   #:request-principal-credential-id
   #:request-security-context
   #:request-security-context-principal
   #:request-security-context-correlation-id
   #:request-security-context-deadline
   #:request-security-context-authenticated-at
   #:service-call-context
   #:service-call-context-principal-id
   #:service-call-context-principal-type
   #:service-call-context-credential-id
   #:service-call-context-scopes
   #:service-call-context-correlation-id
   #:service-call-context-deadline
   #:api-key-record
   #:api-key-record-id
   #:api-key-record-owner
   #:api-key-record-principal-type
   #:api-key-record-scopes
   #:api-key-record-status
   #:api-key-record-salt
   #:api-key-record-verifier
   #:api-key-record-created-at
   #:api-key-record-expires-at
   #:api-key-record-disabled-at
   #:api-key-record-revoked-at
   #:api-key-record-rotation-parent-id
   #:api-key-record-superseded-by
   #:api-key-record-overlap-expires-at
   #:api-key-record-revision
   #:credential-store
   #:memory-credential-store
   #:couchdb-credential-store
   #:make-memory-credential-store
   #:make-couchdb-credential-store
   #:credential-store-get
   #:credential-store-put
   #:credential-store-update
   #:credential-store-list
   #:credential-store-count
   #:*credential-store*
   #:*request-security-context*
   #:*auth-clock*
   #:*verifier-compare-function*
   #:auth-now
   #:constant-time-octets=
   #:constant-time-secret=
   #:parse-api-key
   #:bearer-token
   #:signal-authentication-failure
   #:authenticate-api-key
   #:authenticate-authorization-header
   #:current-request-principal
   #:current-principal-id
   #:current-service-call-context
   #:scope-granted-p
   #:administrator-principal-p
   #:create-api-key
   #:bootstrap-api-key
   #:rotate-api-key
   #:revoke-api-key
   #:disable-api-key
   #:api-key-metadata-json
   #:list-api-key-metadata
   #:validate-auth-configuration
   #:initialize-auth-store))

;; [[file:../source.org::*Namespace setup][Namespace setup:3]]
(uiop:define-package :star.rabbit
  (:use :cl :star.consumers :sento.actor)
  (:documentation "RabbitMQ namespace")
  (:export
   #:with-rabbit-send
   #:with-rabbit-recv
   #:emit-document
   #:+ingest-queue+
   #:+updates-queue+
   #:+ingest-key+
   #:+update-key+
   #:+targets-key+
   #:transient-p
   #:test-make-doc
   #:test-send
   #:start-consumers
   #:+documents-exchange+
   #:+documents-exchange-type+
   #:+ingest-updates-queue+
   #:+ingest-fmt-key+
   #:+new-documents-key+
   #:+new-documents-fmt-key+
   #:+updated-documents-key+
   #:+updated-documents-fmt-key+
   #:+new-targets-key+
   #:+targets-fmt-key+
   #:+ingest-topic-key+
   #:+updates-topic-key+))
;; Namespace setup:3 ends here

(uiop:define-package :star.actors
  (:use :cl :star.databases.couchdb :sento.agent :sento.actor
        :sento.actor-system :sento.actor-context)
  (:documentation "Actor runtime namespace")
  (:export
   #:register-actor
   #:*targets*
   #:*couchdb-gets*
   #:*couchdb-inserts*
   #:*sys*
   #:start-actors
   #:define-actor
   #:with-json
   #:emit
   #:*producer-agent*
   #:*url-extractor*
   #:*pattern-agent*
   #:*pattern-actor*
   #:*wmn-relations-p*
   #:publish
   #:handle-event-message
   #:start-event-consumer
   #:log-actor-event
   #:*event-consumer*
   #:*actor-event-receiver*
   #:make-actor-event
   #:actor-event
   #:event-timestamp
   #:event-actor-name
   #:event-type
   #:event-details
   #:event-source-document
   #:event-id))

;; [[file:../source.org::*Namespace setup][Namespace setup:4]]
(uiop:define-package :starintel-gserver-http-api
  (:nicknames :star.frontends.http-api)
  (:use :cl :ningle :anypool :star.databases.couchdb :star)
  (:documentation "StarIntel HTTP API.")
  (:export
   #:*app*
   #:*default-headers*))
;; Namespace setup:4 ends here
