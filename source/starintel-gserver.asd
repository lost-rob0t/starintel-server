(asdf:defsystem :starintel-gserver
  :version "0.2.0"
  :description "Hackable StarIntel processing, authorization, and access server."
  :author "nsaspy@airmail.cc"
  :license "GPL-3.0-or-later"
  :serial t
  :build-operation program-op
  :build-pathname "star-server"
  :entry-point "star::main"
  :in-order-to ((test-op (test-op "starintel-gserver-tests")))
  :components
  ((:file "document-access-package")
   (:file "document-access")
   (:file "consumers/package")
   (:file "consumers/consumers")
   (:file "consumers/owner-fixes")
   (:file "consumers/retry-policy")
   (:file "producers/package")
   (:file "producers/producers")
   (:file "package")
   (:file "leases/package")
   (:file "leases/protocol")
   (:file "leases/memory-store")
   (:file "leases/valkey-scripts")
   (:file "leases/valkey-store")
   (:file "leases/valkey-list-store")
   (:file "gserver-settings")
   (:file "databases/couchdb")
   (:file "databases/view-request")
   (:file "databases/export")
   (:file "databases/outbox")
   (:file "databases/view-registry-package")
   (:file "databases/view-registry")
   (:file "databases/document-update-package")
   (:file "databases/document-update")
   (:file "databases/quarantine")
   (:file "databases/target-acceptance")
   (:file "auth/core")
   (:file "auth/verification-hardening")
   (:file "auth/immutability")
   (:file "auth/store")
   (:file "couchdb-session-hardening")
   (:file "auth/users")
   (:file "authorization/package")
   (:file "authorization/policy")
   (:file "authorization/quota-policy")
   (:file "authorization/services")
   (:file "init-loader")
   (:file "actors")
   (:file "actors/couchdb-service")
   (:file "target-repository")
   (:file "target-recovery")
   (:file "target-dispatch")
   (:file "target-dispatch-fixes")
   (:file "actor-systems/event-actor")
   (:file "actor-systems/matcher-actor")
   (:file "rabbit")
   (:file "frontends/http-api")
   (:file "frontends/http-document-update")
   (:file "frontends/http-view-registry")
   (:file "frontends/http-status-message")
   (:file "frontends/http-boundary-core")
   (:file "frontends/http-auth")
   (:file "frontends/http-authorization")
   (:file "frontends/http-bulk-jobs")
   (:file "frontends/http-boundary-routes")
   (:file "frontends/http-auth-routes")
   (:file "frontends/http-auth-job-routes")
   (:file "frontends/http-authorization-routes")
   (:file "frontends/http-authorization-view-routes")
   (:file "runtime-lifecycle")
   (:file "main")
   (:file "authorization/services-final"))
  :depends-on
  (#:starintel
   #:cl-couch
   #:serapeum
   #:alexandria
   #:cl-rabbit
   #:sento
   #:babel
   #:yason
   #:ironclad
   #:dexador
   #:quri
   #:uuid
   #:anypool
   #:clack
   #:lack/middleware/accesslog
   #:clack-handler-hunchentoot
   #:ningle
   #:clingon
   #:slynk
   #:nhooks
   #:lparallel
   #:cl-stream
   #:cl-ppcre
   #:cms-ulid
   #:bordeaux-threads
   #:usocket
   #:cl+ssl
   #:jsown
   #:closer-mop))

;;;; StarIntel Gserver is a processing framework for StarIntel documents.
;;;; Runtime documentation lives in ../docs and must track behavior changes.
