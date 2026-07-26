(asdf:defsystem :starintel-gserver
  :version "0.2.0"
  :description "Hackable StarIntel processing and access server."
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
   (:file "gserver-settings")
   (:file "databases/couchdb")
   (:file "databases/outbox")
   (:file "databases/view-registry-package")
   (:file "databases/view-registry")
   (:file "databases/document-update-package")
   (:file "databases/document-update")
   (:file "databases/quarantine")
   (:file "databases/target-acceptance")
   (:file "init-loader")
   (:file "actors")
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
   (:file "main"))
  :depends-on
  (#:starintel
   #:cl-couch
   #:serapeum
   #:alexandria
   #:cl-rabbit
   #:sento
   #:babel
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
   #:jsown
   #:closer-mop
   #:ironclad))
