(asdf:defsystem :starintel-gserver
  :version "0.1.0"
  :description "Hackable StarIntel document and actor service."
  :author "nsaspy@airmail.cc"
  :license "GPL v3"
  :serial t
  :build-operation program-op
  :build-pathname "star-server"
  :entry-point "star::main"
  :in-order-to ((test-op (test-op "starintel-gserver-tests")))
  :components
  ((:file "consumers/package")
   (:file "consumers/consumers")
   (:file "consumers/rabbit-settlement")
   (:file "producers/package")
   (:file "producers/producers")
   (:file "package")
   (:file "gserver-settings")
   (:file "databases/couchdb")
   (:file "databases/export")
   (:file "auth/core")
   (:file "auth/store")
   (:file "init-loader")
   (:file "actors")
   (:file "actors/couchdb-service")
   (:file "actor-systems/event-actor")
   (:file "actor-systems/matcher-actor")
   (:file "rabbit")
   (:file "frontends/http-api")
   (:file "frontends/http-boundary-core")
   (:file "frontends/http-auth")
   (:file "frontends/http-bulk-jobs")
   (:file "frontends/http-boundary-routes")
   (:file "frontends/http-auth-routes")
   (:file "frontends/http-auth-job-routes")
   (:file "main"))
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
   #:bordeaux-threads))

;;;; StarIntel Gserver is a processing framework for StarIntel documents.
;;;; Runtime documentation lives in ../docs and must track behavior changes.
