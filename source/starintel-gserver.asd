(asdf:defsystem :starintel-gserver
  :version      "0.9.0"
  :description  "Hackable StarIntel v0.9 processing and access server."
  :author       "nsaspy@airmail.cc"
  :license      "GPL-3.0-or-later"
  :serial t
  :build-operation program-op
  :build-pathname "star-server"
  :entry-point "star::main"
  :components   ((:file "consumers/package")
                  (:file "consumers/consumers")
                  (:file "consumers/owner-fixes")
                  (:file "consumers/retry-policy")
                  (:file "producers/package")
                  (:file "producers/producers")
                  (:file "document-v09-package")
                  (:file "package")
                  (:file "document-schema-profile")
                  (:file "document-v09")
                  (:file "document-codec")
                  (:file "gserver-settings")
                  (:file "databases/couchdb")
                  (:file "databases/couchdb-v09")
                  (:file "databases/outbox")
                  (:file "databases/view-registry-package")
                  (:file "databases/view-registry")
                  (:file "databases/document-update-package")
                  (:file "databases/document-update")
                  (:file "databases/quarantine")
                  (:file "databases/target-acceptance")
                  (:file "init")
                  (:file "actors")
                  (:file "actors-v09")
                  (:file "target-recovery")
                  (:file "target-repository")
                  (:file "target-dispatch")
                  (:file "target-dispatch-fixes")
                  (:file "actor-systems/event-actor")
                  (:file "actor-systems/matcher-actor")
                  (:file "rabbit")
                  (:file "frontends/http-api")
                  (:file "frontends/http-api-v09")
                  (:file "frontends/http-document-update")
                  (:file "frontends/http-view-registry")
                  (:file "main"))
  :depends-on   (#:starintel
                  #:cl-couch
                  #:serapeum
                  #:alexandria
                  #:cl-rabbit
                  #:sento
                  #:babel
                  #:uuid
                  #:anypool
                  #:clack
                  #:ningle
                  #:clingon
                  #:slynk
                  #:nhooks
                  #:lparallel
                  #:cl-stream
                  #:cl-ppcre
                  #:cms-ulid
                  #:bordeaux-threads
                  #:closer-mop
                  #:ironclad))
