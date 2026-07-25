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
                 (:file "producers/package")
                 (:file "producers/producers")
                 (:file "document-v09-package")
                 (:file "package")
                 (:file "document-v09")
                 (:file "gserver-settings")
                 (:file "databases/couchdb")
                 (:file "databases/couchdb-v09")
                 (:file "init")
                 (:file "actors")
                 (:file "actor-systems/event-actor")
                 (:file "actor-systems/matcher-actor")
                 (:file "rabbit")
                 (:file "frontends/http-api")
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
                 #:bordeaux-threads))
