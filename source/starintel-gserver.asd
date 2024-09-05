;; [[file:../source.org::*ASDF][ASDF:1]]
(asdf:defsystem :starintel-gserver
  :version      "0.1.0"
  :description  "hackable/moddable starintel acess api."
  :author       "nsaspy@airmail.cc"
  :license      "GPL v3"
  :serial t
  :build-operation program-op
  :build-pathname "star-server" ;; shell name
  :entry-point "star::main" ;; thunk
  :components   (
                 (:file "consumers/package")
                 (:file "consumers/consumers")
                 (:file "package")
                 (:file "gserver-settings")
                 (:file "producers/package")
                 (:file "producers/producers")
                 (:file "couchdb")
                 (:file "init")
                 (:file "actors")
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
                 #:cl-json
                 :uuid
                 ;; Not using
                 #:anypool
                 #:clack
                 #:ningle
                 ;; Will use wookie
                 #:clingon
                 ;; Move to config
                 #:slynk
                 #:nhooks
                 #:lparallel
                 #:cl-stream
                 #:cl-ppcre
                 #:bordeaux-threads))
                 
;; ASDF:1 ends here
