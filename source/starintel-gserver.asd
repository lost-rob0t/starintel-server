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
                 (:file "consumers/consumers" :depends-on ("consumers/package"))
                 (:file "package")
                 (:file "couchdb")
                 (:file "gserver-settings" :depends-on ("package"))
                 (:file "init" :depends-on ("gserver-settings"))
                 (:file "actors" :depends-on ("gserver-settings"))
                 (:file "rabbit" :depends-on ("actors" "gserver-settings" "consumers/consumers"))
                 (:file "frontends/http-api" :depends-on ("gserver-settings"))
                 (:file "main" :depends-on ("actors" "rabbit" "package" "gserver-settings" "frontends/http-api")))

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
                 #:hunchentoot
                 ;; Need to readd cli parsing
                 #:clingon
                 ;; Move to config
                 #:slynk
                 #:lparallel
                 #:cl-stream
                 #:bordeaux-threads))
;; ASDF:1 ends here
