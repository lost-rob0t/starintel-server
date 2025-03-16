(asdf:defsystem :starintel-gserver
  :version      "0.1.0"
  :description  "hackable/moddable starintel acess api."
  :author       "nsaspy@airmail.cc"
  :license      "GPL v3"
  :serial t
  :build-operation program-op
  :build-pathname "star-server" ;; shell name
  :entry-point "star::main"     ;; thunk
  :components   (
                 (:file "consumers/package")
                 (:file "consumers/consumers")
                 (:file "producers/package")
                 (:file "producers/producers")
                 (:file "package")
                 (:file "gserver-settings")
                 (:file "databases/couchdb")
                 (:file "init")
                 ;; FIXME Once eventstream is finished
                 ;; (:file "actor-systems/event-actor")
                 ;; (:file "actor-systems/matcher-actor")
                 (:file "actor-systems/rabbit-service")
                 (:file "actor-systems/couchdb-service")
                 (:file "actors")
                 (:file "frontends/middleware")
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
;;;; * Starintel Gserver
;;;;@include "gserver-settings.lisp"
;;;; * Warning
;;;;  Please do not use starintel-gserver in production! it is a PROTYPE, or really any star-* thing. they are subject to change as i correct or find better ideas.
;;;;  Somethings are fairly fine but its not optmized, its a working notepad as of now.
;;;; * About StarIntel Gserver
;;;; Starintel-gserver is a processing framework for starintel documents.
;;;; it was created after a need that a mess of random scripts, bots and a database wasnt enough.
;;;; I needed something to allow multiple bots to communicate, store, query and handle new targets.
;;;; As of <2024-09-04 Wed> This is the second iteration of the starintel service.
;;;; This version improves on starRouter with actors instead of what i called actors, are really just consumers in
;;;; star-gserver. Gserver started in a org-mode notebook while i scketched things out.
;;;;
;;;;




;;;;** What in the box
;;;; Actors, provided by cl-gserver, not written by me. i named it gserver becuase this was the "gserver" version of starRouter, which no longer used ZMQ.
;;;; Recurring Targets (NO GUARENTEE!)
;;;; Database actor
;;;; Extensibility provided by simple init file and hooks.
;;;; simple http api
;;;; rabbitmq consumers (akin to starRouter actors)
;;;; producers
;;;;
;;;;** Todo
;;;;
;;;; + [ ] Durable target scheduling
;;;;
;;;; + [ ] ZMQ api for bulk use
;;;;
;;;; + [ ] A simpler to use "plugin" system
;;;;
