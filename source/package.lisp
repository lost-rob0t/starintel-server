;; [[file:../source.org::*Namespace setup][Namespace setup:2]]
(uiop:define-package   :starintel-gserver
  (:nicknames :star)
  (:use       :cl)
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
   #:*couchdb-default-database*
   #:*couchdb-host*
   #:*couchdb-port*
   #:*couchdb-user*
   #:*couchdb-password*
   #:*couchdb-scheme*
   #:main
   #:reload
   #:start-debugger
   #:*slynk-port*
   #:*actors-start-hook*
   #:*document-patterns*
   #:*injest-workers*
   #:*couchdb-event-log-database*
   #:*couchdb-views*))


(uiop:define-package   :star.databases.couchdb
  (:use       :cl-couch :cl :star #:lparallel)
  (:export :init-db
   :init-views
           :get-targets*
   :get-view-docs
           :query-view
   :map-view-results
           :get-neighbors
   :search-fts
           :sort-docs-by-date
   :messages-by-user
           :messages-by-platform
   :messages-by-group
           :social-posts-by-user
   :social-posts-by-group
           :by-channel
   :export-by-dataset*
           :count-by-dtype
   :dataset-size
           :total-documents-since
   :orgs-by-country
           :orgs-by-name
   :persons-by-name
           :persons-by-region
   :relations-edges
           :relations-incoming-count
   :relations-outgoing-count
           :targets-actor-counts
   :targets-by-actor
           :targets-target-count
   :users-by-platform
           :as-json
   :format-key
           :from-json
   :*couchdb-pool*
           :groups
   :lazy
           :t
   :nil)
  (:documentation "doc"))
;; Namespace setup:2 ends here

;; [[file:../source.org::*Namespace setup][Namespace setup:3]]
(uiop:define-package   :star.rabbit
  (:use       :cl :star.consumers  :sento.actor)
  (:documentation "Rabitmq namespace")
  (:export
   #:with-rabbit-send
   #:with-rabbit-recv
   #:emit-document
   #:+injest-queue+
   #:+updates-queue+
   #:+injest-key+
   #:+update-key+
   #:+targets-key+
   #:transient-p
   #:test-make-doc
   #:test-send
   #:start-consumers))
;; Namespace setup:3 ends here

(uiop:define-package   :star.actors
  (:use       :cl :star.databases.couchdb :sento.agent :sento.actor :sento.actor-system :sento.actor-context)
  (:documentation "doc")
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
(uiop:define-package   :starintel-gserver-http-api
  (:nicknames :star.frontends.http-api)
  (:use       :cl :ningle :anypool :star.databases.couchdb :star)
  (:documentation "simple http api.")
  (:export
   #:*default-headers*))
;; Namespace setup:4 ends here
