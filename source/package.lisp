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
   #:*couchdb-views*
   #:+star-server-version+
   #:*actor-system-config*))


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

(uiop:define-package :star.actors
  (:use :cl :star.databases.couchdb :star.consumers :sento.agent :sento.actor :sento.actor-system :sento.actor-context)
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
   #:*publish-service*
   #:*couchdb-bulk-gets*
   #:*couchdb-bulk-inserts*
   #:*actor-index-agent*
   #:start-actor-index
   #:get-dest-actor
   #:route-target
   #:*couchdb-agent*))

(defpackage #:lack/middleware/couchdb-pool
  (:use #:cl)
  (:nicknames #:lack.middleware.couchdb-pool)
  (:import-from #:anypool
                #:make-pool
                #:too-many-open-connection)
  (:export #:*lack-middleware-couchdb-pool*
           #:with-couchdb))

;; [[file:../source.org::*Namespace setup][Namespace setup:4]]
(uiop:define-package   :starintel-gserver-http-api
  (:nicknames :star.frontends.http-api)
  (:import-from :lack/middleware/couchdb-pool :with-couchdb :*connection-pool*)
  (:use       :cl :ningle :star.databases.couchdb :star)
  (:documentation "simple http api.")
  (:export
   #:*default-headers*
   #:start-http-api))
;; Namespace setup:4 ends here
