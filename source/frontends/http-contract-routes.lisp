(in-package :star.frontends.http-api)

(defparameter *public-search-datasets*
  (star::split-comma-setting (uiop:getenv "STAR_PUBLIC_SEARCH_DATASETS"))
  "Explicitly approved datasets for anonymous reads; empty disables them.")

(defparameter +public-tenant+ "public")

(defun require-public-datasets ()
  (unless (and *public-search-datasets*
               (every (lambda (dataset)
                        (and (stringp dataset)
                             (plusp (length dataset))
                             (not (string= dataset "*"))))
                      *public-search-datasets*))
    (signal-http-input-error
     503 "public_scope_unconfigured"
     "Public dataset allowlist is not configured"))
  *public-search-datasets*)

(defparameter +public-search-path+ "/api/v1/search")
(defparameter +public-stats-path+ "/api/v1/stats")
(defparameter +public-search-max-query-length+ 512)
(defparameter +public-search-max-results+ 50)
(defparameter +public-stats-cache-seconds+ 15)

;; Schema/discovery artifacts remain safe to fetch anonymously regardless of
;; deployment mode. Data-bearing read endpoints are gated dynamically by
;; STAR::*PUBLIC-MODE* in the authentication middleware, so init.lisp can
;; switch them between anonymous and authenticated operation after system load.
(dolist (path '("/openapi.json"
                "/client-manifest.json"))
  (pushnew path star:*auth-public-paths* :test #'string=))

(defun mount-http-operation (operation-id handler)
  "Mount HANDLER using the canonical method/path for OPERATION-ID."
  (let ((operation (star.http.contract:find-http-operation operation-id)))
    (setf (ningle:route *app*
                        (star.http.contract:http-operation-path operation)
                        :method (star.http.contract:http-operation-method operation))
          handler)
    operation))

(defun set-cache-control (value)
  (setf (lack.response:response-headers *response*)
        (append (lack.response:response-headers *response*)
                (list :cache-control value))))

(defun handle-contracted-health-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (status-msg "OK" 'info)))

(defun handle-contracted-server-info-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (jsown:to-json
     (jsown:new-js
       ("doc_spec_version" (starintel.canonical:release-version))
       ("document_schema_version" (starintel.canonical:schema-version))
       ("spec_authority" "lost-rob0t/star-lang")
       ("default-dataset" star:*couchdb-default-database*)
       ("event_log" star:*couchdb-event-log-database*)
       ("server" "starintel-gserver")
       ("version" star:*star-server-version*)
       ("openapi" "/openapi.json")
       ("client_manifest" "/client-manifest.json")))))

(defun public-search-scopes ()
  (append
   '("search:read" "tenant:public")
   (mapcar (lambda (dataset)
             (format nil "dataset:~a" dataset))
           (require-public-datasets))))

(defun public-search-context ()
  (star.authorization:make-trusted-authorization-context
   :id "public-api-v1"
   :principal-type "public_reader"
   :scopes (public-search-scopes)))

(defun public-search-authorized-query (query)
  "Compile QUERY through the normal authorization scoper using only
server-owned public scopes. No caller principal or caller scope enters here."
  (let ((context (public-search-context)))
    (star.authorization:with-trusted-authorization-context (context)
      (star.authorization:authorized-search-query
       (star.authorization::lucene-escape query)
       :principal context
       :requested-dataset nil
       :requested-tenant +public-tenant+
       :metadata (route-policy-metadata +public-search-path+ "GET")))))

(defun reject-public-scope-overrides (params)
  (when (or (query-value params "dataset")
            (query-value params "tenant"))
    (signal-http-input-error
     400
     "public_scope_is_server_owned"
     "Public search dataset and tenant scope cannot be overridden")))

(defun handle-public-search-route (params)
  (with-http-boundary ()
    (reject-public-scope-overrides params)
    (let* ((q (require-query-string params "q"))
           (limit (bounded-query-integer
                   params "limit"
                   :default 25
                   :minimum 1
                   :maximum +public-search-max-results+)))
      (when (> (length q) +public-search-max-query-length+)
        (signal-http-input-error
         400
         "search_query_too_long"
         "Public search query exceeds the configured length limit"
         (jsown:new-js
           ("maximum" +public-search-max-query-length+))))
      (let ((scoped-query (public-search-authorized-query q)))
        (couchdb-handler (client *couchdb-pool*)
          (let* ((bookmark (query-value params "bookmark"))
                 (query (jsown:new-js
                          ("q" scoped-query)
                          ("limit" limit)
                          ("include_docs" t))))
            (when bookmark
              (setf (jsown:val query "bookmark") bookmark))
            (set-cache-control "no-store")
            (strip-server-tenant-from-search-body
             (cl-couch:fts-search
              client
              (jsown:to-json query)
              star:*couchdb-default-database*
              "search"
              "fts"))))))))

(defun public-dtype-counts (responses)
  (let ((counts (list :obj))
        (total 0))
    (loop for response in responses
          for dataset in *public-search-datasets* do
      (when (>= (length (or (jsown:val-safe response "rows") nil)) 1000)
        (error "Public statistics view result may be truncated"))
      (dolist (row (or (jsown:val-safe response "rows") nil))
        (let* ((key (jsown:val-safe row "key"))
               (dtype (and (listp key) (third key)))
               (count (jsown:val-safe row "value")))
          (unless (and (listp key)
                       (= (length key) 3)
                       (string= (first key) +public-tenant+)
                       (string= (second key) dataset)
                       (stringp dtype) (integerp count) (>= count 0))
            (error "Invalid public statistics view row"))
          (incf total count)
          (setf (jsown:val counts dtype)
                (+ count (or (jsown:val-safe counts dtype) 0))))))
    (values counts total)))

(defun dtype-count (counts dtype)
  (or (jsown:val-safe counts dtype) 0))

(defun unix-time-now ()
  (- (get-universal-time) 2208988800))

(defun public-stats-document-from-view-responses (responses)
  "Build the stable public stats envelope from aggregate CouchDB view output."
  (multiple-value-bind (by-dtype total) (public-dtype-counts responses)
    (let ((target-total
            (+ (dtype-count by-dtype "target")
               (dtype-count by-dtype "investigation-target"))))
      (jsown:new-js
        ("status" "ok")
        ("data"
         (jsown:new-js
           ("service" "starintel-gserver")
           ("version" star:*star-server-version*)
           ("generated_at" (unix-time-now))
           ("documents"
            (jsown:new-js
              ("total" total)
              ("by_dtype" by-dtype)))
           ("targets"
            (jsown:new-js
              ("total" target-total)))))))))

(defun handle-public-stats-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (require-public-datasets)
    (couchdb-handler (client *couchdb-pool*)
      (let ((responses
              (mapcar
               (lambda (dataset)
                 (query-view
                  client star:*couchdb-default-database*
                  "data" "count_by_tenant_dataset_dtype"
                  :start-key (list +public-tenant+ dataset)
                  :end-key (list +public-tenant+ dataset (jsown:new-js))
                  :limit 1000 :include-docs nil :reduce t :group t))
               *public-search-datasets*)))
        (set-cache-control
         (format nil "public, max-age=~d" +public-stats-cache-seconds+))
        (jsown:to-json
         (public-stats-document-from-view-responses
          responses))))))

(defun handle-openapi-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (star.http.contract:openapi-json)))

(defun handle-client-manifest-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (star.http.contract:client-manifest-json)))

;; Versioned document operations. Each handler reuses the shared authorized
;; document pipelines of the hardened boundary and only labels the
;; authorization audit with the versioned route shape.
(defun handle-contracted-document-create-route (params)
  (authorized-new-document params "/api/v1/documents"))

(defun handle-contracted-document-bulk-route (params)
  (handle-authorized-bulk-route params "/api/v1/documents/bulk"))

(defun handle-contracted-document-get-route (params)
  (handle-authorized-document-get-route params "/api/v1/documents/:id"))

(defun handle-contracted-document-update-route (params)
  (handle-authorized-document-update-route params "/api/v1/documents/:id"))

(defun handle-contracted-document-delete-route (params)
  (handle-authorized-document-delete-route params "/api/v1/documents/:id"))

(defun handle-contracted-document-search-route (params)
  (handle-authorized-search-route params "/api/v1/documents/search"))

;; Re-mount the contracted surface from operation IDs. Some legacy route
;; declarations still exist in their historical source files; these final
;; mounts are authoritative and prevent method/path drift for the contracted
;; client surface while the remaining legacy API is migrated incrementally.
(mount-http-operation "health.get" #'handle-contracted-health-route)
(mount-http-operation "server.get" #'handle-contracted-server-info-route)
(mount-http-operation "schema.openapi.get" #'handle-openapi-route)
(mount-http-operation "schema.client-manifest.get" #'handle-client-manifest-route)
(mount-http-operation "public.search.get" #'handle-public-search-route)
(mount-http-operation "stats.get" #'handle-public-stats-route)

(mount-http-operation "auth.login" #'handle-auth-login-route)
(mount-http-operation "auth.bootstrap" #'handle-auth-bootstrap-route)
(mount-http-operation "auth.context.get" #'handle-auth-context-route)
(mount-http-operation "auth.users.create" #'handle-auth-create-user-route)
(mount-http-operation "auth.users.list" #'handle-auth-list-users-route)
(mount-http-operation "auth.users.password.reset"
                      #'handle-auth-reset-user-password-route)
(mount-http-operation "auth.password.change" #'handle-auth-change-password-route)
(mount-http-operation "auth.credentials.create" #'handle-auth-create-route)
(mount-http-operation "auth.credentials.list" #'handle-auth-list-route)
(mount-http-operation "auth.credentials.rotate" #'handle-auth-rotate-route)
(mount-http-operation "auth.credentials.revoke" #'handle-auth-revoke-route)
(mount-http-operation "auth.credentials.disable" #'handle-auth-disable-route)

;; Versioned document operations. documents.search must be mounted before
;; documents.get: myway dispatches in registration order, so the static
;; route would otherwise be captured by the /:id route.
(mount-http-operation "documents.search"
                      #'handle-contracted-document-search-route)
(mount-http-operation "documents.create"
                      #'handle-contracted-document-create-route)
(mount-http-operation "documents.bulk.create"
                      #'handle-contracted-document-bulk-route)
(mount-http-operation "documents.get"
                      #'handle-contracted-document-get-route)
(mount-http-operation "documents.update"
                      #'handle-contracted-document-update-route)
(mount-http-operation "documents.delete"
                      #'handle-contracted-document-delete-route)
