(in-package :star.frontends.http-api)

(defparameter *public-search-datasets*
  (or (star::split-comma-setting
       (uiop:getenv "STAR_PUBLIC_SEARCH_DATASETS"))
      '("*"))
  "Datasets visible through the v1 public-read search surface.
The default wildcard matches the public-server deployment model. Operators
that mix public and private datasets must explicitly configure this list.")

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
       ("doc_spec_version" starintel:+starintel-doc-version+)
       ("default-dataset" star:*couchdb-default-database*)
       ("event_log" star:*couchdb-event-log-database*)
       ("server" "starintel-gserver")
       ("version" star:*star-server-version*)
       ("openapi" "/openapi.json")
       ("client_manifest" "/client-manifest.json")))))

(defun public-search-scopes ()
  (append
   '("search:read" "tenant:*")
   (mapcar (lambda (dataset)
             (format nil "dataset:~a" dataset))
           *public-search-datasets*)))

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
       query
       :principal context
       :requested-dataset nil
       :requested-tenant nil
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
            (cl-couch:fts-search
             client
             (jsown:to-json query)
             star:*couchdb-default-database*
             "search"
             "fts")))))))

(defun reduced-view-value (response)
  (let* ((rows (or (jsown:val-safe response "rows") nil))
         (row (first rows)))
    (or (and row (jsown:val-safe row "value")) 0)))

(defun dtype-count-object (response)
  (let ((result (list :obj)))
    (dolist (row (or (jsown:val-safe response "rows") nil) result)
      (let ((dtype (jsown:val-safe row "key"))
            (count (jsown:val-safe row "value")))
        (when (and (stringp dtype) (numberp count))
          (setf (jsown:val result dtype) count))))))

(defun dtype-count (counts dtype)
  (or (jsown:val-safe counts dtype) 0))

(defun unix-time-now ()
  (- (get-universal-time) 2208988800))

(defun public-stats-document-from-view-responses
    (total-response dtype-response)
  "Build the stable public stats envelope from aggregate CouchDB view output."
  (let* ((by-dtype (dtype-count-object dtype-response))
         (target-total
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
            ("total" (reduced-view-value total-response))
            ("by_dtype" by-dtype)))
         ("targets"
          (jsown:new-js
            ("total" target-total))))))))

(defun handle-public-stats-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (couchdb-handler (client *couchdb-pool*)
      (let ((total-response
              (query-view
               client
               star:*couchdb-default-database*
               "data"
               "total"
               :limit 1
               :include-docs nil
               :reduce t))
            (dtype-response
              (query-view
               client
               star:*couchdb-default-database*
               "data"
               "count_by_dtype"
               :limit 1000
               :include-docs nil
               :reduce t
               :group t)))
        (set-cache-control
         (format nil "public, max-age=~d" +public-stats-cache-seconds+))
        (jsown:to-json
         (public-stats-document-from-view-responses
          total-response dtype-response))))))

(defun handle-openapi-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (star.http.contract:openapi-json)))

(defun handle-client-manifest-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (star.http.contract:client-manifest-json)))

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
