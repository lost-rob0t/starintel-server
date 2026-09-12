(in-package :star.frontends.http-api)

(defparameter +scoped-view-max-results+ 500)
(defparameter +scoped-view-max-key-length+ 2048)

(defstruct scoped-view-contract
  design
  view
  mode
  max-group-level)

(defparameter *scoped-view-contracts*
  (list
   (make-scoped-view-contract
    :design "scoped" :view "documents" :mode :documents)
   (make-scoped-view-contract
    :design "scoped" :view "docs_added_by_day"
    :mode :aggregate :max-group-level 3)
   (make-scoped-view-contract
    :design "scoped" :view "count_by_dtype"
    :mode :aggregate :max-group-level 3))
  "Server-approved projections exposed by /api/v1/views/:design/:view.

Every registered projection MUST derive its first two key components from
[tenant-id, dataset-id]. Aggregate projections must perform reduction only
after that prefix has been applied by this route.")

(defun scoped-view-contract-for (design view-name)
  (find-if
   (lambda (contract)
     (and (string= design (scoped-view-contract-design contract))
          (string= view-name (scoped-view-contract-view contract))))
   *scoped-view-contracts*))

(defun scoped-view-query-boolean (params name default)
  (let ((value (query-value params name)))
    (cond
      ((null value) default)
      ((string-equal value "true") t)
      ((string-equal value "false") nil)
      (t
       (signal-http-input-error
        400
        "invalid_view_boolean"
        (format nil "Query parameter ~a must be true or false" name))))))

(defun scoped-view-json-object-p (value)
  (and (consp value) (eq (first value) :obj)))

(defun scoped-view-key-value-p (value)
  (cond
    ((scoped-view-json-object-p value) nil)
    ((consp value) (every #'scoped-view-key-value-p value))
    (t t)))

(defun scoped-view-key-suffix (params name)
  (let ((raw (query-value params name)))
    (when raw
      (when (> (length raw) +scoped-view-max-key-length+)
        (signal-http-input-error
         400
         "view_key_too_long"
         (format nil "Query parameter ~a exceeds the maximum encoded length" name)))
      (handler-case
          (let ((value (jsown:parse raw)))
            (unless (scoped-view-key-value-p value)
              (signal-http-input-error
               400
               "invalid_view_key"
               (format nil "Query parameter ~a may contain only JSON scalars or arrays" name)))
            (if (and (listp value) (not (scoped-view-json-object-p value)))
                value
                (list value)))
        (error ()
          (signal-http-input-error
           400
           "invalid_view_key"
           (format nil "Query parameter ~a must be valid JSON" name)))))))

(defun scoped-view-prefix-key (tenant dataset suffix)
  (append (list tenant dataset) suffix))

(defun scoped-view-high-key (tenant dataset)
  ;; CouchDB JSON collation places objects after scalar/array values.  The
  ;; empty object is server-generated only; callers cannot provide objects in
  ;; key suffixes, so the tenant/dataset prefix cannot be escaped.
  (list tenant dataset (list :obj)))

(defun validate-scoped-view-mode-options
    (params contract reduce include-docs group group-level)
  (ecase (scoped-view-contract-mode contract)
    (:documents
     (when reduce
       (signal-http-input-error
        400 "view_reduce_not_supported"
        "This scoped document view does not support reduce=true"))
     (unless include-docs
       (signal-http-input-error
        400 "view_include_docs_required"
        "Scoped document views require include_docs=true for defense-in-depth row authorization"))
     (when (or group group-level)
       (signal-http-input-error
        400 "view_group_not_supported"
        "Grouping is available only for approved aggregate views")))
    (:aggregate
     (unless reduce
       (signal-http-input-error
        400 "view_reduce_required"
        "Approved aggregate views require reduce=true"))
     (when include-docs
       (signal-http-input-error
        400 "view_include_docs_not_supported"
        "Reduced aggregate views cannot include source documents"))
     (when (and group-level
                (> group-level (scoped-view-contract-max-group-level contract)))
       (signal-http-input-error
        400 "view_group_level_too_deep"
        "group_level exceeds the registered projection key depth"))))
  params)

(defun build-scoped-view-query (params contract tenant dataset)
  "Return validated QUERY-VIEW keyword arguments for CONTRACT.

Caller-controlled keys are suffixes only.  The tenant/dataset prefix is always
injected by the server before CouchDB sees the query."
  (let* ((mode (scoped-view-contract-mode contract))
         (limit (bounded-query-integer
                 params "limit" :default 100 :minimum 1
                 :maximum +scoped-view-max-results+))
         (descending (scoped-view-query-boolean params "descending" nil))
         (inclusive-end (scoped-view-query-boolean params "inclusive_end" t))
         (reduce (scoped-view-query-boolean
                  params "reduce" (eq mode :aggregate)))
         (include-docs (scoped-view-query-boolean
                        params "include_docs" (eq mode :documents)))
         (group (scoped-view-query-boolean params "group" nil))
         (group-level
           (when (query-value params "group_level")
             (bounded-query-integer
              params "group_level" :minimum 2
              :maximum (or (scoped-view-contract-max-group-level contract) 2))))
         (start-suffix (scoped-view-key-suffix params "startkey"))
         (end-suffix (scoped-view-key-suffix params "endkey"))
         (low (list tenant dataset))
         (high (scoped-view-high-key tenant dataset))
         (start-key
           (if start-suffix
               (scoped-view-prefix-key tenant dataset start-suffix)
               (if descending high low)))
         (end-key
           (if end-suffix
               (scoped-view-prefix-key tenant dataset end-suffix)
               (if descending low high))))
    (validate-scoped-view-mode-options
     params contract reduce include-docs group group-level)
    (list :limit limit
          :descending descending
          :include-docs include-docs
          :reduce reduce
          :start-key start-key
          :end-key end-key
          :inclusive-end inclusive-end
          :group group
          :group-level group-level)))

(defun scoped-view-response-document
    (design view-name tenant dataset contract response)
  (jsown:new-js
    ("status" "ok")
    ("data"
     (jsown:new-js
       ("design" design)
       ("view" view-name)
       ("tenant" tenant)
       ("dataset" dataset)
       ("mode" (string-downcase
                 (symbol-name (scoped-view-contract-mode contract))))
       ("result" response)))))

(defun handle-bounded-scoped-view-route (params)
  (with-http-boundary ()
    (let* ((design (require-path-string params "design"))
           (view-name (require-path-string params "view"))
           (dataset (require-query-string params "dataset"))
           (tenant (or (query-value params "tenant") "default"))
           (contract (and (safe-view-name-p design)
                          (safe-view-name-p view-name)
                          (scoped-view-contract-for design view-name)))
           (metadata (route-policy-metadata
                      "/api/v1/views/:design/:view" "GET")))
      (unless contract
        (signal-http-input-error
         404
         "view_not_exposed"
         "The requested view is not exposed through the bounded scoped API"))
      (star.authorization:authorize!
       "views:read"
       :principal (current-policy-principal)
       :resource
       (star.authorization:make-authorization-resource
        :tenant-id tenant
        :dataset-id dataset)
       :metadata metadata)
      (let ((arguments (build-scoped-view-query
                        params contract tenant dataset)))
        (couchdb-handler (client *couchdb-pool*)
          (let* ((raw
                   (apply #'query-view
                          client
                          star:*couchdb-default-database*
                          design
                          view-name
                          arguments))
                 (authorized
                   (if (eq (scoped-view-contract-mode contract) :documents)
                       (star.authorization:authorized-view-response
                        raw
                        :principal (current-policy-principal)
                        :requested-dataset dataset
                        :requested-tenant tenant
                        :metadata metadata)
                       raw)))
            (set-cache-control "private, no-store")
            (jsown:to-json
             (scoped-view-response-document
              design view-name tenant dataset contract authorized))))))))

(mount-http-operation "views.scoped.get" #'handle-bounded-scoped-view-route)
