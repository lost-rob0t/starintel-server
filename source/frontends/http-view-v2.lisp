(in-package :star.frontends.http-api)

(defparameter +view-v2-max-limit+ 5000)

(defun view-v2-normalize-slug (value)
  (when (stringp value)
    (string-downcase (substitute #\- #\_ value))))

(defun view-v2-resolve-name (raw-name &optional category)
  "Resolve an HTTP slug to a server-owned registered view symbol."
  (let* ((slug (view-v2-normalize-slug raw-name))
         (category-name (and category (view-v2-normalize-slug category)))
         (prefix (and category-name
                      (cdr (assoc category-name
                                  star.databases.couchdb::+v2-view-categories+
                                  :test #'string=))))
         (qualified (cond ((null slug) nil)
                          ((null category) slug)
                          ((null prefix) nil)
                          (t (concatenate 'string prefix slug))))
         (name (and qualified
                    (find qualified
                          (star.databases.couchdb:registered-view-names)
                          :key (lambda (symbol)
                                 (string-downcase (symbol-name symbol)))
                          :test #'string=))))
    (unless name
      (signal-http-input-error
       404 "view_not_found" "Requested registered view does not exist"
       (jsown:new-js ("view" (or raw-name :null))
                     ("category" (or category :null)))))
    name))

(defun view-v2-query-present-p (params name)
  (not (null (or (assoc name params :test #'string=)
                 (assoc (intern (string-upcase name) :keyword)
                        params :test #'eq)))))

(defun view-v2-boolean (params name)
  (let ((raw (query-value params name)))
    (cond
      ((or (string-equal (or raw "") "true")
           (string= (or raw "") "1")) t)
      ((or (string-equal (or raw "") "false")
           (string= (or raw "") "0")) nil)
      (t
       (signal-http-input-error
        400 "invalid_query_parameter"
        (format nil "Query parameter ~a must be true or false" name)
        (jsown:new-js ("parameter" name)
                      ("received" (or raw :null))))))))

(defun view-v2-integer (params name &key default (minimum 0)
                                      (maximum +view-v2-max-limit+))
  (let ((raw (query-value params name)))
    (when (null raw)
      (return-from view-v2-integer default))
    (let ((value
            (handler-case
                (parse-integer raw :junk-allowed nil)
              (error ()
                (signal-http-input-error
                 400 "invalid_query_parameter"
                 (format nil "Query parameter ~a must be an integer" name)
                 (jsown:new-js ("parameter" name)
                               ("received" raw)))))))
      (unless (<= minimum value maximum)
        (signal-http-input-error
         400 "invalid_query_parameter"
         (format nil "Query parameter ~a is outside the allowed range" name)
         (jsown:new-js ("parameter" name)
                       ("minimum" minimum)
                       ("maximum" maximum)
                       ("received" value))))
      value)))

(defun view-v2-json-query-value (params name)
  (let ((raw (query-value params name)))
    (when raw
      (handler-case
          (progn
            (yason:parse raw)
            (jsown:with-injective-reader (jsown:parse raw)))
        (error ()
          (signal-http-input-error
           400 "invalid_query_parameter"
           (format nil "Query parameter ~a must contain valid JSON" name)
           (jsown:new-js ("parameter" name))))))))

(defun view-v2-update-value (params)
  (let ((raw (query-value params "update")))
    (cond
      ((null raw) nil)
      ((string-equal raw "true") t)
      ((string-equal raw "false") nil)
      ((string-equal raw "lazy") :lazy)
      (t
       (signal-http-input-error
        400 "invalid_query_parameter"
        "Query parameter update must be true, false, or lazy"
        (jsown:new-js ("parameter" "update")
                      ("received" raw)))))))

(defun view-v2-query-arguments (params)
  "Translate the v1 HTTP query contract into registry wrapper arguments."
  (let ((arguments
          (list :limit (view-v2-integer params "limit" :default 50
                                                       :minimum 0
                                                       :maximum +view-v2-max-limit+)
                :skip (view-v2-integer params "skip" :default 0
                                                     :minimum 0
                                                     :maximum most-positive-fixnum))))
    (dolist (entry '(("descending" . :descending)
                     ("include_docs" . :include-docs)
                     ("reduce" . :reduce)
                     ("group" . :group)))
      (when (view-v2-query-present-p params (car entry))
        (setf arguments
              (append arguments
                      (list (cdr entry)
                            (view-v2-boolean params (car entry)))))))
    (dolist (entry '(("key" . :key)
                     ("keys" . :keys)
                     ("start_key" . :start-key)
                     ("end_key" . :end-key)))
      (when (view-v2-query-present-p params (car entry))
        (setf arguments
              (append arguments
                      (list (cdr entry)
                            (view-v2-json-query-value params (car entry)))))))
    (when (view-v2-query-present-p params "group_level")
      (setf arguments
            (append arguments
                    (list :group-level
                          (view-v2-integer params "group_level"
                                                   :minimum 0 :maximum 100)))))
    (when (view-v2-query-present-p params "update")
      (setf arguments
            (append arguments
                    (list :update (view-v2-update-value params)))))
    arguments))

(defun view-v2-result-rows (result)
  (etypecase result
    (star.databases.couchdb:view-document-result
     (star.databases.couchdb:view-document-result-rows result))
    (star.databases.couchdb:view-map-result
     (star.databases.couchdb:view-map-result-rows result))
    (star.databases.couchdb:view-reduced-result
     (star.databases.couchdb:view-reduced-result-rows result))))

(defun view-v2-result-kind (result)
  (etypecase result
    (star.databases.couchdb:view-document-result "documents")
    (star.databases.couchdb:view-map-result "map")
    (star.databases.couchdb:view-reduced-result "reduced")))

(defun view-v2-result-document (name result)
  (let* ((spec (star.databases.couchdb:registered-view-spec name))
         (rows (view-v2-result-rows result))
         (view (jsown:new-js
                 ("name" (string-downcase (symbol-name name)))
                 ("design_document"
                  (star.databases.couchdb:view-spec-design-document spec))
                 ("view" (star.databases.couchdb:view-spec-view-name spec))
                 ("reducer"
                  (if (star.databases.couchdb:view-spec-reducer-p spec)
                      :true :false))))
         (document (jsown:new-js
                     ("status" "ok")
                     ("kind" (view-v2-result-kind result))
                     ("view" view)
                     ("row_count" (length rows))
                     ("rows" rows))))
    (when (typep result 'star.databases.couchdb:view-document-result)
      (setf (jsown:val document "documents")
            (star.databases.couchdb:view-document-result-documents result)))
    document))

(defun execute-view-v2-with-client (client name arguments)
  (view-v2-result-document
   name
   (apply #'star.databases.couchdb:execute-registered-view
          name client star:*couchdb-default-database* arguments)))

(defun execute-http-view-v2 (name params)
  (couchdb-handler (client *couchdb-pool*)
    (jsown:to-json
     (execute-view-v2-with-client
      client name (view-v2-query-arguments params)))))

(defun view-v2-category-for-name (name)
  (let ((slug (string-downcase (symbol-name name))))
    (or (loop for (category . prefix)
                in star.databases.couchdb::+v2-view-categories+
              when (uiop:string-prefix-p prefix slug)
                do (return category))
        "legacy")))

(defun view-v2-spec-document (name)
  (let ((spec (star.databases.couchdb:registered-view-spec name)))
    (jsown:new-js
      ("name" (string-downcase (symbol-name name)))
      ("category" (view-v2-category-for-name name))
      ("design_document"
       (star.databases.couchdb:view-spec-design-document spec))
      ("view" (star.databases.couchdb:view-spec-view-name spec))
      ("reducer" (if (star.databases.couchdb:view-spec-reducer-p spec)
                     :true :false))
      ("default_reduce"
       (if (star.databases.couchdb:view-spec-default-reduce spec)
           :true :false))
      ("default_include_docs"
       (if (star.databases.couchdb:view-spec-default-include-docs spec)
           :true :false)))))

(defun view-v2-catalog-document (&optional category)
  (let* ((normalized-category (and category (view-v2-normalize-slug category)))
         (views
           (loop for name in (star.databases.couchdb:registered-view-names)
                 when (or (null normalized-category)
                          (string= normalized-category
                                   (view-v2-category-for-name name)))
                   collect (view-v2-spec-document name))))
    (jsown:new-js ("status" "ok")
                  ("api" "v1")
                  ("view_api" "v2")
                  ("count" (length views))
                  ("views" views))))

(defun view-v2-default-analytics-document (params)
  "Return the default aggregate dashboard without exposing raw documents."
  (let ((limit (view-v2-integer params "limit" :default 50
                                               :minimum 0
                                               :maximum +view-v2-max-limit+))
        (skip (view-v2-integer params "skip" :default 0
                                             :minimum 0
                                             :maximum most-positive-fixnum))
        (descending (if (view-v2-query-present-p params "descending")
                        (view-v2-boolean params "descending")
                        nil)))
    (couchdb-handler (client *couchdb-pool*)
      (flet ((rows (name)
               (view-v2-result-rows
                (star.databases.couchdb:execute-registered-view
                 name client star:*couchdb-default-database*
                 :limit limit :skip skip :descending descending
                 :reduce t :include-docs nil :group t))))
        (jsown:new-js
          ("status" "ok")
          ("api" "v1")
          ("view_api" "v2")
          ("corpus" (rows 'star.databases.couchdb::analytics-count-by-dataset-dtype))
          ("research" (rows 'star.databases.couchdb::research-node-status))
          ("targets" (rows 'star.databases.couchdb::targets-v2-by-actor-status))
          ("geo" (rows 'star.databases.couchdb::geo-by-country))
          ("graph" (rows 'star.databases.couchdb::graph-predicate-counts))
          ("operations" (rows 'star.databases.couchdb::operations-status))
          ("migrations" (rows 'star.databases.couchdb::migrations-version-distribution)))))))

(defun view-v2-route-view-name (params)
  (or (query-value params "view")
      (signal-http-input-error
       400 "missing_path_parameter" "View name is required")))

(defun view-v2-route-category-name (params)
  (or (query-value params "category")
      (signal-http-input-error
       400 "missing_path_parameter" "View category is required")))

(defun install-view-v2-route (path resolver)
  (setf (ningle:route *app* path :method :get)
        (lambda (params)
          (with-http-boundary ()
            (funcall resolver params)))))

(install-view-v2-route
 "/api/v1/views"
 (lambda (params)
   (jsown:to-json
    (view-v2-catalog-document (query-value params "category")))))

(install-view-v2-route
 "/api/v1/views/:view"
 (lambda (params)
   (execute-http-view-v2
    (view-v2-resolve-name (view-v2-route-view-name params)) params)))

(install-view-v2-route
 "/api/v1/views/:category/:view"
 (lambda (params)
   (execute-http-view-v2
    (view-v2-resolve-name (view-v2-route-view-name params)
                          (view-v2-route-category-name params))
    params)))

(install-view-v2-route
 "/api/v1/analytics"
 (lambda (params)
   (jsown:to-json (view-v2-default-analytics-document params))))

(dolist (category '("analytics" "research" "graph" "geo"
                    "operations" "migrations"))
  (let ((path (format nil "/api/v1/~a/:view" category))
        (category-name category))
    (install-view-v2-route
     path
     (lambda (params)
       (execute-http-view-v2
        (view-v2-resolve-name (view-v2-route-view-name params)
                              category-name)
        params)))))

(defparameter *http-view-v2-route-matrix*
  '(("/api/v1/views" catalog)
    ("/api/v1/views/:view" registered-view)
    ("/api/v1/views/:category/:view" categorized-view)
    ("/api/v1/analytics" default-analytics)
    ("/api/v1/analytics/:view" analytics)
    ("/api/v1/research/:view" research)
    ("/api/v1/graph/:view" graph)
    ("/api/v1/geo/:view" geo)
    ("/api/v1/operations/:view" operations)
    ("/api/v1/migrations/:view" migrations)))
