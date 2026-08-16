(in-package :star.authorization)

(defun request-metadata (&key route method correlation-id)
  (list :route route
        :method method
        :correlation-id
        (or correlation-id
            (and star.auth:*request-security-context*
                 (star.auth:request-security-context-correlation-id
                  star.auth:*request-security-context*)))))

(defun principal-has-capability-p (action &optional principal)
  (let* ((candidate (candidate-principal principal))
         (scopes (principal-scopes candidate)))
    (capability-granted-p scopes action)))

(defun principal-has-wildcard-dataset-p (&optional principal)
  (let* ((candidate (candidate-principal principal))
         (scopes (principal-scopes candidate)))
    (or (administrator-scopes-p scopes)
        (member "*" (scope-values scopes "dataset:") :test #'string=))))

(defun authorize-document! (action document &key principal actor-name metadata)
  (authorize!
   action
   :principal principal
   :resource (resource-from-document document :actor-name actor-name)
   :metadata metadata))

(defun parse-document-value (value)
  (if (stringp value)
      (jsown:parse value)
      value))

(defun authorized-fetch-document (document-id fetch-fn
                                  &key principal metadata)
  "Fetch and authorize one document. FETCH-FN receives DOCUMENT-ID."
  (let* ((raw (funcall fetch-fn document-id))
         (document (parse-document-value raw)))
    (authorize-document!
     "documents:read"
     document
     :principal principal
     :metadata metadata)
    raw))

(defun authorized-delete-document (document-id fetch-fn delete-fn
                                   &key principal metadata)
  "Fetch, authorize, and delete one document through explicit injected I/O."
  (let* ((raw (funcall fetch-fn document-id))
         (document (parse-document-value raw))
         (revision (jsown:val document "_rev")))
    (authorize-document!
     "documents:delete"
     document
     :principal principal
     :metadata metadata)
    (funcall delete-fn document-id revision)))

(defun document-action (document &optional requested-action)
  (or requested-action
      (let ((dtype (jsown:val-safe document "dtype")))
        (if (and (stringp dtype)
                 (string-equal dtype "target"))
            "targets:dispatch"
            "documents:write"))))

(defun authorized-publish-document (document publish-fn
                                    &key principal actor-name action metadata)
  "Authorize at the publish boundary, bind the server decision, then publish."
  (let* ((resolved-action (document-action document action))
         (decision
           (authorize-document!
            resolved-action
            document
            :principal principal
            :actor-name actor-name
            :metadata metadata)))
    (let ((*current-authorization-decision* decision))
      (funcall publish-fn document))))

(defun authorize-bulk-documents! (documents &key principal metadata)
  "Reject the complete batch before side effects if any document is unauthorized."
  (authorize!
   "documents:bulk"
   :principal principal
   :metadata metadata)
  (loop for document in documents
        collect
        (authorize-document!
         (document-action document)
         document
         :principal principal
         :metadata metadata)))

(defun authorized-target-documents (documents actor-name action
                                    &key principal metadata)
  "Return only target documents individually authorized for the caller."
  (loop for raw in documents
        for document = (parse-document-value raw)
        for decision =
          (authorize
           action
           :principal principal
           :resource (resource-from-document
                      document
                      :actor-name actor-name)
           :metadata metadata)
        when (authorization-decision-allowed-p decision)
          collect raw))

(defun lucene-escape (value)
  (with-output-to-string (stream)
    (loop for character across value
          do (when (find character "+&|!(){}[]^\"~*?:\\/")
               (write-char #\\ stream))
             (write-char character stream))))

(defun lucene-term (field value)
  (format nil "~a:\"~a\"" field (lucene-escape value)))

(defun lucene-scope-clause (field values)
  (when values
    (format nil "(~{~a~^ OR ~})"
            (mapcar (lambda (value)
                      (lucene-term field value))
                    values))))

(defun restricted-values (scopes prefix requested-value)
  (let ((values (scope-values scopes prefix)))
    (cond
      ((or (administrator-scopes-p scopes)
           (member "*" values :test #'string=))
       (and requested-value (list requested-value)))
      (requested-value
       (if (member requested-value values :test #'string=)
           (list requested-value)
           nil))
      (t
       (remove "*" values :test #'string=)))))

(defun require-search-dimension (values reason action principal metadata)
  (unless values
    (let ((decision
            (make-authorization-decision
             :id (cms-ulid:ulid)
             :allowed-p nil
             :reason reason
             :action action
             :resource nil
             :principal-id (principal-id (candidate-principal principal)))))
      (let ((request
              (make-authorization-request
               :principal principal
               :action action
               :resource nil
               :metadata metadata)))
        (emit-authorization-audit decision request))
      (error 'authorization-error
             :code "access_denied"
             :decision decision))))

(defun authorized-search-query (query
                                &key principal requested-dataset
                                  (requested-tenant "default") metadata)
  "Return a backend-scoped Clouseau query, never an unscoped post-filter query."
  (let* ((candidate (candidate-principal principal))
         (scopes (principal-scopes candidate)))
    (authorize!
     "search:read"
     :principal candidate
     :metadata metadata)
    (let* ((wild-dataset
             (or (administrator-scopes-p scopes)
                 (member "*" (scope-values scopes "dataset:")
                         :test #'string=)))
           (wild-tenant
             (or (administrator-scopes-p scopes)
                 (member "*" (scope-values scopes "tenant:")
                         :test #'string=)))
           (datasets
             (restricted-values scopes "dataset:" requested-dataset))
           (tenants
             (restricted-values scopes "tenant:" requested-tenant)))
      (unless wild-dataset
        (require-search-dimension
         datasets "dataset_scope_required" "search:read" candidate metadata))
      (unless wild-tenant
        (require-search-dimension
         tenants "tenant_scope_required" "search:read" candidate metadata))
      (let ((clauses
              (remove nil
                      (list
                       (format nil "(~a)" query)
                       (and (not wild-dataset)
                            (lucene-scope-clause "dataset" datasets))
                       (and (not wild-tenant)
                            (lucene-scope-clause "tenant_id" tenants))))))
        (format nil "~{~a~^ AND ~}" clauses)))))

(defun row-document (row)
  (or (jsown:val-safe row "doc")
      (let ((value (jsown:val-safe row "value")))
        (and (consp value)
             (eq (first value) :obj)
             value))))

(defun authorized-view-row-p (row principal metadata)
  (let ((document (row-document row)))
    (and document
         (authorization-decision-allowed-p
          (authorize
           "views:read"
           :principal principal
           :resource (resource-from-document document)
           :metadata metadata)))))

(defun authorized-view-response (response
                                 &key principal requested-dataset
                                   (requested-tenant "default") metadata)
  "Filter a CouchDB view response and replace counts so unauthorized rows leak no data."
  (let ((resource
          (make-authorization-resource
           :tenant-id requested-tenant
           :dataset-id requested-dataset)))
    (authorize!
     "views:read"
     :principal principal
     :resource resource
     :metadata metadata))
  (let* ((parsed (parse-document-value response))
         (rows (or (jsown:val-safe parsed "rows") nil))
         (authorized-rows
           (loop for row in rows
                 when (authorized-view-row-p row principal metadata)
                   collect row)))
    (jsown:new-js
      ("total_rows" (length authorized-rows))
      ("offset" 0)
      ("rows" authorized-rows))))
