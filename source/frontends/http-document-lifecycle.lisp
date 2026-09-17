(in-package :star.frontends.http-api)

(defun storage-command-result-or-error (result)
  (case (star.actors:document-storage-result-status result)
    (:success result)
    (:not-found
     (setf (lack.response:response-status *response*) 404)
     nil)
    (otherwise
     (let ((code (star.actors:document-storage-result-error-code result)))
       (setf (lack.response:response-status *response*)
             (if (string= code "storage_backend_error") 502 500))
       nil))))

(defun execute-storage-command (client command)
  "Use the runtime actor when available; keep unit boundaries directly testable."
  (if star.actors:*document-storage-actor*
      (star.actors:call-document-storage command)
      (star.actors::execute-document-storage-command client command)))

(defun current-stored-document (client document-id)
  (star.documents:parse-document-object
   (cl-couch:get-document
    client star:*couchdb-default-database* document-id)))

(defun authorize-stored-document! (action document route method)
  (star.authorization:authorize-document!
   action
   document
   :principal (current-policy-principal)
   :metadata (route-policy-metadata route method)))

(defun storage-hydrate-authorized-document (client raw document-id)
  (star.storage:load-document
   client
   star:*couchdb-default-database*
   document-id
   :get-fn
   (lambda (ignored-client ignored-database ignored-id)
     (declare (ignore ignored-client ignored-database ignored-id))
     raw)))

(defun handle-authorized-document-get-route
    (params &optional (route "/document/:id"))
  "Storage-aware document read: authorize the CouchDB stub before hydration."
  (with-http-boundary ()
    (let ((document-id (require-path-string params "id")))
      (couchdb-handler (client *couchdb-pool*)
        (let* ((raw
                 (cl-couch:get-document
                  client star:*couchdb-default-database* document-id))
               (stored (star.documents:parse-document-object raw)))
          (authorize-stored-document! "documents:read" stored route "GET")
          (strip-server-tenant-fields
           (storage-hydrate-authorized-document client raw document-id)))))))

(defun storage-update-result-response (result)
  (let ((valid (storage-command-result-or-error result)))
    (cond
      ((null valid)
       (status-msg
        (or (star.actors:document-storage-result-error-message result)
            "Document storage operation failed")
        'error
        :code
        (or (star.actors:document-storage-result-error-code result)
            "storage_operation_failed")))
      ((star.actors:document-storage-result-outcome valid)
       (document-update-response
        (star.actors:document-storage-result-outcome valid)))
      (t
       (setf (lack.response:response-status *response*) 500)
       (status-msg "Document storage update returned no outcome"
                   'error
                   :code "storage_update_missing_outcome")))))

(defun handle-authorized-document-update-route
    (params &optional (route "/document/:id"))
  "Storage-aware optimistic update preserving the current data tier."
  (with-http-boundary ()
    (let* ((document-id (require-path-string params "id"))
           (patch (request-json-body)))
      (couchdb-handler (client *couchdb-pool*)
        (star.authorization:authorized-update-document
         document-id
         patch
         (lambda (id)
           (handler-case
               (cl-couch:get-document
                client star:*couchdb-default-database* id)
             (dex:http-request-not-found () nil)))
         (lambda (authorized-patch)
           (storage-update-result-response
            (execute-storage-command
             client
             (star.actors:make-document-storage-command
              :operation :upsert
              :document-id document-id
              :patch authorized-patch))))
         :principal (current-policy-principal)
         :metadata (route-policy-metadata route "PUT"))))))

(defun handle-authorized-document-delete-route
    (params &optional (route "/document/:id"))
  "Delete the CouchDB record and clean up an external object copy after auth."
  (with-http-boundary ()
    (let ((document-id (require-path-string params "id")))
      (couchdb-handler (client *couchdb-pool*)
        (progn
          (star.authorization:authorized-delete-document
           document-id
           (lambda (id)
             (cl-couch:get-document
              client star:*couchdb-default-database* id))
           (lambda (id revision)
             (let ((result
                     (execute-storage-command
                      client
                      (star.actors:make-document-storage-command
                       :operation :delete
                       :document-id id
                       :revision revision))))
               (unless (storage-command-result-or-error result)
                 (error "Document storage delete failed: ~a"
                        (star.actors:document-storage-result-error-message result)))
               t))
           :principal (current-policy-principal)
           :metadata (route-policy-metadata route "DELETE"))
          (status-msg
           (format nil "Document ~a deleted" document-id)
           'success))))))

(defun require-lifecycle-tier (body)
  (let ((tier (jsown:val-safe body "tier")))
    (unless (and (stringp tier) (plusp (length tier)))
      (signal-http-input-error
       400
       "missing_storage_tier"
       "Lifecycle request requires a non-empty tier"))
    (handler-case
        (star.storage:normalize-storage-tier tier)
      (error (condition)
        (signal-http-input-error
         400
         "invalid_storage_tier"
         (princ-to-string condition))))))

(defun handle-document-lifecycle-get-route
    (params &optional (route "/api/v1/documents/:id/lifecycle"))
  "Return placement state after authorizing against the CouchDB metadata stub."
  (with-http-boundary ()
    (let ((document-id (require-path-string params "id")))
      (couchdb-handler (client *couchdb-pool*)
        (let ((stored (current-stored-document client document-id)))
          (authorize-stored-document! "documents:read" stored route "GET")
          (jsown:to-json
           (star.storage:document-storage-lifecycle-json stored)))))))

(defun handle-document-lifecycle-put-route
    (params &optional (route "/api/v1/documents/:id/lifecycle"))
  "Move a tenant document between server-configured storage tiers.

The request chooses only the logical tier. Backend, bucket, endpoint and
credentials remain server-owned configuration."
  (with-http-boundary ()
    (let* ((document-id (require-path-string params "id"))
           (body (require-json-object (parse-json-request)))
           (tier (require-lifecycle-tier body)))
      (couchdb-handler (client *couchdb-pool*)
        (let ((stored (current-stored-document client document-id)))
          (authorize-stored-document! "documents:write" stored route "PUT")
          (let* ((result
                   (execute-storage-command
                    client
                    (star.actors:make-document-storage-command
                     :operation :transition
                     :document-id document-id
                     :tier tier)))
                 (valid (storage-command-result-or-error result)))
            (if valid
                (jsown:to-json
                 (star.actors:document-storage-result-lifecycle valid))
                (status-msg
                 (or (star.actors:document-storage-result-error-message result)
                     "Document storage transition failed")
                 'error
                 :code
                 (or (star.actors:document-storage-result-error-code result)
                     "storage_operation_failed")))))))))
