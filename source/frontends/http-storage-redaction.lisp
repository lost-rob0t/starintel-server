(in-package :star.frontends.http-api)

(defun strip-server-storage-extension! (document)
  "Remove server-owned storage placement metadata from a client document."
  (when (and document (jsown:keyp document "extensions"))
    (let ((extensions (jsown:val document "extensions")))
      (when (and extensions
                 (jsown:keyp extensions
                             star.storage:+document-storage-extension-key+))
        (jsown:remkey extensions
                      star.storage:+document-storage-extension-key+))))
  document)

(defun strip-server-tenant-fields (document)
  "Remove server-only tenancy and storage placement from outgoing documents."
  (etypecase document
    (string
     (jsown:to-json
      (strip-server-tenant-fields (jsown:parse document))))
    (list
     (when (jsown:keyp document "tenant_id")
       (jsown:remkey document "tenant_id"))
     (strip-server-storage-extension! document)
     document)))

(defun storage-safe-document-update-response (outcome)
  "Serialize OUTCOME while redacting server-only fields from its document."
  (if (eq :validation-failed
          (star.databases.couchdb:document-update-outcome-status outcome))
      (document-update-response outcome)
      (let* ((object
               (star.databases.couchdb:document-update-outcome-json outcome))
             (document (jsown:val-safe object "document")))
        (when (and document (listp document))
          (strip-server-tenant-fields document))
        (jsown:to-json object))))

(defun storage-update-result-response (result)
  "Storage-aware update response with the normal validation semantics and redaction."
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
       (storage-safe-document-update-response
        (star.actors:document-storage-result-outcome valid)))
      (t
       (setf (lack.response:response-status *response*) 500)
       (status-msg "Document storage update returned no outcome"
                   'error
                   :code "storage_update_missing_outcome")))))
