(in-package :star.frontends.http-api)

(defun file-error-response (condition)
  "Render a typed file-content error at the HTTP boundary."
  (setf (lack.response:response-status *response*)
        (star.databases.couchdb::file-content-error-status condition))
  (status-msg "File content rejected" 'error
              :code (star.databases.couchdb::file-content-error-code condition)))

(defun handle-file-create-route (params)
  "Authorize and atomically commit a bounded core file envelope."
  (declare (ignore params))
  (with-http-boundary ()
    (handler-case
        (let* ((limit (+ (* 4 (ceiling star.databases.couchdb::*file-max-bytes* 3))
                         +http-max-body-bytes+))
               (envelope (require-json-object (parse-json-request :max-bytes limit)))
               (document (star.databases.couchdb::prepare-file-ingest envelope)))
          (star.authorization:authorized-publish-document
           (star.documents:canonical-wire-document document)
           (lambda (ignored)
             (declare (ignore ignored))
             (stamp-server-tenant! document)
             (couchdb-handler (client *couchdb-pool*)
               (jsown:to-json
                (strip-server-tenant-fields
                 (star.databases.couchdb::persist-file-ingest
                  client star:*couchdb-default-database* document
                  #'star.rabbit:publish-outbox-event)))))
           :principal (current-publish-service-context)
           :metadata (route-policy-metadata "/api/v1/files" "POST")))
      (star.databases.couchdb::file-content-error (condition)
        (file-error-response condition))
      (star.documents:document-schema-validation-error ()
        (signal-http-input-error 422 "invalid_document_schema" "Invalid core file document")))))

(defun handle-file-content-route (params)
  "Authorize the stored file and return its verified content bytes."
  (with-http-boundary ()
    (couchdb-handler (client *couchdb-pool*)
      (handler-case
          (let* ((id (require-path-string params "id"))
                 (document
                   (star.authorization:authorized-fetch-document
                    id
                    (lambda (document-id)
                      (star.documents:parse-document-object
                       (cl-couch:get-document client star:*couchdb-default-database* document-id)))
                    :principal (current-policy-principal)
                    :metadata (route-policy-metadata "/api/v1/files/:id/content" "GET")))
                 (bytes (star.databases.couchdb::couchdb-file-content
                         client star:*couchdb-default-database* document)))
            ;; Imported media is untrusted; serve as a download, never active HTML.
            (setf (lack.response:response-headers *response*)
                  (append (lack.response:response-headers *response*)
                          (list :content-type "application/octet-stream"
                                :content-length (length bytes)
                                :content-disposition "attachment"
                                :x-content-type-options "nosniff"
                                :cache-control "no-store"
                                :etag (format nil "\"~a\"" (jsown:val document "bytesHash")))))
            bytes)
        (star.databases.couchdb::file-content-error (condition)
          (file-error-response condition))))))

(mount-http-operation "files.create" #'handle-file-create-route)
(mount-http-operation "files.content.get" #'handle-file-content-route)
