(in-package :star.frontends.http-api)

(defun request-json-body ()
  (require-json-object (parse-json-request)))

(defun document-update-id (params)
  (let ((document-id (query-value params "id")))
    (unless (non-empty-string-p document-id)
      (signal-http-input-error
       400
       "missing_path_parameter"
       "Route parameter id is required"))
    document-id))

(defun document-update-response (outcome)
  (if (eq :validation-failed
          (star.databases.couchdb:document-update-outcome-status outcome))
      (progn
        (setf (lack.response:response-status *response*) 422)
        (status-msg
         "Invalid document update request"
         'error
         :code
         (or (star.databases.couchdb:document-update-outcome-code outcome)
             "invalid_document_update")
         :info
         (jsown:new-js
           ("reason"
            (star.databases.couchdb:document-update-outcome-reason outcome)))))
      (jsown:to-json
       (star.databases.couchdb:document-update-outcome-json outcome))))

(defun handle-document-update-route (params)
  (with-http-boundary ()
    (let ((document-id (document-update-id params))
          (patch (request-json-body)))
      (couchdb-handler (client *couchdb-pool*)
        (document-update-response
         (star.databases.couchdb:couchdb-upsert-document-update
          client
          star:*couchdb-default-database*
          document-id
          patch))))))

(setf (ningle:route *app* "/document/:id" :method :put)
      #'handle-document-update-route)
