(in-package :star.frontends.http-api)

(defun request-json-body ()
  (jsown:with-injective-reader
    (jsown:parse
     (babel:octets-to-string
      (lack.request:request-content (ningle:context :request))
      :encoding :utf-8))))

(setf (ningle:route *app* "/document/:id" :method :put)
      (lambda (params)
        (set-default-headers)
        (let ((document-id (cdr (assoc :id params :test #'string=))))
          (handler-case
              (let ((patch (request-json-body)))
                (couchdb-handler (client *couchdb-pool*)
                  (jsown:to-json
                   (star.databases.couchdb:document-update-outcome-json
                    (star.databases.couchdb:couchdb-upsert-document-update
                     client
                     star:*couchdb-default-database*
                     document-id
                     patch)))))
            (error (condition)
              (status-msg
               "Invalid document update request"
               'error
               :info (princ-to-string condition)))))))
