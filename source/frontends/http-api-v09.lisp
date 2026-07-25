(in-package :star.frontends.http-api)

(defun request-body-string ()
  (babel:octets-to-string
   (lack.request:request-content (ningle:context :request))
   :encoding :utf-8))

(defun bad-document-response (error)
  (set-default-headers)
  (setf (lack.response:response-status *response*) 400)
  (status-msg "Invalid StarIntel v0.9 document"
              'error
              :info (format nil "~a" error)))

(defun publish-v09-document (document)
  (let* ((dtype (star.documents.v09:document-dtype document))
         (routing-key (format nil "documents.new.~a" dtype))
         (body (jsown:to-json document)))
    (with-rabbitmq (*rabbitmq-conn*)
      (cl-rabbit:basic-publish *rabbitmq-conn* 1
                               :routing-key routing-key
                               :exchange "documents"
                               :properties (list (cons :type dtype)
                                                 (cons :content-type "application/json"))
                               :body body))
    body))

(setf (ningle:route *app* "/new/target/:actor" :method :post)
      #'(lambda (params)
          (handler-case
              (let* ((actor (cdr (assoc :actor params :test #'string=)))
                     (document (star.documents.v09:ensure-v09-document
                                (request-body-string)))
                     (dtype (star.documents.v09:document-dtype document))
                     (data (star.documents.v09:document-data document))
                     (document-actor (star.documents.v09:document-value document "actor")))
                (unless (member dtype '("target" "investigation-target") :test #'string=)
                  (error 'star.documents.v09:v09-document-error
                         :message (format nil "target endpoint does not accept dtype ~a" dtype)))
                (when (and document-actor
                           (not (string= (string document-actor) actor)))
                  (error 'star.documents.v09:v09-document-error
                         :message (format nil "route actor ~a does not match document actor ~a"
                                          actor document-actor)))
                (unless document-actor
                  (setf (jsown:val data "actor") actor))
                (set-default-headers)
                (publish-v09-document document))
            (star.documents.v09:v09-document-error (error)
              (bad-document-response error)))))

(setf (ningle:route *app* "/new/document/:dtype" :method :post)
      #'(lambda (params)
          (handler-case
              (let* ((route-dtype (cdr (assoc :dtype params :test #'string=)))
                     (document (star.documents.v09:ensure-v09-document
                                (request-body-string)
                                :route-dtype route-dtype)))
                (set-default-headers)
                (publish-v09-document document))
            (star.documents.v09:v09-document-error (error)
              (bad-document-response error)))))

(setf (ningle:route *app* "/document/:id/schema-org" :method :get)
      #'(lambda (params)
          (set-default-headers)
          (let ((document-id (cdr (assoc :id params :test #'string=))))
            (couchdb-handler (client *couchdb-pool*)
              (let* ((document (jsown:parse
                                (cl-couch:get-document client
                                                       star:*couchdb-default-database*
                                                       document-id)))
                     (schema-org (or (jsown:val-safe document "schema_org")
                                     (spec:schema-org-metadata
                                      (star.documents.v09:document-dtype document)
                                      document-id))))
                (jsown:to-json schema-org))))))
