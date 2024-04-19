;; [[file:../../source.org::*Couchdb client pool][Couchdb client pool:1]]
(defparameter *couchdb-pool*
  (anypool:make-pool :name "couchdb-connections"
                     :connector (lambda ()
                                  (let ((client (cl-couch:new-couchdb (uiop:getenv "COUCHDB_HOST") 5984 :scheme (string-downcase (uiop:getenv "COUCHDB_SCHEME")))))
                                    (cl-couch:password-auth client (uiop:getenv "COUCHDB_USER") (uiop:getenv "COUCHDB_PASSWORD"))
                                    client))

                     :disconnector (lambda (obj)
                                     (setf (cl-couch:couchdb-headers obj) nil))
                     :max-open-count 20))
;; Couchdb client pool:1 ends here

;; [[file:../../source.org::*Submit documents][Submit documents:1]]
(setf (ningle:route *app* "/new/document/:dtype" :method :post)
      #'(lambda (params)

          (let ((dtype  (cdr (assoc :dtype params :test #'string=)))
                (doc (car (car (lack.request:request-body-parameters (ningle:context :request))))))

            (star.rabbit:emit-document  "new-documents" "documents"  (format nil "documents.new.~a" dtype) doc  :properties (list (list :headers `("dtype" . ,dtype))))
             (format nil "documents.new.~a" dtype))))
;; Submit documents:1 ends here

;; [[file:../../source.org::*Get Documents][Get Documents:1]]
(setf (ningle:route *app* "/document/:id" :method :get)
      #'(lambda (params)

          (let ((document-id  (cdr (assoc :id params :test #'string=))))

            (anypool:with-connection (client *couchdb-pool*)
              (cl-couch:get-document client "starintel" document-id)))))
;; Get Documents:1 ends here

;; [[file:../../source.org::*Start webapp][Start webapp:1]]
;(couchdb-middleware *app*)
(defun start-http-api ()
  (clack:clackup *app* :address star:*http-api-address* :port star:*http-api-port*))
;; Start webapp:1 ends here
