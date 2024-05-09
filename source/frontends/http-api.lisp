;; http-api


;; [[file:../../source.org::*http-api][http-api:1]]
(in-package :star.frontends.http-api)
(defvar *app* (make-instance 'ningle:app))
;; http-api:1 ends here

;; [[file:../../source.org::*Couchdb client pool][Couchdb client pool:1]]
(in-package :star.frontends.http-api)
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

;; [[file:../../source.org::*Get Targets for actor][Get Targets for actor:1]]
(setf (ningle:route *app* "/targets/:actor" :method :get)
      #'(lambda (params)
          (let ((targets (loop for row in (anypool:with-connection (client *couchdb-pool*)
                                            (jsown:val (jsown:parse (cl-couch:get-view client star:*couchdb-default-database* "targets" "actor-targets" (jsown:to-json
                                                                                                                                                         (jsown:new-js
                                                                                                                                                           ("include_docs" "true")
                                                                                                                                                           ("keys" (list (cdr (assoc :actor params :test #'string=)))))))) "rows"))
                               collect (jsown:val row "doc"))))
            (jsown:to-json targets))))
;; Get Targets for actor:1 ends here

;; [[file:../../source.org::*Create Target][Create Target:1]]
(setf (ningle:route *app* "/new/target/:actor" :method :post)
      #'(lambda (params)

          (let* ((actor  (cdr (assoc :actor params :test #'string=)))
                 (body (babel:octets-to-string  (lack.request:request-content (ningle:context :request)) :encoding :utf-8))
                 (routing-key (format nil "documents.new.target.~a" actor)))
            (cl-rabbit:with-connection (conn)
              (let ((socket (cl-rabbit:tcp-socket-new conn)))
                (cl-rabbit:socket-open socket star:*rabbit-address* star:*rabbit-port*)
                (cl-rabbit:login-sasl-plain conn "/" star:*rabbit-user* star:*rabbit-password*)
                (cl-rabbit:with-channel (conn 1)
                  (cl-rabbit:basic-publish conn 1 :routing-key routing-key :exchange "documents"  :properties (list (cons :type "target" )) :body body))))
            body)))
;; Create Target:1 ends here

;; [[file:../../source.org::*Submit documents][Submit documents:1]]
;; (setf (ningle:route *app* "/new/document/:dtype" :method :post)
;;       #'(lambda (params)

;;           (let ((dtype  (cdr (assoc :dtype params :test #'string=)))
;;                 (body (lack.request:request-content (ningle:context :request))))

;;             (star.consumers:emit-document  "new-documents" "documents"  (format nil "documents.new.~a" dtype) body :properties (list (list :headers `("dtype" . ,dtype))))
;;             (format nil "documents.new.~a" dtype))))


(setf (ningle:route *app* "/new/document/:dtype" :method :post)
      #'(lambda (params)

          (let* ((dtype  (cdr (assoc :dtype params :test #'string=)))
                 (body (babel:octets-to-string  (lack.request:request-content (ningle:context :request)) :encoding :utf-8))
                 (routing-key (format nil "documents.new.~a" dtype)))
            (cl-rabbit:with-connection (conn)
              (let ((socket (cl-rabbit:tcp-socket-new conn)))
                (cl-rabbit:socket-open socket star:*rabbit-address* star:*rabbit-port*)
                (cl-rabbit:login-sasl-plain conn "/" star:*rabbit-user* star:*rabbit-password*)
                (cl-rabbit:with-channel (conn 1)
                  (cl-rabbit:basic-publish conn 1 :routing-key routing-key :exchange "documents"  :properties (list (cons :type dtype )) :body body))))
            body)))
;; Submit documents:1 ends here

;; [[file:../../source.org::*Get Documents][Get Documents:1]]
(setf (ningle:route *app* "/document/:id" :method :get)
      #'(lambda (params)

          (let ((document-id  (cdr (assoc :id params :test #'string=))))

            (anypool:with-connection (client *couchdb-pool*)
              (cl-couch:get-document client star:*couchdb-default-database* document-id)))))
;; Get Documents:1 ends here

;; [[file:../../source.org::*Start webapp][Start webapp:1]]
                                        ;(couchdb-middleware *app*)
(defun start-http-api ()
  (clack:clackup *app* :address star:*http-api-address* :port star:*http-api-port*))
;; Start webapp:1 ends here
