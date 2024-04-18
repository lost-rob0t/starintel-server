;; [[file:../../source.org::*Auth Database][Auth Database:1]]
(in-package :star.frontends.http-api)
(defun init-database (username password &optional (host "127.0.0.1") (port 5984))
  "Add the couchdb object to the context, should only be called once!"
  (let ((client (cl-couch:new-couchdb host port)))
    (cl-couch:password-auth client username password)
    client))

(defun init-state (couchdb)
  "Create the needed databases, map-reduce views. ")
;; Auth Database:1 ends here

;; [[file:../../source.org::*Auth Database][Auth Database:2]]
(in-package :star.frontends.http-api)

;; (defclass app (ningle:app)
;;   ()
;;   (:documentation "Custom application based on NINGLE:APP"))

;; (defparameter *couchdb*
;;   "*REQUEST-ENV* will be dynamically bound to the environment context
;; of HTTP requests")


                                        ;(defun setup-couchdb)


(defvar *app* (make-instance 'ningle:app))
;; Auth Database:2 ends here

;; [[file:../../source.org::*Submit documents][Submit documents:1]]
(setf (ningle:route *app* "/new/document/:dtype" :method :post)
      #'(lambda (params)

          (let ((dtype  (cdr (assoc :dtype params :test #'string=)))
                (doc (car (car (lack.request:request-body-parameters (ningle:context :request))))))

            (star.rabbit:emit-document  "new-documents" "documents"  (format nil "documents.new.~a" dtype) doc  :properties (list (list :headers `("dtype" . ,dtype))))
             (format nil "documents.new.~a" dtype))))
;; Submit documents:1 ends here

;; [[file:../../source.org::*Start webapp][Start webapp:1]]
                                        ;(couchdb-middleware *app*)
(defun start-http-api ()
  (clack:clackup *app* :address star:*http-api-address* :port star:*http-api-port*))
;; Start webapp:1 ends here
