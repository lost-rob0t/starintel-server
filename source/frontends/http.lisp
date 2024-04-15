(uiop:define-package   :star.frontend.http
  (:nicknames :frontend.http)
  (:use       :cl)
  (:documentation "doc"))

(in-package :star.frontend.http)
(defun init-database (username password &optional (host "127.0.0.1") (port 5984))
  "Add the couchdb object to the context, should only be called once!"
  (let ((client (cl-couch:new-couchdb host port)))
    (cl-couch:password-auth client username password)
    client))

(defun init-state (couchdb)
    "Create the needed databases, map-reduce views. ")

(in-package :star.frontend.http)

(defclass app (ningle:app)
  ()
  (:documentation "Custom application based on NINGLE:APP"))

(defparameter *couchdb*
  "*REQUEST-ENV* will be dynamically bound to the environment context
of HTTP requests")





(defun couchdb-middleware (app)
  "A custom middleware which wraps a NINGLE:APP and injects additional
metadata into the environment for HTTP handlers/controllers as part of
each HTTP request"
  (lambda (env)
    (setf (getf env :couchdb-middleware/client) (init-database "admin" "password"))
    (funcall app env)))

(defmethod lack.component:call ((app app) env)
  ;; Dynamically bind *REQUEST-ENV* for each request, so that ningle
  ;; routes can access the environment.
  (let ((*couchdb* env))
    (call-next-method)))

(defvar *app* (make-instance 'app))

(setf (ningle:route *app* "/submit/:operation/:dtype/:id")
      #'(lambda (args)
          (format nil "~a" args)))

(couchdb-middleware *app*)
(defparameter *server* (clack:clackup *app* :address *listen-address* :port *port*))
