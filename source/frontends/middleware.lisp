(in-package #:lack/middleware/couchdb-pool)
(defparameter *connection-pool-storage*
  (make-hash-table :test 'eq))

(defun make-couchdb (&key host port user password (scheme "http"))
  (assert (and host user password scheme))
  (let ((client (cl-couch:new-couchdb host port :scheme scheme)))
    (cl-couch:password-auth client user password)
    client))

(defun make-connection-pool (connect-args pool-args)
  (apply #'anypool:make-pool
         :connector (lambda () (apply #'make-couchdb connect-args))
         :disconnector (lambda (obj) (setf (cl-couch:couchdb-headers obj) nil))
         ;; TODO configure PING
         pool-args))

(defun get-connection-pool (database-id)
  (or (gethash database-id *connection-pool-storage*)
      (error "No connection pool found for ~S" database-id)))

(defmacro with-couchdb ((var database-id) &body body)
  (let ((e (gensym "E")))
    `(block nil
       ;; TODO These should return json
       (handler-bind ((dex:http-request-conflict
                        (lambda (,e)
                          (declare (ignore ,e))
                          (return '(409 (:content-type "text/plain") ("Document Conflict")))))
                      (dex:http-request-not-found
                        (lambda (,e)
                          (declare (ignore ,e))
                          (return '(404 (:content-type "text/plain") ("Document Not found")))))
                      (anypool:too-many-open-connection
                        (lambda (,e)
                          (declare (ignore ,e))
                          (return '(503 (:content-type "text/plain") ("Service Temporarily Unavailable"))))))
         (anypool:with-connection (,var (get-connection-pool ,database-id))
           ,@body)))))

(defparameter *lack-middleware-couchdb-pool*
  (lambda (app id &key connect-args pool-args)
    (check-type id symbol)
    (assert connect-args)
    (let ((pool (make-connection-pool connect-args pool-args)))
      (setf (gethash id *connection-pool-storage*) pool)
      (lambda (env)
        (funcall app env)))))
