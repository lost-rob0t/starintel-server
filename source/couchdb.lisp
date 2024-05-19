(in-package :star.databases.couchdb)

(defun init-views (client database)
  (let ((files (uiop:directory-files (uiop:merge-pathnames* "views/" (asdf:system-source-directory :starintel-gserver)))))
    (loop for file in files
          for jdata = (with-open-file (str file)
                        (format nil "~a~%" (read-line str)))
          do (cl-couch:create-document client database jdata))))

(defun init-db ()
  "Create the database, and all map-reduce views with it."
  (let* ((client (cl-couch:new-couchdb *couchdb-host* *couchdb-port* :scheme (string-downcase "http")))
         (database *couchdb-default-database*))
    (cl-couch:password-auth client *couchdb-user* *couchdb-password*)
    (handler-case (cl-couch:get-database client database)
      (dexador:http-request-not-found () (progn
                                           (cl-couch:create-database client database)
                                           (init-views client database))))))
