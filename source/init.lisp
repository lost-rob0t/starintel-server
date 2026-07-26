(in-package :starintel-gserver)

(defun read-view-document (path)
  (jsown:with-injective-reader
    (jsown:parse (uiop:read-file-string path))))

(defun upsert-view-document (client database document)
  (let ((document-id (jsown:val document "_id")))
    (handler-case
        (let* ((current
                 (jsown:with-injective-reader
                   (jsown:parse
                    (cl-couch:get-document client database document-id))))
               (revision (jsown:val current "_rev")))
          (setf (jsown:val document "_rev") revision)
          (cl-couch:create-document
           client database (jsown:to-json document)))
      (dexador:http-request-not-found ()
        (cl-couch:create-document
         client database (jsown:to-json document))))))

(defun init-views (client database)
  "Create or update every checked-in CouchDB design document."
  (let ((files
          (uiop:directory-files
           (uiop:merge-pathnames*
            "views/"
            (asdf:system-source-directory :starintel-gserver)))))
    (loop for file in files
          when (string-equal "json" (pathname-type file))
            do (upsert-view-document
                client database (read-view-document file)))))

(defun init-db ()
  "Create the database, install design docs, and validate every wrapper target."
  (let* ((client
           (cl-couch:new-couchdb
            *couchdb-host*
            *couchdb-port*
            :scheme (string-downcase *couchdb-scheme*)))
         (database *couchdb-default-database*))
    (cl-couch:password-auth client *couchdb-user* *couchdb-password*)
    (handler-case
        (cl-couch:get-database client database)
      (dexador:http-request-not-found ()
        (cl-couch:create-database client database)))
    (star.databases.couchdb:validate-view-registry)
    (init-views client database)
    t))
