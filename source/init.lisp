(in-package :starintel-gserver)


(defun init-db ()
  "Create the database, and all map-reduce views with it."
  (let* ((client (cl-couch:new-couchdb (uiop:getenv "COUCHDB_HOST") (parse-integer (or (uiop:getenv "COUCHDB_PORT") 5984)) :scheme (string-downcase (uiop:getenv "COUCHDB_SCHEME"))))
         (database (or (uiop:getenv "COUCHDB_DATABASE") "starintel")))
    (cl-couch:password-auth client (uiop:getenv "COUCHDB_USER") (uiop:getenv "COUCHDB_PASSWORD"))
    (handler-case (cl-couch:get-database client database)
      (dexador:http-request-not-found () (cl-couch:create-database client database)))))
