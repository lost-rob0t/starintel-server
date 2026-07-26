(in-package :star)

(setf *couchdb-host* (or (uiop:getenv "COUCHDB_HOST") *couchdb-host*)
      *couchdb-default-database*
      (or (uiop:getenv "COUCHDB_DATABASE") *couchdb-default-database*)
      *couchdb-user* (or (uiop:getenv "COUCHDB_USER") *couchdb-user*)
      *couchdb-password*
      (or (uiop:getenv "COUCHDB_PASSWORD") *couchdb-password*)
      *rabbit-address* (or (uiop:getenv "RABBITMQ_ADDRESS") *rabbit-address*)
      *rabbit-user* (or (uiop:getenv "RABBITMQ_USER") *rabbit-user*)
      *rabbit-password*
      (or (uiop:getenv "RABBITMQ_PASSWORD") *rabbit-password*)
      *http-api-address*
      (or (uiop:getenv "HTTP_API_LISTEN_ADDRESS") *http-api-address*))
