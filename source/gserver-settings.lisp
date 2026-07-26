(in-package :star)

(defparameter *couchdb-auth-database* "starintel-gserver-auth")
(defparameter *couchdb-scheme* "http" "what http scheme to use. set to http or https")
(defparameter *couchdb-user* (or (uiop:getenv "COUCHDB_USER") "admin") "couchdb user")
(defparameter *couchdb-password* (uiop:getenv "COUCHDB_PASSWORD") "couchdb user password")
;;;; By Default the views in starintel-gserver/views will be installed, but you can append your own to this setting to have it created at startup.
(defparameter *couchdb-views* (let ((files (uiop:directory-files (uiop:merge-pathnames* "views/" (asdf:system-source-directory :starintel-gserver)))))
                                (loop for file in files
                                      collect (with-open-file (str file)
                                                (let ((content (make-string (file-length str))))
                                                  (read-sequence content str)
                                                  content))))
  "List of views to install into couchdb.")

(defparameter *couchdb-host* +default-couchdb-host+
  "The Couchdb host to use. Uses ENV var COUCHDB_HOST if set.")
(defparameter *couchdb-port* +default-couchdb-port+
  "The Couchdb port to use. Defaults to 5984.")
(defparameter *couchdb-default-database* +default-couchdb-default-database+
  "The default database name to use.")
(defparameter *couchdb-user* +default-couchdb-user+
  "Couchdb user name.")
(defparameter *couchdb-password* +default-couchdb-password+
  "Couchdb user password.")

;;;; *** RabbitMQ
(defparameter *rabbit-address* (or (uiop:getenv "RABBITMQ_ADDRESS") "localhost") "The address rabbitmq is running on.")
(defparameter *rabbit-port* 5672 "The port that rabbitmq is listening on.")
(defparameter *rabbit-user* (or (uiop:getenv "RABBITMQ_USER") "guest") "the username for rabbitmq")
(defparameter *rabbit-password* (uiop:getenv "RABBITMQ_PASSWORD") "the password for the rabbitmq user.")
(defparameter *slynk-port* 4009 "Port to use for SLYNK remote debugging")

;;;; *** Actors
;;;; Hooks are implemented Via nhooks you can read documentation here for how to add hooks. https://github.com/atlas-engineer/nhooks
(defparameter *actors-start-hook* (make-instance 'nhooks:hook-void) "Actor startup hook.")
;;;; *** Patterns
;;;; Patterns are
(defparameter *document-patterns* () "A List of document patterns created by defpattern")
(defparameter *ingest-workers* 4 "Number of workers for handling documents, set to 4 by default.")
;;;; *** actor event log
(defparameter *couchdb-event-log-database* "starintel-event-source" "The name of the database to be used for event logs.")
;;;; *** Bulk operations
(defparameter *bulk-max-documents* 500 "Maximum number of documents allowed in a single bulk operation.")
