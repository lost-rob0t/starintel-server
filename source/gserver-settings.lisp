(in-package :star)
;;;; ** Gserver Settings
;;;; *** Couchdb
(defparameter *couchdb-host* (or (uiop:getenv "COUCHDB_HOST") "127.0.0.1") "The Couchdb host to use.
Defaults to using ENV var $COUCHDB_HOST if set, or localhost ")
(defparameter *couchdb-port* 5984 "The Couchdb port to use. Defaults to 5984")
(defparameter *couchdb-default-database* (or (uiop:getenv "COUCHDB_DATABASE") "starintel") "the default database name to use.")

(defparameter *couchdb-auth-database* "starintel-gserver-auth")
(defparameter *couchdb-scheme* "http" "what http scheme to use. set to http or https")
(defparameter *couchdb-user* (or (uiop:getenv "COUCHDB_USER") "admin") "couchdb user")
(defparameter *couchdb-password* (or (uiop:getenv "COUCHDB_PASSWORD") "password") "couchdb user password")
;;;; By Default the views in starintel-gserver/views will be installed, but you can append your own to this setting to have it created at startup.
(defparameter *couchdb-views* (let ((files (uiop:directory-files (uiop:merge-pathnames* "views/" (asdf:system-source-directory :starintel-gserver)))))
                                (loop for file in files
                                      collect (with-open-file (str file)
                                                (read-line str))))
  "List of views to install into couchdb.")

;;;; *** HTTP API
(defparameter *http-api-address* (or (uiop:getenv "HTTP_API_LISTEN_ADDRESS") "localhost") "the listen address")
(defparameter *http-api-port* 5000  "the port the api server listen on")
(defparameter *http-api-base-path* "/api" "the base url to use for the api endpoint")
(defparameter *http-cert-file* nil "path to the http api cert providing https")
(defparameter *http-key-file* nil "path to the http cert providing https")
(defparameter *http-scheme* 'http "use https or not.")

;;;; *** RabbitMQ
(defparameter *rabbit-address* (or (uiop:getenv "RABBITMQ_ADDRESS") "localhost") "The address rabbitmq is running on.")
(defparameter *rabbit-port* 5672 "The port that rabbitmq is listening on.")
(defparameter *rabbit-user* "guest" "the username for rabbimq")
(defparameter *rabbit-password* "guest" "the password for the rabbitmq user.")
(defparameter *slynk-port* 4009 "Port to use for SLYNK remote debugging")

;;;; *** Actors
;;;; Hooks are implemented Via nhooks you can read documentation here for how to add hooks. https://github.com/atlas-engineer/nhooks
(defparameter *actors-start-hook* (make-instance 'nhooks:hook-void) "Actor startup hook.")
;;;; *** Patterns
;;;; Patterns are
(defparameter *document-patterns* () "A List of document patterns created by defpattern")
(defparameter *injest-workers* 4 "Number of workers for handling documents, set to 4 by default.")
;;;; *** actor event log
(defparameter *couchdb-event-log-database* "starintel-event-source" "The name of the database to be used for event logs.")
