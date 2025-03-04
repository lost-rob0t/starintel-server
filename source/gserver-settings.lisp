(in-package :star)

(defparameter +default-couchdb-host+ "127.0.0.1")
(defparameter +default-couchdb-port+ 5984)
(defparameter +default-couchdb-default-database+ "starintel")
(defparameter +default-couchdb-user+ "admin")
(defparameter +default-couchdb-password+ "password")
(defparameter +default-http-api-address+ "localhost")
(defparameter +default-rabbit-address+ "localhost")
(defparameter +default-rabbit-port+ 5672)
(defparameter +default-rabbit-user+ "guest")
(defparameter +default-rabbit-password+ "guest")
(defparameter +default-slynk-port+ 4009)

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

(defparameter *couchdb-views*
  (when (asdf:system-source-directory :starintel-gserver)
    (let* ((views-dir (uiop::merge-pathnames* "views/" (asdf:system-source-directory :starintel-gserver))))
      (when (probe-file views-dir)
        (loop for file in (uiop:directory-files views-dir)
              collect (with-open-file (str (uiop:merge-pathnames* file views-dir))
                        (read-line str))))))
  "List of views to install into Couchdb.")

;; HTTP API configuration
(defparameter *http-api-address* +default-http-api-address+
  "The address on which the HTTP API listens.")
(defparameter *http-api-port* 5000  "Port for the HTTP API.")
(defparameter *http-api-base-path* "/api" "Base URL for the API endpoint.")
(defparameter *http-cert-file* nil "Path to the HTTPS cert file.")
(defparameter *http-key-file* nil "Path to the HTTPS key file.")
(defparameter *http-scheme* 'http "HTTP scheme to use (http or https).")

;; RabbitMQ configuration
(defparameter *rabbit-address* +default-rabbit-address+
  "The RabbitMQ host.")
(defparameter *rabbit-port* +default-rabbit-port+
  "The port for RabbitMQ.")
(defparameter *rabbit-user* +default-rabbit-user+
  "The RabbitMQ user name.")
(defparameter *rabbit-password* +default-rabbit-password+
  "The RabbitMQ user password.")

;; Other configurations
(defparameter *slynk-port* +default-slynk-port+
  "Port to use for SLYNK remote debugging.")
(defparameter *actors-start-hook* (make-instance 'nhooks:hook-void)
  "Actor startup hook.")
(defparameter *document-patterns* () "A list of document patterns created by defpattern.")
(defparameter *injest-workers* 4 "Number of workers for handling documents.")
(defparameter *couchdb-event-log-database* "starintel-event-source"
  "The database name used for event logs.")
