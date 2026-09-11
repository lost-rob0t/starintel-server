;;;; StarIntel Server init file
;;;;
;;;; This file is executable Common Lisp. Load it only from a trusted,
;;;; root/operator-controlled path.

(in-package :star)

(format t "~&Starting StarIntel Server configuration...~%")

;;; CouchDB -----------------------------------------------------------------

(setf *couchdb-host* (or (uiop:getenv "COUCHDB_HOST") "127.0.0.1")
      *couchdb-port* 5984
      *couchdb-scheme* "http"
      *couchdb-user* (or (uiop:getenv "COUCHDB_USER") "admin")
      *couchdb-password* (or (uiop:getenv "COUCHDB_PASSWORD") "")
      *couchdb-default-database*
      (or (uiop:getenv "COUCHDB_DATABASE") "starintel")
      *couchdb-event-log-database* "starintel-event-source")

;;; RabbitMQ ---------------------------------------------------------------

(setf *rabbit-address* (or (uiop:getenv "RABBITMQ_ADDRESS") "127.0.0.1")
      *rabbit-port* 5672
      *rabbit-user* (or (uiop:getenv "RABBITMQ_USER") "guest")
      *rabbit-password* (or (uiop:getenv "RABBITMQ_PASSWORD") ""))

;;; HTTP -------------------------------------------------------------------

(setf *http-api-address*
      (or (uiop:getenv "HTTP_API_LISTEN_ADDRESS") "127.0.0.1")
      *http-api-port* 5000
      ;; Public-read mode is the default. Set NIL for authenticated-only
      ;; /api/v1/search and /api/v1/stats deployments.
      *public-mode* t
      *bulk-max-documents* 500)

;;; Concurrency ------------------------------------------------------------

(setf *ingest-workers* 4
      star.actors:*publish-timeout-seconds* 5)

;;; Logging ----------------------------------------------------------------

(ensure-directories-exist #P"logs/")
(log:config :daily "logs/star-server.log"
            :file2
            :sane)

;;; Optional local actors --------------------------------------------------
;;;
;;; Actor definition files loaded here can add startup/registration functions
;;; to STAR:*ACTORS-START-HOOK*. The hook runs after the Sento actor system,
;;; producer agent, database actors, target timer, and target router exist.
;;;
;;; Example:
;;; (load #P"/etc/starintel/actors/domain-enricher.lisp")

;;; Optional addons ---------------------------------------------------------
;;;
;;; Addons are trusted operator code loaded through the addon lifecycle and
;;; started explicitly. Nothing below runs unless the operator adds it here.
;;;
;;; Observability: OTLP export of logs, metrics and traces to the local
;;; OpenTelemetry Collector. Off unless this line is present; the export
;;; target and signal mix are controlled by STAR_OBSERVABILITY_* env vars.
;;; The application holds no OpenObserve credentials.
;;;
;;; (load-addon :starintel-observability)
;;;
;;; Example:
;;; (load-addon :starintel-bixby)

;;; Optional SLYNK debugger ------------------------------------------------
;;;
;;; SLYNK is remote code execution. Use loopback/SSH forwarding only.
;;;
;;; (setf *slynk-port* 4009)
;;; (start-debugger)

(format t "~&StarIntel configuration loaded.~%")
