(in-package :star)

(defun read-secret-file (path)
  (when (and path (probe-file path))
    (string-trim '(#\Space #\Tab #\Newline #\Return)
                 (uiop:read-file-string path))))

(defun environment-secret (value-variable file-variable)
  (or (uiop:getenv value-variable)
      (read-secret-file (uiop:getenv file-variable))))

(defun environment-boolean (name &optional default)
  (let ((value (uiop:getenv name)))
    (if value
        (member (string-downcase value)
                '("1" "true" "yes" "on")
                :test #'string=)
        default)))

(defun environment-integer (name default)
  (let ((value (uiop:getenv name)))
    (if value
        (parse-integer value :junk-allowed nil)
        default)))

(defun split-comma-setting (value)
  (when value
    (loop with start = 0
          for position = (position #\, value :start start)
          for item = (string-trim '(#\Space #\Tab)
                                  (subseq value start position))
          when (plusp (length item))
            collect item
          while position
          do (setf start (1+ position)))))

;;; Version info
(defparameter *star-server-version* "0.0.1")

;;;; CouchDB
(defparameter *couchdb-host*
  (or (uiop:getenv "COUCHDB_HOST") "127.0.0.1"))
(defparameter *couchdb-port*
  (environment-integer "COUCHDB_PORT" 5984))
(defparameter *couchdb-default-database*
  (or (uiop:getenv "COUCHDB_DATABASE") "starintel"))
(defparameter *couchdb-auth-database*
  (or (uiop:getenv "STAR_AUTH_DATABASE") "starintel-gserver-auth"))
(defparameter *couchdb-scheme*
  (or (uiop:getenv "COUCHDB_SCHEME") "http"))
(defparameter *couchdb-user*
  (or (uiop:getenv "COUCHDB_USER") "admin"))
(defparameter *couchdb-password*
  (environment-secret "COUCHDB_PASSWORD" "COUCHDB_PASSWORD_FILE"))

(defparameter *couchdb-views*
  (let ((files
          (uiop:directory-files
           (uiop:merge-pathnames*
            "views/"
            (asdf:system-source-directory :starintel-gserver)))))
    (loop for file in files
          collect
          (with-open-file (stream file)
            (let ((content (make-string (file-length stream))))
              (read-sequence content stream)
              content))))
  "View documents installed into the intelligence database at startup.")

;;;; HTTP API
(defparameter *http-api-address*
  (or (uiop:getenv "HTTP_API_LISTEN_ADDRESS") "localhost"))
(defparameter *http-api-port*
  (environment-integer "HTTP_API_PORT" 5000))
(defparameter *http-api-base-path* "/api")
(defparameter *http-cert-file* nil)
(defparameter *http-key-file* nil)
(defparameter *http-scheme* 'http)
(defparameter *public-mode*
  (not (null (environment-boolean "STAR_PUBLIC_MODE" t)))
  "When true, safe v1 read endpoints such as search and aggregate stats may be
used without credentials. Operators can set this to NIL in init.lisp (or set
STAR_PUBLIC_MODE=false before init loads) for authenticated-only deployments.")
(defparameter *http-cors-allowed-origins*
  (split-comma-setting (uiop:getenv "STAR_AUTH_ALLOWED_ORIGINS")))
(defparameter *http-cors-allowed-methods*
  "GET, POST, PUT, PATCH, DELETE, OPTIONS")
(defparameter *http-cors-allowed-headers*
  "Content-Type, Authorization, X-Correlation-ID, X-Request-Timeout-Ms, X-Star-Bootstrap-Secret")

;;;; HTTP authentication
(defparameter *auth-mode*
  (or (uiop:getenv "STAR_AUTH_MODE") "api-key"))
(defparameter *auth-pepper*
  (environment-secret "STAR_AUTH_PEPPER" "STAR_AUTH_PEPPER_FILE"))
(defparameter *auth-bootstrap-secret*
  (environment-secret
   "STAR_AUTH_BOOTSTRAP_SECRET"
   "STAR_AUTH_BOOTSTRAP_SECRET_FILE"))
(defparameter *auth-dev-bypass*
  (not (null (environment-boolean "STAR_AUTH_DEV_BYPASS" nil))))
(defparameter *auth-key-secret-bytes* 32)
(defparameter *auth-salt-bytes* 16)
(defparameter *auth-rotation-overlap-max-seconds*
  (environment-integer "STAR_AUTH_MAX_ROTATION_OVERLAP_SECONDS" 86400))
(defparameter *auth-default-request-timeout-ms*
  (environment-integer "STAR_AUTH_DEFAULT_REQUEST_TIMEOUT_MS" 30000))
(defparameter *auth-max-request-timeout-ms*
  (environment-integer "STAR_AUTH_MAX_REQUEST_TIMEOUT_MS" 600000))

;;;; OAuth authorization-code credentials
(defparameter *oauth-authorization-code-seconds*
  (environment-integer "STAR_OAUTH_AUTHORIZATION_CODE_SECONDS" 300))
(defparameter *oauth-access-token-seconds*
  (environment-integer "STAR_OAUTH_ACCESS_TOKEN_SECONDS" 900))

;;;; Human users
(defparameter *auth-initial-username*
  (or (uiop:getenv "STAR_AUTH_INITIAL_USERNAME") "star"))
(defparameter *auth-initial-password*
  (or (environment-secret
       "STAR_AUTH_INITIAL_PASSWORD"
       "STAR_AUTH_INITIAL_PASSWORD_FILE")
      "intel"))
(defparameter *auth-password-min-length*
  (environment-integer "STAR_AUTH_PASSWORD_MIN_LENGTH" 12))
(defparameter *auth-password-iterations*
  (environment-integer "STAR_AUTH_PASSWORD_ITERATIONS" 600000))
(defparameter *auth-login-session-seconds*
  (environment-integer "STAR_AUTH_LOGIN_SESSION_SECONDS" 86400))

(defparameter *auth-public-paths*
  '("/health" "/" "/auth/bootstrap" "/auth/login"
    "/oauth/authorize" "/oauth/token"))

;;;; RabbitMQ
(defparameter *rabbit-address*
  (or (uiop:getenv "RABBITMQ_ADDRESS") "localhost"))
(defparameter *rabbit-port*
  (environment-integer "RABBITMQ_PORT" 5672))
(defparameter *rabbit-user*
  (or (uiop:getenv "RABBITMQ_USER") "guest"))
(defparameter *rabbit-password*
  (environment-secret "RABBITMQ_PASSWORD" "RABBITMQ_PASSWORD_FILE"))
(defparameter *slynk-port* 4009)

;;;; Actors and patterns
(defparameter *actors-start-hook* (make-instance 'nhooks:hook-void))
(defparameter *document-patterns* nil)
(defparameter *ingest-workers* 4)

;;;; Event log and bulk operations
(defparameter *couchdb-event-log-database* "starintel-event-source")
(defparameter *bulk-max-documents* 500)

;;;; Rabbit retry and quarantine
(defparameter *rabbit-max-retries* 4
  "Maximum republished attempts after the original delivery.")
(defparameter *rabbit-retry-base-delay-ms* 250
  "Initial retry delay in milliseconds.")
(defparameter *rabbit-retry-max-delay-ms* 30000
  "Maximum exponential retry delay in milliseconds.")
(defparameter *rabbit-retry-jitter-ratio* 0.20d0
  "Symmetric retry jitter ratio.")
(defparameter *rabbit-quarantine-exchange* "starintel.quarantine"
  "Durable topic exchange receiving structured quarantine records.")
(defparameter *rabbit-quarantine-queue* "starintel-quarantine"
  "Durable queue used for poison-message inspection.")