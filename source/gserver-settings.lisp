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
(defparameter *star-server-version* "0.0.1"
  "Server version string reported by =/health= and admin endpoints.")

;;;; CouchDB
(defparameter *couchdb-host*
  (or (uiop:getenv "COUCHDB_HOST") "127.0.0.1")
  "CouchDB host.

- env: =COUCHDB_HOST=
- default: =127.0.0.1=")
(defparameter *couchdb-port*
  (environment-integer "COUCHDB_PORT" 5984)
  "CouchDB port.

- env: =COUCHDB_PORT=
- default: =5984=")
(defparameter *couchdb-default-database*
  (or (uiop:getenv "COUCHDB_DATABASE") "starintel")
  "Intelligence database name.

Documents, targets and views live here.

- env: =COUCHDB_DATABASE=
- default: =starintel=")
(defparameter *couchdb-auth-database*
  (or (uiop:getenv "STAR_AUTH_DATABASE") "starintel-gserver-auth")
  "Auth database name for credentials, users and sessions.

- env: =STAR_AUTH_DATABASE=
- default: =starintel-gserver-auth=")
(defparameter *couchdb-scheme*
  (or (uiop:getenv "COUCHDB_SCHEME") "http")
  "CouchDB URL scheme, =http= or =https=.

- env: =COUCHDB_SCHEME=
- default: =http=")
(defparameter *couchdb-user*
  (or (uiop:getenv "COUCHDB_USER") "admin")
  "CouchDB admin user.

- env: =COUCHDB_USER=
- default: =admin=")
(defparameter *couchdb-password*
  (environment-secret "COUCHDB_PASSWORD" "COUCHDB_PASSWORD_FILE")
  "CouchDB password, read from =COUCHDB_PASSWORD= or the file
=COUCHDB_PASSWORD_FILE=.

Never log this value; session renewal relies on it.")

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
  "View documents installed into the intelligence database at startup.

Loaded from the =views/= directory of the system source.")

;;;; HTTP API
(defparameter *http-api-address*
  (or (uiop:getenv "HTTP_API_LISTEN_ADDRESS") "localhost")
  "HTTP API bind address.

- env: =HTTP_API_LISTEN_ADDRESS=
- default: =localhost=")
(defparameter *http-api-port*
  (environment-integer "HTTP_API_PORT" 5000)
  "HTTP API port.

- env: =HTTP_API_PORT=
- default: =5000=")
(defparameter *http-api-base-path* "/api"
  "URL prefix for all v1 API routes.")
(defparameter *http-cert-file* nil
  "PEM certificate for TLS.  Both this and =*http-key-file*= must be set
to serve HTTPS; otherwise the API listens plain HTTP.")
(defparameter *http-key-file* nil
  "PEM private key for TLS; see =*http-cert-file*=.")
(defparameter *http-scheme* 'http
  "Effective scheme, =http= or =https=, derived from TLS configuration.")
(defparameter *public-mode*
  (not (null (environment-boolean "STAR_PUBLIC_MODE" t)))
  "When true, safe v1 read endpoints such as search and aggregate stats may be
used without credentials. Operators can set this to NIL in init.lisp (or set
STAR_PUBLIC_MODE=false before init loads) for authenticated-only deployments.")
(defparameter *http-cors-allowed-origins*
  (split-comma-setting (uiop:getenv "STAR_AUTH_ALLOWED_ORIGINS"))
  "Allowed CORS origins, split from a comma separated list.

- env: =STAR_AUTH_ALLOWED_ORIGINS=")
(defparameter *http-cors-allowed-methods*
  "GET, POST, PUT, PATCH, DELETE, OPTIONS"
  "Allowed CORS methods header value.")
(defparameter *http-cors-allowed-headers*
  "Content-Type, Authorization, X-Correlation-ID, X-Request-Timeout-Ms, X-Star-Bootstrap-Secret"
  "Allowed CORS headers header value.")

;;;; HTTP authentication
(defparameter *auth-mode*
  (or (uiop:getenv "STAR_AUTH_MODE") "api-key")
  "Authentication mode: =api-key= or =user= password logins.

- env: =STAR_AUTH_MODE=
- default: =api-key=")
(defparameter *auth-pepper*
  (environment-secret "STAR_AUTH_PEPPER" "STAR_AUTH_PEPPER_FILE")
  "Server-side pepper mixed into password hashing.

Read from =STAR_AUTH_PEPPER= or =STAR_AUTH_PEPPER_FILE=.  Keep stable
across deployments; changing it invalidates every stored verifier.")
(defparameter *auth-bootstrap-secret*
  (environment-secret
   "STAR_AUTH_BOOTSTRAP_SECRET"
   "STAR_AUTH_BOOTSTRAP_SECRET_FILE")
  "One-time secret that guards =/auth/bootstrap=.

Read from =STAR_AUTH_BOOTSTRAP_SECRET= or the =*_FILE= variant.  Set it
before first boot; unset it after bootstrapping the first admin key.")
(defparameter *auth-dev-bypass*
  (not (null (environment-boolean "STAR_AUTH_DEV_BYPASS" nil)))
  "Dangerous: disables authentication entirely when true.

Set via =STAR_AUTH_DEV_BYPASS=true=.  Never enable outside a local
developer sandbox.")
(defparameter *auth-key-secret-bytes* 32
  "Entropy of generated API key secrets, in bytes (256 bits).")
(defparameter *auth-salt-bytes* 16
  "Per-credential salt size, in bytes.")
(defparameter *auth-rotation-overlap-max-seconds*
  (environment-integer "STAR_AUTH_MAX_ROTATION_OVERLAP_SECONDS" 86400)
  "How long an old API key stays valid after rotation.

- env: =STAR_AUTH_MAX_ROTATION_OVERLAP_SECONDS=
- default: =86400= (one day)")
(defparameter *auth-default-request-timeout-ms*
  (environment-integer "STAR_AUTH_DEFAULT_REQUEST_TIMEOUT_MS" 30000)
  "Default per-request deadline in milliseconds.

Clients may send =X-Request-Timeout-Ms= up to =*auth-max-request-timeout-ms*=.")
(defparameter *auth-max-request-timeout-ms*
  (environment-integer "STAR_AUTH_MAX_REQUEST_TIMEOUT_MS" 600000)
  "Maximum accepted per-request deadline in milliseconds.")

;;;; OAuth authorization-code credentials
(defparameter *oauth-authorization-code-seconds*
  (environment-integer "STAR_OAUTH_AUTHORIZATION_CODE_SECONDS" 300))
(defparameter *oauth-access-token-seconds*
  (environment-integer "STAR_OAUTH_ACCESS_TOKEN_SECONDS" 900))

;;;; Human users
(defparameter *auth-initial-username*
  (or (uiop:getenv "STAR_AUTH_INITIAL_USERNAME") "star")
  "Username of the bootstrapped human user.

- env: =STAR_AUTH_INITIAL_USERNAME=
- default: =star=")
(defparameter *auth-initial-password*
  (or (environment-secret
       "STAR_AUTH_INITIAL_PASSWORD"
       "STAR_AUTH_INITIAL_PASSWORD_FILE")
      "intel")
  "Initial password of the bootstrapped user.

Read from =STAR_AUTH_INITIAL_PASSWORD= or =STAR_AUTH_INITIAL_PASSWORD_FILE=.
Change it immediately after first login.")
(defparameter *auth-password-min-length*
  (environment-integer "STAR_AUTH_PASSWORD_MIN_LENGTH" 12)
  "Minimum accepted password length.")
(defparameter *auth-password-iterations*
  (environment-integer "STAR_AUTH_PASSWORD_ITERATIONS" 600000)
  "PBKDF2 iteration count for password hashing.")
(defparameter *auth-login-session-seconds*
  (environment-integer "STAR_AUTH_LOGIN_SESSION_SECONDS" 86400)
  "Lifetime of a login session, in seconds.")

(defparameter *auth-public-paths*
  '("/health" "/" "/auth/bootstrap" "/auth/login"
    "/oauth/authorize" "/oauth/token")
  "Path prefixes reachable without credentials.

Keep this list minimal; everything else is authenticated.")

;;;; RabbitMQ
(defparameter *rabbit-address*
  (or (uiop:getenv "RABBITMQ_ADDRESS") "localhost")
  "RabbitMQ host.

- env: =RABBITMQ_ADDRESS=
- default: =localhost=")
(defparameter *rabbit-port*
  (environment-integer "RABBITMQ_PORT" 5672)
  "RabbitMQ AMQP port.

- env: =RABBITMQ_PORT=
- default: =5672=")
(defparameter *rabbit-user*
  (or (uiop:getenv "RABBITMQ_USER") "guest")
  "RabbitMQ user.

- env: =RABBITMQ_USER=
- default: =guest=")
(defparameter *rabbit-password*
  (environment-secret "RABBITMQ_PASSWORD" "RABBITMQ_PASSWORD_FILE")
  "RabbitMQ password, from =RABBITMQ_PASSWORD= or =RABBITMQ_PASSWORD_FILE=.")
(defparameter *slynk-port* 4009
  "Port for the SLY/Slynk REPL when =start-debugger= is invoked.")

;;;; Actors and patterns
(defparameter *actors-start-hook* (make-instance 'nhooks:hook-void)
  "Hook run after the actor system boots.

Plugins may register functions here to start their own actors.")
(defparameter *document-patterns* nil
  "Pattern matchers applied to every ingested document.

Populated by plugins; see =addons.lisp=.")
(defparameter *ingest-workers* 4
  "Number of parallel ingest workers consuming the ingest queue.")

;;;; Event log and bulk operations
(defparameter *couchdb-event-log-database* "starintel-event-source"
  "Event sourced log database; outbox and settlement events land here.")
(defparameter *bulk-max-documents* 500
  "Maximum documents accepted by the bulk ingest endpoint per request.")

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