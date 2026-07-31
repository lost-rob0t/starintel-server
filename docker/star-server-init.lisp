(in-package :star)

(setf *couchdb-host*
      (or (uiop:getenv "COUCHDB_HOST") *couchdb-host*)
      *couchdb-default-database*
      (or (uiop:getenv "COUCHDB_DATABASE") *couchdb-default-database*)
      *couchdb-auth-database*
      (or (uiop:getenv "STAR_AUTH_DATABASE") *couchdb-auth-database*)
      *couchdb-user*
      (or (uiop:getenv "COUCHDB_USER") *couchdb-user*)
      *couchdb-password*
      (or (uiop:getenv "COUCHDB_PASSWORD") *couchdb-password*)
      *rabbit-address*
      (or (uiop:getenv "RABBITMQ_ADDRESS") *rabbit-address*)
      *rabbit-user*
      (or (uiop:getenv "RABBITMQ_USER") *rabbit-user*)
      *rabbit-password*
      (or (uiop:getenv "RABBITMQ_PASSWORD") *rabbit-password*)
      *http-api-address*
      (or (uiop:getenv "HTTP_API_LISTEN_ADDRESS") *http-api-address*)
      *http-cors-allowed-origins*
      (or (split-comma-setting (uiop:getenv "STAR_AUTH_ALLOWED_ORIGINS"))
          *http-cors-allowed-origins*)
      *auth-mode*
      (or (uiop:getenv "STAR_AUTH_MODE") *auth-mode*)
      *auth-pepper*
      (or (uiop:getenv "STAR_AUTH_PEPPER") *auth-pepper*)
      *auth-bootstrap-secret*
      (or (uiop:getenv "STAR_AUTH_BOOTSTRAP_SECRET")
          *auth-bootstrap-secret*)
      *auth-dev-bypass*
      (not (null
            (environment-boolean
             "STAR_AUTH_DEV_BYPASS"
             *auth-dev-bypass*)))
      *auth-rotation-overlap-max-seconds*
      (environment-integer
       "STAR_AUTH_MAX_ROTATION_OVERLAP_SECONDS"
       *auth-rotation-overlap-max-seconds*)
      *auth-default-request-timeout-ms*
      (environment-integer
       "STAR_AUTH_DEFAULT_REQUEST_TIMEOUT_MS"
       *auth-default-request-timeout-ms*)
      *auth-max-request-timeout-ms*
      (environment-integer
       "STAR_AUTH_MAX_REQUEST_TIMEOUT_MS"
       *auth-max-request-timeout-ms*))
