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
      (or (environment-secret "COUCHDB_PASSWORD" "COUCHDB_PASSWORD_FILE")
          *couchdb-password*)
      *rabbit-address*
      (or (uiop:getenv "RABBITMQ_ADDRESS") *rabbit-address*)
      *rabbit-user*
      (or (uiop:getenv "RABBITMQ_USER") *rabbit-user*)
      *rabbit-password*
      (or (environment-secret "RABBITMQ_PASSWORD" "RABBITMQ_PASSWORD_FILE")
          *rabbit-password*)
      *http-api-address*
      (or (uiop:getenv "HTTP_API_LISTEN_ADDRESS") *http-api-address*)
      *http-cors-allowed-origins*
      (or (split-comma-setting (uiop:getenv "STAR_AUTH_ALLOWED_ORIGINS"))
          *http-cors-allowed-origins*)
      *auth-mode*
      (or (uiop:getenv "STAR_AUTH_MODE") *auth-mode*)
      *auth-pepper*
      (or (environment-secret "STAR_AUTH_PEPPER" "STAR_AUTH_PEPPER_FILE")
          *auth-pepper*)
      *auth-bootstrap-secret*
      (or (environment-secret
           "STAR_AUTH_BOOTSTRAP_SECRET"
           "STAR_AUTH_BOOTSTRAP_SECRET_FILE")
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

;; The standalone SBCL image is built before deployment environment variables
;; exist. Refresh target-lease settings from the live process environment here,
;; just like the server's CouchDB/Rabbit/auth settings above, so dumped build-time
;; values never become runtime configuration.
(setf star.authorization::*target-lease-valkey-host*
      (or (uiop:getenv "VALKEY_HOST")
          star.authorization::*target-lease-valkey-host*)
      star.authorization::*target-lease-valkey-port*
      (environment-integer
       "VALKEY_PORT"
       star.authorization::*target-lease-valkey-port*)
      star.authorization::*target-lease-valkey-password-file*
      (uiop:getenv "VALKEY_PASSWORD_FILE")
      star.authorization::*target-lease-valkey-tls-p*
      (not (null
            (environment-boolean
             "VALKEY_TLS"
             star.authorization::*target-lease-valkey-tls-p*)))
      star.authorization::*target-lease-valkey-ca-file*
      (uiop:getenv "VALKEY_CA_FILE")
      star.authorization::*target-lease-valkey-pool-size*
      (environment-integer
       "STAR_TARGET_LEASE_POOL_SIZE"
       star.authorization::*target-lease-valkey-pool-size*)
      star.authorization::*target-lease-valkey-pool-wait-timeout-ms*
      (environment-integer
       "STAR_TARGET_LEASE_POOL_WAIT_TIMEOUT_MS"
       star.authorization::*target-lease-valkey-pool-wait-timeout-ms*)
      star.authorization::*target-lease-valkey-operation-timeout-ms*
      (environment-integer
       "STAR_TARGET_LEASE_OPERATION_TIMEOUT_MS"
       star.authorization::*target-lease-valkey-operation-timeout-ms*)
      star.authorization::*target-lease-valkey-reconnect-attempts*
      (environment-integer
       "STAR_TARGET_LEASE_RECONNECT_ATTEMPTS"
       star.authorization::*target-lease-valkey-reconnect-attempts*)
      star.authorization::*target-lease-valkey-reconnect-backoff-ms*
      (environment-integer
       "STAR_TARGET_LEASE_RECONNECT_BACKOFF_MS"
       star.authorization::*target-lease-valkey-reconnect-backoff-ms*)
      star.authorization::*target-lease-idempotency-ttl-ms*
      (environment-integer
       "STAR_TARGET_LEASE_IDEMPOTENCY_TTL_MS"
       star.authorization::*target-lease-idempotency-ttl-ms*)
      star.authorization::*target-lease-default-ttl-ms*
      (environment-integer
       "STAR_TARGET_LEASE_DEFAULT_TTL_MS"
       star.authorization::*target-lease-default-ttl-ms*)
      star.authorization::*target-lease-maximum-lifetime-ms*
      (environment-integer
       "STAR_TARGET_LEASE_MAXIMUM_LIFETIME_MS"
       star.authorization::*target-lease-maximum-lifetime-ms*)
      star.authorization::*target-lease-service-instance-id*
      (uiop:getenv "STAR_TARGET_LEASE_SERVICE_INSTANCE_ID"))
