(in-package :star)

(defparameter *document-storage-default-tier*
  (or (uiop:getenv "STAR_STORAGE_DEFAULT_TIER") "hot")
  "Default tier for newly persisted tenant documents.

Supported tiers are hot, warm, cold and archive.

- env: =STAR_STORAGE_DEFAULT_TIER=
- default: =hot=")

(defparameter *document-storage-tier-backends*
  (or (uiop:getenv "STAR_STORAGE_TIER_BACKENDS")
      "hot=couchdb,warm=s3,cold=s3,archive=s3")
  "Comma-separated tier-to-backend map.

Backend names are resolved by the document-storage actor. The built-in S3
backend is named =s3= and CouchDB is the inline metadata/index plane.

- env: =STAR_STORAGE_TIER_BACKENDS=
- default: =hot=couchdb,warm=s3,cold=s3,archive=s3=")

(defparameter *file-artifact-storage-tier*
  (or (uiop:getenv "STAR_FILE_STORAGE_TIER") "warm")
  "Logical storage tier used for raw file artifact bytes.

The tier must resolve to an external backend; the default warm tier resolves to
S3 under the default storage map.

- env: =STAR_FILE_STORAGE_TIER=
- default: =warm=")

(defparameter *file-artifact-max-bytes*
  (environment-integer "STAR_FILE_MAX_BYTES" 16777216)
  "Maximum decoded size of one inline actor file artifact.

- env: =STAR_FILE_MAX_BYTES=
- default: 16777216 (16 MiB)")

(defparameter *document-storage-tenant-tiers*
  (or (uiop:getenv "STAR_STORAGE_TENANT_TIERS") "")
  "Comma-separated tenant-to-default-tier map, for example
=default=hot,archive-tenant=cold=.

The map is server-owned; lifecycle callers select a tier but never supply an
S3 bucket, endpoint or credentials.

- env: =STAR_STORAGE_TENANT_TIERS=
- default: empty")

(defparameter *s3-region*
  (or (uiop:getenv "STAR_S3_REGION")
      (uiop:getenv "AWS_REGION")
      (uiop:getenv "AWS_DEFAULT_REGION")
      "us-east-1")
  "AWS/S3 signing region.")

(defparameter *s3-endpoint*
  (or (uiop:getenv "STAR_S3_ENDPOINT")
      (format nil "https://s3.~a.amazonaws.com" *s3-region*))
  "S3-compatible endpoint. Path-style bucket addressing is used so MinIO and
other S3-compatible services work without a separate adapter.")

(defparameter *s3-bucket*
  (uiop:getenv "STAR_S3_BUCKET")
  "S3 bucket used by the document-storage backend. Unset disables S3 backend
registration until an operator configures it.")

(defparameter *s3-access-key-id*
  (or (uiop:getenv "STAR_S3_ACCESS_KEY_ID")
      (uiop:getenv "AWS_ACCESS_KEY_ID"))
  "S3 access-key id. Never logged.")

(defparameter *s3-secret-access-key*
  (or (environment-secret
       "STAR_S3_SECRET_ACCESS_KEY"
       "STAR_S3_SECRET_ACCESS_KEY_FILE")
      (environment-secret
       "AWS_SECRET_ACCESS_KEY"
       "AWS_SECRET_ACCESS_KEY_FILE"))
  "S3 secret access key. Prefer the *_FILE variants in deployed systems.")

(defparameter *s3-session-token*
  (or (environment-secret
       "STAR_S3_SESSION_TOKEN"
       "STAR_S3_SESSION_TOKEN_FILE")
      (environment-secret
       "AWS_SESSION_TOKEN"
       "AWS_SESSION_TOKEN_FILE"))
  "Optional STS session token. Never logged.")

(defparameter *s3-connect-timeout-seconds*
  (environment-integer "STAR_S3_CONNECT_TIMEOUT_SECONDS" 5)
  "S3 TCP/TLS connection timeout.")

(defparameter *s3-read-timeout-seconds*
  (environment-integer "STAR_S3_READ_TIMEOUT_SECONDS" 30)
  "S3 response timeout.")
