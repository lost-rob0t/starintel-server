(in-package :star.auth)

(defparameter +api-key-prefix+ "star_sk_v1_"
  "Prefix of every presented API key string.")
(defparameter +api-key-version+ "v1")
(defparameter +credential-kind+ "api-key")

(define-condition authentication-error (error)
  ((code
    :initarg :code
    :initform "invalid_credential"
    :reader authentication-error-code)
   (message
    :initarg :message
    :initform "Authentication failed"
    :reader authentication-error-message))
  (:report
   (lambda (condition stream)
     (format stream "~a" (authentication-error-message condition))))
  (:documentation "Uniform failure signalled for every rejected credential."))

;; Accessor documentation for authentication-error
(setf (documentation 'AUTHENTICATION-ERROR-CODE 'function)
"The =code= slot of =authentication-error=.")
(setf (documentation 'AUTHENTICATION-ERROR-MESSAGE 'function)
"The =message= slot of =authentication-error=.")



(define-condition credential-lifecycle-error (error)
  ((code
    :initarg :code
    :reader credential-lifecycle-error-code)
   (message
    :initarg :message
    :reader credential-lifecycle-error-message))
  (:report
   (lambda (condition stream)
     (format stream "~a" (credential-lifecycle-error-message condition))))
  (:documentation "Signalled for invalid API key lifecycle operations."))

;; Accessor documentation for credential-lifecycle-error
(setf (documentation 'CREDENTIAL-LIFECYCLE-ERROR-CODE 'function)
"The =code= slot of =credential-lifecycle-error=.")
(setf (documentation 'CREDENTIAL-LIFECYCLE-ERROR-MESSAGE 'function)
"The =message= slot of =credential-lifecycle-error=.")



(defstruct (request-principal
            (:constructor %make-request-principal)
            (:copier nil))
  "Who is calling: id, type, scopes and credential id."
  (id nil :read-only t)
  (type nil :read-only t)
  (scopes nil :read-only t)
  (credential-id nil :read-only t))

;; Accessor documentation for request-principal
(setf (documentation 'REQUEST-PRINCIPAL-CREDENTIAL-ID 'function)
"The =credential-id= slot of =request-principal=.")
(setf (documentation 'REQUEST-PRINCIPAL-ID 'function)
"The =id= slot of =request-principal=.")
(setf (documentation 'REQUEST-PRINCIPAL-SCOPES 'function)
"The =scopes= slot of =request-principal=.")
(setf (documentation 'REQUEST-PRINCIPAL-TYPE 'function)
"The =type= slot of =request-principal=.")



(defstruct (request-security-context
            (:constructor %make-request-security-context)
            (:copier nil))
  "Authentication facts bound to one request."
  (principal nil :read-only t)
  (correlation-id nil :read-only t)
  (deadline nil :read-only t)
  (authenticated-at nil :read-only t))

;; Accessor documentation for request-security-context
(setf (documentation 'REQUEST-SECURITY-CONTEXT-AUTHENTICATED-AT 'function)
"The =authenticated-at= slot of =request-security-context=.")
(setf (documentation 'REQUEST-SECURITY-CONTEXT-CORRELATION-ID 'function)
"The =correlation-id= slot of =request-security-context=.")
(setf (documentation 'REQUEST-SECURITY-CONTEXT-DEADLINE 'function)
"The =deadline= slot of =request-security-context=.")
(setf (documentation 'REQUEST-SECURITY-CONTEXT-PRINCIPAL 'function)
"The =principal= slot of =request-security-context=.")



(defstruct (service-call-context
            (:constructor %make-service-call-context)
            (:copier nil))
  "The principal context propagated across service boundaries."
  (principal-id nil :read-only t)
  (principal-type nil :read-only t)
  (credential-id nil :read-only t)
  (scopes nil :read-only t)
  (correlation-id nil :read-only t)
  (deadline nil :read-only t))

;; Accessor documentation for service-call-context
(setf (documentation 'SERVICE-CALL-CONTEXT-CORRELATION-ID 'function)
"The =correlation-id= slot of =service-call-context=.")
(setf (documentation 'SERVICE-CALL-CONTEXT-CREDENTIAL-ID 'function)
"The =credential-id= slot of =service-call-context=.")
(setf (documentation 'SERVICE-CALL-CONTEXT-DEADLINE 'function)
"The =deadline= slot of =service-call-context=.")
(setf (documentation 'SERVICE-CALL-CONTEXT-PRINCIPAL-ID 'function)
"The =principal-id= slot of =service-call-context=.")
(setf (documentation 'SERVICE-CALL-CONTEXT-PRINCIPAL-TYPE 'function)
"The =principal-type= slot of =service-call-context=.")
(setf (documentation 'SERVICE-CALL-CONTEXT-SCOPES 'function)
"The =scopes= slot of =service-call-context=.")



(defstruct api-key-record
  "Persisted record of one API key credential."
  id
  owner
  principal-type
  scopes
  status
  salt
  verifier
  created-at
  expires-at
  disabled-at
  revoked-at
  rotation-parent-id
  superseded-by
  overlap-expires-at
  revision)

;; Accessor documentation for api-key-record
(setf (documentation 'API-KEY-RECORD-CREATED-AT 'function)
"The =created-at= slot of =api-key-record=.")
(setf (documentation 'API-KEY-RECORD-DISABLED-AT 'function)
"The =disabled-at= slot of =api-key-record=.")
(setf (documentation 'API-KEY-RECORD-EXPIRES-AT 'function)
"The =expires-at= slot of =api-key-record=.")
(setf (documentation 'API-KEY-RECORD-ID 'function)
"The =id= slot of =api-key-record=.")
(setf (documentation 'API-KEY-RECORD-OVERLAP-EXPIRES-AT 'function)
"The =overlap-expires-at= slot of =api-key-record=.")
(setf (documentation 'API-KEY-RECORD-OWNER 'function)
"The =owner= slot of =api-key-record=.")
(setf (documentation 'API-KEY-RECORD-PRINCIPAL-TYPE 'function)
"The =principal-type= slot of =api-key-record=.")
(setf (documentation 'API-KEY-RECORD-REVISION 'function)
"Optimistic concurrency revision of the record.")
(setf (documentation 'API-KEY-RECORD-REVOKED-AT 'function)
"The =revoked-at= slot of =api-key-record=.")
(setf (documentation 'API-KEY-RECORD-ROTATION-PARENT-ID 'function)
"The =rotation-parent-id= slot of =api-key-record=.")
(setf (documentation 'API-KEY-RECORD-SALT 'function)
"The =salt= slot of =api-key-record=.")
(setf (documentation 'API-KEY-RECORD-SCOPES 'function)
"The =scopes= slot of =api-key-record=.")
(setf (documentation 'API-KEY-RECORD-STATUS 'function)
"The =status= slot of =api-key-record=.")
(setf (documentation 'API-KEY-RECORD-SUPERSEDED-BY 'function)
"The =superseded-by= slot of =api-key-record=.")
(setf (documentation 'API-KEY-RECORD-VERIFIER 'function)
"The =verifier= slot of =api-key-record=.")



(defclass credential-store () ()
  (:documentation "Storage protocol for API key credentials."))

;; Accessor documentation for credential-store
(setf (documentation 'CREDENTIAL-STORE-COUNT 'function)
"Number of credentials in the store.")
(setf (documentation 'CREDENTIAL-STORE-GET 'function)
"Fetch the credential record for CREDENTIAL-ID, or nil.")
(setf (documentation 'CREDENTIAL-STORE-LIST 'function)
"List every credential record in the store.")
(setf (documentation 'CREDENTIAL-STORE-PUT 'function)
"Insert a new credential record.")
(setf (documentation 'CREDENTIAL-STORE-UPDATE 'function)
"Update an existing credential record.")



(defgeneric credential-store-get (store credential-id)
  (:documentation "Fetch the credential record for CREDENTIAL-ID, or nil."))
(defgeneric credential-store-put (store record)
  (:documentation "Insert a new credential record."))
(defgeneric credential-store-update (store record)
  (:documentation "Update an existing credential record."))
(defgeneric credential-store-list (store)
  (:documentation "List every credential record in the store."))
(defgeneric credential-store-count (store)
  (:documentation "Number of credentials in the store."))

(defvar *credential-store* nil
  "Global credential store used when no explicit store is passed.")
(defvar *request-security-context* nil
  "Dynamic binding of the current request authentication context.")
(defvar *auth-clock* #'get-universal-time
  "Function returning the current universal time; swapped in tests.")

(defun auth-now ()
  "Current universal time from =*auth-clock*=."
  (funcall *auth-clock*))

(defun signal-authentication-failure ()
  "Signal the uniform authentication error with CODE and MESSAGE."
  (error 'authentication-error
         :code "invalid_credential"
         :message "Authentication failed"))

(defun signal-lifecycle-error (code message)
  (error 'credential-lifecycle-error
         :code code
         :message message))

(defun string-octets (value)
  (babel:string-to-octets value :encoding :utf-8))

(defun concatenate-octet-vectors (&rest vectors)
  (let* ((length (reduce #'+ vectors :key #'length :initial-value 0))
         (result (make-array length :element-type '(unsigned-byte 8)))
         (offset 0))
    (dolist (vector vectors result)
      (replace result vector :start1 offset)
      (incf offset (length vector)))))

(defun constant-time-octets= (left right)
  "Compare octet vectors without data-dependent early return.
Verifier inputs are fixed-length SHA-256 values at the authentication boundary."
  (let* ((left-length (length left))
         (right-length (length right))
         (maximum (max left-length right-length))
         (difference (logxor left-length right-length)))
    (dotimes (index maximum (zerop difference))
      (setf difference
            (logior difference
                    (logxor (if (< index left-length)
                                (aref left index)
                                0)
                            (if (< index right-length)
                                (aref right index)
                                0)))))))

(defvar *verifier-compare-function* #'constant-time-octets=
  "Constant-time comparison function; injection point for tests.")

(defun sha256 (&rest vectors)
  (ironclad:digest-sequence
   :sha256
   (apply #'concatenate-octet-vectors vectors)))

(defun random-hex (octet-count)
  (ironclad:byte-array-to-hex-string
   (ironclad:random-data octet-count)))

(defun decode-hex (value)
  (ironclad:hex-string-to-byte-array value))

(defun derive-verifier (secret-octets salt-hex pepper)
  (sha256 (string-octets pepper)
          (decode-hex salt-hex)
          secret-octets))

(defun verifier-hex (secret-octets salt-hex pepper)
  (ironclad:byte-array-to-hex-string
   (derive-verifier secret-octets salt-hex pepper)))

(defun fixed-secret-digest (value)
  (sha256 (string-octets (or value ""))))

(defun constant-time-secret= (left right)
  "Constant-time string comparison for presented secrets."
  (funcall *verifier-compare-function*
           (fixed-secret-digest left)
           (fixed-secret-digest right)))

(defun split-on-character (string character)
  (loop with start = 0
        for position = (position character string :start start)
        collect (subseq string start position)
        while position
        do (setf start (1+ position))))

(defun valid-hex-string-p (value expected-length)
  (and (stringp value)
       (= (length value) expected-length)
       (every (lambda (character)
                (not (null (digit-char-p character 16))))
              value)))

(defun parse-api-key (api-key)
  "Parse a presented API key string into (VALUES CREDENTIAL-ID SECRET-OCTETS).

Key format: =star_sk_v1_<credential-id>_<hex-secret>=.  Any malformed
input signals the same uniform =authentication-error=, so timing and
error shapes do not leak which part failed."
  (handler-case
      (let ((parts (and (stringp api-key)
                        (split-on-character api-key #\_))))
        (unless (and (= (length parts) 5)
                     (string= (first parts) "star")
                     (string= (second parts) "sk")
                     (string= (third parts) +api-key-version+)
                     (plusp (length (fourth parts)))
                     (valid-hex-string-p
                      (fifth parts)
                      (* 2 star:*auth-key-secret-bytes*)))
          (signal-authentication-failure))
        (values (fourth parts)
                (decode-hex (fifth parts))))
    (authentication-error (condition)
      (error condition))
    (error ()
      (signal-authentication-failure))))

(defun bearer-token (authorization-header)
  "Extract the token from an =Authorization: Bearer= header value."
  (unless (and (stringp authorization-header)
               (> (length authorization-header) 7)
               (string-equal "Bearer " authorization-header :end2 7))
    (signal-authentication-failure))
  (let ((token (subseq authorization-header 7)))
    (when (or (zerop (length token))
              (find #\Space token)
              (find #\Tab token))
      (signal-authentication-failure))
    token))

(defun normalize-principal-type (value)
  (string-downcase
   (etypecase value
     (string value)
     (symbol (symbol-name value)))))

(defun normalize-scopes (scopes)
  (unless (and (listp scopes)
               (every (lambda (scope)
                        (and (stringp scope)
                             (plusp (length scope))))
                      scopes))
    (signal-lifecycle-error
     "invalid_scopes"
     "Scopes must be a list of non-empty strings"))
  (remove-duplicates (copy-list scopes) :test #'string=))

(defun active-record-p (record now)
  (and record
       (eq :active (api-key-record-status record))
       (or (null (api-key-record-expires-at record))
           (> (api-key-record-expires-at record) now))
       (or (null (api-key-record-superseded-by record))
           (and (api-key-record-overlap-expires-at record)
                (> (api-key-record-overlap-expires-at record) now)))))

(defun record-principal (record)
  (%make-request-principal
   :id (api-key-record-owner record)
   :type (api-key-record-principal-type record)
   :scopes (copy-list (api-key-record-scopes record))
   :credential-id (api-key-record-id record)))

(defun authenticate-api-key (api-key correlation-id deadline
                              &key (store *credential-store*))
  "Authenticate a presented API key; return a security context on success.

- API-KEY :: the raw =star_sk_v1_...= string
- CORRELATION-ID :: request trace id carried into the context
- DEADLINE :: request deadline (universal time or nil)
- STORE :: credential store; defaults to =*credential-store*=

On any failure signals a uniform =authentication-error= (never a plain
error), so callers can map it to 401 without leaking detail."
  (unless store
    (signal-authentication-failure))
  (multiple-value-bind (credential-id secret-octets)
      (parse-api-key api-key)
    (let* ((record (credential-store-get store credential-id))
           (now (auth-now)))
      (unless (active-record-p record now)
        (signal-authentication-failure))
      (let ((expected (decode-hex (api-key-record-verifier record)))
            (actual (derive-verifier
                     secret-octets
                     (api-key-record-salt record)
                     star:*auth-pepper*)))
        (unless (funcall *verifier-compare-function* expected actual)
          (signal-authentication-failure)))
      (%make-request-security-context
       :principal (record-principal record)
       :correlation-id correlation-id
       :deadline deadline
       :authenticated-at now))))

(defun authenticate-authorization-header (authorization-header correlation-id deadline
                                          &key (store *credential-store*))
  "Authenticate an =Authorization: Bearer ...= header value.

Extracts the token with =bearer-token= and delegates to
=authenticate-api-key=.  Returns the security context or signals
=authentication-error=."
  (authenticate-api-key
   (bearer-token authorization-header)
   correlation-id
   deadline
   :store store))

(defun current-request-principal ()
  "The principal bound to this request, or nil."
  (and *request-security-context*
       (request-security-context-principal *request-security-context*)))

(defun current-principal-id ()
  "The id of the principal bound to this request, or nil."
  (let ((principal (current-request-principal)))
    (and principal (request-principal-id principal))))

(defun current-service-call-context ()
  "Serializable view of the current request context for service calls."
  (let ((context *request-security-context*))
    (when context
      (let ((principal (request-security-context-principal context)))
        (%make-service-call-context
         :principal-id (request-principal-id principal)
         :principal-type (request-principal-type principal)
         :credential-id (request-principal-credential-id principal)
         :scopes (copy-list (request-principal-scopes principal))
         :correlation-id (request-security-context-correlation-id context)
         :deadline (request-security-context-deadline context))))))

(defun scope-granted-p (scope &optional (principal (current-request-principal)))
  "True when the current principal holds SCOPE (or admin)."
  (and principal
       (or (member "admin" (request-principal-scopes principal) :test #'string=)
           (member scope (request-principal-scopes principal) :test #'string=))))

(defun administrator-principal-p (&optional (principal (current-request-principal)))
  "True when the principal is an administrator type."
  (and principal
       (or (string= "administrator" (request-principal-type principal))
           (scope-granted-p "admin" principal))))

(defun make-api-key-material (owner principal-type scopes
                              &key expires-at rotation-parent-id)
  (let* ((credential-id (cms-ulid:ulid))
         (secret-hex (random-hex star:*auth-key-secret-bytes*))
         (secret-octets (decode-hex secret-hex))
         (salt-hex (random-hex star:*auth-salt-bytes*))
         (record
           (make-api-key-record
            :id credential-id
            :owner owner
            :principal-type (normalize-principal-type principal-type)
            :scopes (normalize-scopes scopes)
            :status :active
            :salt salt-hex
            :verifier (verifier-hex secret-octets salt-hex star:*auth-pepper*)
            :created-at (auth-now)
            :expires-at expires-at
            :rotation-parent-id rotation-parent-id)))
    (values record
            (format nil "~a~a_~a"
                    +api-key-prefix+
                    credential-id
                    secret-hex))))

(defun validate-expiry (expires-in-seconds)
  (cond
    ((null expires-in-seconds) nil)
    ((and (integerp expires-in-seconds)
          (plusp expires-in-seconds))
     (+ (auth-now) expires-in-seconds))
    (t
     (signal-lifecycle-error
      "invalid_expiry"
      "Expiration must be a positive number of seconds"))))

(defun create-api-key (owner principal-type scopes
                       &key expires-in-seconds rotation-parent-id
                         (store *credential-store*))
  "Mint a new API key for OWNER.

Returns (VALUES RECORD RAW-KEY); the raw key is the full
=star_sk_v1_...= string, shown once and never stored verbatim (only a
peppered verifier is persisted).  Signals =credential-lifecycle-error=
for invalid owner, expiry or missing store."
  (unless (and (stringp owner) (plusp (length owner)))
    (signal-lifecycle-error
     "invalid_owner"
     "Credential owner must be a non-empty string"))
  (unless store
    (signal-lifecycle-error
     "auth_store_unavailable"
     "Credential store is unavailable"))
  (multiple-value-bind (record raw-key)
      (make-api-key-material
       owner
       principal-type
       scopes
       :expires-at (validate-expiry expires-in-seconds)
       :rotation-parent-id rotation-parent-id)
    (credential-store-put store record)
    (values record raw-key)))

(defun bootstrap-api-key (presented-secret owner
                          &key (store *credential-store*))
  "Create the first administrator key, guarded by the bootstrap secret.

- PRESENTED-SECRET must constant-time-match =*auth-bootstrap-secret*=
- fails with =bootstrap_complete= once any credential exists

Returns (VALUES RECORD RAW-KEY) like =create-api-key=."
  (unless store
    (signal-lifecycle-error
     "auth_store_unavailable"
     "Credential store is unavailable"))
  (unless (and star:*auth-bootstrap-secret*
               (constant-time-secret=
                presented-secret
                star:*auth-bootstrap-secret*))
    (signal-lifecycle-error
     "bootstrap_denied"
     "Bootstrap denied"))
  (unless (zerop (credential-store-count store))
    (signal-lifecycle-error
     "bootstrap_complete"
     "Bootstrap has already been completed"))
  (create-api-key owner
                  "administrator"
                  (list "admin")
                  :store store))

(defun validate-overlap-seconds (overlap-seconds)
  (unless (and (integerp overlap-seconds)
               (<= 0 overlap-seconds star:*auth-rotation-overlap-max-seconds*))
    (signal-lifecycle-error
     "invalid_overlap"
     "Rotation overlap is outside the configured bound"))
  overlap-seconds)

(defun rotate-api-key (credential-id overlap-seconds
                       &key (store *credential-store*))
  "Rotate CREDENTIAL-ID: mint a replacement and phase out the old key.

The old key stays valid for OVERLAP-SECONDS (bounded by
=*auth-rotation-overlap-max-seconds*=) then is superseded.  If the
store update fails the replacement is revoked so no untracked key
survives.  Returns (VALUES REPLACEMENT-RECORD RAW-KEY)."
  (let* ((overlap (validate-overlap-seconds overlap-seconds))
         (record (and store
                      (credential-store-get store credential-id))))
    (unless record
      (signal-lifecycle-error
       "credential_not_found"
       "Credential was not found"))
    (unless (eq :active (api-key-record-status record))
      (signal-lifecycle-error
       "credential_not_active"
       "Credential is not active"))
    (multiple-value-bind (replacement raw-key)
        (make-api-key-material
         (api-key-record-owner record)
         (api-key-record-principal-type record)
         (api-key-record-scopes record)
         :expires-at (api-key-record-expires-at record)
         :rotation-parent-id credential-id)
      (credential-store-put store replacement)
      (setf (api-key-record-superseded-by record)
            (api-key-record-id replacement)
            (api-key-record-overlap-expires-at record)
            (+ (auth-now) overlap))
      (handler-case
          (credential-store-update store record)
        (error (condition)
          (setf (api-key-record-status replacement) :revoked
                (api-key-record-revoked-at replacement) (auth-now))
          (ignore-errors
            (credential-store-update store replacement))
          (error condition)))
      (values replacement raw-key))))

(defun revoke-api-key (credential-id &key (store *credential-store*))
  "Immediately revoke the credential CREDENTIAL-ID.

Revoked keys never authenticate again; contrast =disable-api-key=,
which permits temporary suspension."  (let ((record (and store
                     (credential-store-get store credential-id))))
    (unless record
      (signal-lifecycle-error
       "credential_not_found"
       "Credential was not found"))
    (setf (api-key-record-status record) :revoked
          (api-key-record-revoked-at record) (auth-now))
    (credential-store-update store record)))

(defun disable-api-key (credential-id &key (store *credential-store*))
  "Temporarily suspend the credential CREDENTIAL-ID.

A disabled key can be re-enabled by returning its status to
=active= via the store; use =revoke-api-key= for permanent removal."  (let ((record (and store
                     (credential-store-get store credential-id))))
    (unless record
      (signal-lifecycle-error
       "credential_not_found"
       "Credential was not found"))
    (setf (api-key-record-status record) :disabled
          (api-key-record-disabled-at record) (auth-now))
    (credential-store-update store record)))

(defun nullable-json-value (value)
  (or value :null))

(defun api-key-metadata-json (record)
  "Serialize API key metadata (no secrets) to JSON."
  (jsown:new-js
    ("credential_id" (api-key-record-id record))
    ("owner" (api-key-record-owner record))
    ("principal_type" (api-key-record-principal-type record))
    ("scopes" (copy-list (api-key-record-scopes record)))
    ("status" (string-downcase
               (symbol-name (api-key-record-status record))))
    ("created_at" (api-key-record-created-at record))
    ("expires_at" (nullable-json-value
                   (api-key-record-expires-at record)))
    ("disabled_at" (nullable-json-value
                    (api-key-record-disabled-at record)))
    ("revoked_at" (nullable-json-value
                   (api-key-record-revoked-at record)))
    ("rotation_parent_id" (nullable-json-value
                           (api-key-record-rotation-parent-id record)))
    ("superseded_by" (nullable-json-value
                      (api-key-record-superseded-by record)))
    ("overlap_expires_at" (nullable-json-value
                           (api-key-record-overlap-expires-at record)))))

(defun list-api-key-metadata (&key (store *credential-store*))
  "Metadata (no secrets) for every stored API key."
  (mapcar #'api-key-metadata-json
          (credential-store-list store)))
