(in-package :star.auth)

(defparameter +api-key-prefix+ "star_sk_v1_")
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
     (format stream "~a" (authentication-error-message condition)))))

(define-condition credential-lifecycle-error (error)
  ((code
    :initarg :code
    :reader credential-lifecycle-error-code)
   (message
    :initarg :message
    :reader credential-lifecycle-error-message))
  (:report
   (lambda (condition stream)
     (format stream "~a" (credential-lifecycle-error-message condition)))))

(defstruct (request-principal
            (:constructor %make-request-principal)
            (:copier nil))
  (id nil :read-only t)
  (type nil :read-only t)
  (scopes nil :read-only t)
  (credential-id nil :read-only t))

(defstruct (request-security-context
            (:constructor %make-request-security-context)
            (:copier nil))
  (principal nil :read-only t)
  (correlation-id nil :read-only t)
  (deadline nil :read-only t)
  (authenticated-at nil :read-only t))

(defstruct (service-call-context
            (:constructor %make-service-call-context)
            (:copier nil))
  (principal-id nil :read-only t)
  (principal-type nil :read-only t)
  (credential-id nil :read-only t)
  (scopes nil :read-only t)
  (correlation-id nil :read-only t)
  (deadline nil :read-only t))

(defstruct api-key-record
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

(defclass credential-store () ())

(defgeneric credential-store-get (store credential-id))
(defgeneric credential-store-put (store record))
(defgeneric credential-store-update (store record))
(defgeneric credential-store-list (store))
(defgeneric credential-store-count (store))

(defvar *credential-store* nil)
(defvar *request-security-context* nil)
(defvar *auth-clock* #'get-universal-time)

(defun auth-now ()
  (funcall *auth-clock*))

(defun signal-authentication-failure ()
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

(defvar *verifier-compare-function* #'constant-time-octets=)

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
  "Return credential id and decoded secret. Signal one uniform failure otherwise."
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
  (authenticate-api-key
   (bearer-token authorization-header)
   correlation-id
   deadline
   :store store))

(defun current-request-principal ()
  (and *request-security-context*
       (request-security-context-principal *request-security-context*)))

(defun current-principal-id ()
  (let ((principal (current-request-principal)))
    (and principal (request-principal-id principal))))

(defun current-service-call-context ()
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
  (and principal
       (or (member "admin" (request-principal-scopes principal) :test #'string=)
           (member scope (request-principal-scopes principal) :test #'string=))))

(defun administrator-principal-p (&optional (principal (current-request-principal)))
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
  (let ((record (and store
                     (credential-store-get store credential-id))))
    (unless record
      (signal-lifecycle-error
       "credential_not_found"
       "Credential was not found"))
    (setf (api-key-record-status record) :revoked
          (api-key-record-revoked-at record) (auth-now))
    (credential-store-update store record)))

(defun disable-api-key (credential-id &key (store *credential-store*))
  (let ((record (and store
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
  (mapcar #'api-key-metadata-json
          (credential-store-list store)))
