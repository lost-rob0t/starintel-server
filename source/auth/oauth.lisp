(in-package :star.auth)

(defparameter +oauth-client-kind+ "oauth-client")
(defparameter +oauth-code-kind+ "oauth-code")
(defparameter +oauth-access-token-kind+ "oauth-access-token")
(defparameter +oauth-code-prefix+ "star_oc_v1_")
(defparameter +oauth-access-token-prefix+ "star_at_v1_")

(define-condition oauth-error (error)
  ((code
    :initarg :code
    :reader oauth-error-code)
   (message
    :initarg :message
    :reader oauth-error-message))
  (:report
   (lambda (condition stream)
     (format stream "~a" (oauth-error-message condition)))))

(defun signal-oauth-error (code message)
  (error 'oauth-error :code code :message message))

(defstruct oauth-client-record
  id
  status
  allowed-scopes
  redirect-uris
  secret-salt
  secret-verifier
  created-at
  revision)

(defstruct oauth-authorization-code-record
  id
  status
  client-id
  owner
  scopes
  redirect-uri
  code-challenge
  code-challenge-method
  salt
  verifier
  created-at
  expires-at
  consumed-at
  revision)

(defstruct oauth-access-token-record
  id
  status
  client-id
  owner
  principal-type
  scopes
  salt
  verifier
  created-at
  expires-at
  revoked-at
  revision)

(defgeneric oauth-client-store-get (store client-id))
(defgeneric oauth-client-store-put (store record))
(defgeneric oauth-client-store-update (store record))
(defgeneric oauth-code-store-get (store code-id))
(defgeneric oauth-code-store-put (store record))
(defgeneric oauth-code-store-update (store record))
(defgeneric oauth-access-token-store-get (store token-id))
(defgeneric oauth-access-token-store-put (store record))
(defgeneric oauth-access-token-store-update (store record))

(defun copy-oauth-client-or-nil (record)
  (and record (copy-oauth-client-record record)))

(defun copy-oauth-code-or-nil (record)
  (and record (copy-oauth-authorization-code-record record)))

(defun copy-oauth-token-or-nil (record)
  (and record (copy-oauth-access-token-record record)))

(defmethod oauth-client-store-get ((store memory-credential-store) client-id)
  (bt:with-lock-held ((memory-store-lock store))
    (copy-oauth-client-or-nil
     (gethash client-id (memory-store-oauth-clients store)))))

(defmethod oauth-client-store-put ((store memory-credential-store) record)
  (bt:with-lock-held ((memory-store-lock store))
    (when (gethash (oauth-client-record-id record)
                   (memory-store-oauth-clients store))
      (signal-oauth-error "invalid_client" "OAuth client already exists"))
    (setf (gethash (oauth-client-record-id record)
                   (memory-store-oauth-clients store))
          (copy-oauth-client-record record)))
  (copy-oauth-client-record record))

(defmethod oauth-client-store-update ((store memory-credential-store) record)
  (bt:with-lock-held ((memory-store-lock store))
    (unless (gethash (oauth-client-record-id record)
                     (memory-store-oauth-clients store))
      (signal-oauth-error "invalid_client" "OAuth client was not found"))
    (setf (gethash (oauth-client-record-id record)
                   (memory-store-oauth-clients store))
          (copy-oauth-client-record record)))
  (copy-oauth-client-record record))

(defmethod oauth-code-store-get ((store memory-credential-store) code-id)
  (bt:with-lock-held ((memory-store-lock store))
    (copy-oauth-code-or-nil
     (gethash code-id (memory-store-oauth-codes store)))))

(defmethod oauth-code-store-put ((store memory-credential-store) record)
  (bt:with-lock-held ((memory-store-lock store))
    (when (gethash (oauth-authorization-code-record-id record)
                   (memory-store-oauth-codes store))
      (signal-oauth-error "server_error" "OAuth code identifier conflicted"))
    (setf (gethash (oauth-authorization-code-record-id record)
                   (memory-store-oauth-codes store))
          (copy-oauth-authorization-code-record record)))
  (copy-oauth-authorization-code-record record))

(defmethod oauth-code-store-update ((store memory-credential-store) record)
  (bt:with-lock-held ((memory-store-lock store))
    (let* ((id (oauth-authorization-code-record-id record))
           (current (gethash id (memory-store-oauth-codes store))))
      (unless current
        (signal-oauth-error "invalid_grant" "Authorization code is invalid"))
      (when (oauth-authorization-code-record-consumed-at current)
        (signal-oauth-error "invalid_grant" "Authorization code is invalid"))
      (setf (gethash id (memory-store-oauth-codes store))
            (copy-oauth-authorization-code-record record))))
  (copy-oauth-authorization-code-record record))

(defmethod oauth-access-token-store-get ((store memory-credential-store) token-id)
  (bt:with-lock-held ((memory-store-lock store))
    (copy-oauth-token-or-nil
     (gethash token-id (memory-store-oauth-access-tokens store)))))

(defmethod oauth-access-token-store-put ((store memory-credential-store) record)
  (bt:with-lock-held ((memory-store-lock store))
    (when (gethash (oauth-access-token-record-id record)
                   (memory-store-oauth-access-tokens store))
      (signal-oauth-error "server_error" "OAuth token identifier conflicted"))
    (setf (gethash (oauth-access-token-record-id record)
                   (memory-store-oauth-access-tokens store))
          (copy-oauth-access-token-record record)))
  (copy-oauth-access-token-record record))

(defmethod oauth-access-token-store-update ((store memory-credential-store) record)
  (bt:with-lock-held ((memory-store-lock store))
    (unless (gethash (oauth-access-token-record-id record)
                     (memory-store-oauth-access-tokens store))
      (signal-oauth-error "invalid_token" "OAuth access token was not found"))
    (setf (gethash (oauth-access-token-record-id record)
                   (memory-store-oauth-access-tokens store))
          (copy-oauth-access-token-record record)))
  (copy-oauth-access-token-record record))

(defun oauth-client-document-id (client-id)
  (format nil "oauth-client:~a" client-id))

(defun oauth-code-document-id (code-id)
  (format nil "oauth-code:~a" code-id))

(defun oauth-token-document-id (token-id)
  (format nil "oauth-access:~a" token-id))

(defun oauth-client-record-to-json (record)
  (let ((document
          (jsown:new-js
            ("_id" (oauth-client-document-id (oauth-client-record-id record)))
            ("kind" +oauth-client-kind+)
            ("client_id" (oauth-client-record-id record))
            ("status" (status-string (oauth-client-record-status record)))
            ("allowed_scopes" (copy-list (oauth-client-record-allowed-scopes record)))
            ("redirect_uris" (copy-list (oauth-client-record-redirect-uris record)))
            ("secret_salt" (oauth-client-record-secret-salt record))
            ("secret_verifier" (oauth-client-record-secret-verifier record))
            ("created_at" (oauth-client-record-created-at record)))))
    (when (oauth-client-record-revision record)
      (setf (jsown:val document "_rev") (oauth-client-record-revision record)))
    document))

(defun json-to-oauth-client-record (value)
  (let ((document (if (stringp value) (jsown:parse value) value)))
    (make-oauth-client-record
     :id (jsown:val document "client_id")
     :status (parse-status (jsown:val document "status"))
     :allowed-scopes (copy-list (or (jsown:val-safe document "allowed_scopes") nil))
     :redirect-uris (copy-list (or (jsown:val-safe document "redirect_uris") nil))
     :secret-salt (jsown:val document "secret_salt")
     :secret-verifier (jsown:val document "secret_verifier")
     :created-at (jsown:val document "created_at")
     :revision (jsown:val-safe document "_rev"))))

(defun oauth-code-record-to-json (record)
  (let ((document
          (jsown:new-js
            ("_id" (oauth-code-document-id (oauth-authorization-code-record-id record)))
            ("kind" +oauth-code-kind+)
            ("code_id" (oauth-authorization-code-record-id record))
            ("status" (status-string (oauth-authorization-code-record-status record)))
            ("client_id" (oauth-authorization-code-record-client-id record))
            ("owner" (oauth-authorization-code-record-owner record))
            ("scopes" (copy-list (oauth-authorization-code-record-scopes record)))
            ("redirect_uri" (oauth-authorization-code-record-redirect-uri record))
            ("code_challenge" (oauth-authorization-code-record-code-challenge record))
            ("code_challenge_method" (oauth-authorization-code-record-code-challenge-method record))
            ("salt" (oauth-authorization-code-record-salt record))
            ("verifier" (oauth-authorization-code-record-verifier record))
            ("created_at" (oauth-authorization-code-record-created-at record))
            ("expires_at" (oauth-authorization-code-record-expires-at record))
            ("consumed_at" (nullable-json-value (oauth-authorization-code-record-consumed-at record))))))
    (when (oauth-authorization-code-record-revision record)
      (setf (jsown:val document "_rev") (oauth-authorization-code-record-revision record)))
    document))

(defun json-to-oauth-code-record (value)
  (let ((document (if (stringp value) (jsown:parse value) value)))
    (make-oauth-authorization-code-record
     :id (jsown:val document "code_id")
     :status (parse-status (jsown:val document "status"))
     :client-id (jsown:val document "client_id")
     :owner (jsown:val document "owner")
     :scopes (copy-list (or (jsown:val-safe document "scopes") nil))
     :redirect-uri (jsown:val document "redirect_uri")
     :code-challenge (jsown:val document "code_challenge")
     :code-challenge-method (jsown:val document "code_challenge_method")
     :salt (jsown:val document "salt")
     :verifier (jsown:val document "verifier")
     :created-at (jsown:val document "created_at")
     :expires-at (jsown:val document "expires_at")
     :consumed-at (json-value-or-nil document "consumed_at")
     :revision (jsown:val-safe document "_rev"))))

(defun oauth-access-token-record-to-json (record)
  (let ((document
          (jsown:new-js
            ("_id" (oauth-token-document-id (oauth-access-token-record-id record)))
            ("kind" +oauth-access-token-kind+)
            ("token_id" (oauth-access-token-record-id record))
            ("status" (status-string (oauth-access-token-record-status record)))
            ("client_id" (oauth-access-token-record-client-id record))
            ("owner" (oauth-access-token-record-owner record))
            ("principal_type" (oauth-access-token-record-principal-type record))
            ("scopes" (copy-list (oauth-access-token-record-scopes record)))
            ("salt" (oauth-access-token-record-salt record))
            ("verifier" (oauth-access-token-record-verifier record))
            ("created_at" (oauth-access-token-record-created-at record))
            ("expires_at" (oauth-access-token-record-expires-at record))
            ("revoked_at" (nullable-json-value (oauth-access-token-record-revoked-at record))))))
    (when (oauth-access-token-record-revision record)
      (setf (jsown:val document "_rev") (oauth-access-token-record-revision record)))
    document))

(defun json-to-oauth-access-token-record (value)
  (let ((document (if (stringp value) (jsown:parse value) value)))
    (make-oauth-access-token-record
     :id (jsown:val document "token_id")
     :status (parse-status (jsown:val document "status"))
     :client-id (jsown:val document "client_id")
     :owner (jsown:val document "owner")
     :principal-type (jsown:val document "principal_type")
     :scopes (copy-list (or (jsown:val-safe document "scopes") nil))
     :salt (jsown:val document "salt")
     :verifier (jsown:val document "verifier")
     :created-at (jsown:val document "created_at")
     :expires-at (jsown:val document "expires_at")
     :revoked-at (json-value-or-nil document "revoked_at")
     :revision (jsown:val-safe document "_rev"))))

(defun couch-auth-get-record (store document-id parser)
  (anypool:with-connection (client (couchdb-store-pool store))
    (handler-case
        (funcall parser
                 (cl-couch:get-document
                  client (couchdb-store-database store) document-id))
      (dex:http-request-not-found () nil))))

(defun couch-auth-put-record (store record serializer conflict-code conflict-message)
  (anypool:with-connection (client (couchdb-store-pool store))
    (handler-case
        (let* ((response
                 (jsown:parse
                  (cl-couch:create-document
                   client
                   (couchdb-store-database store)
                   (jsown:to-json (funcall serializer record)))))
               (revision (jsown:val-safe response "rev")))
          revision)
      (dex:http-request-conflict ()
        (signal-oauth-error conflict-code conflict-message)))))

(defmethod oauth-client-store-get ((store couchdb-credential-store) client-id)
  (couch-auth-get-record store (oauth-client-document-id client-id)
                         #'json-to-oauth-client-record))

(defmethod oauth-client-store-put ((store couchdb-credential-store) record)
  (setf (oauth-client-record-revision record)
        (couch-auth-put-record store record #'oauth-client-record-to-json
                               "invalid_client" "OAuth client already exists"))
  record)

(defmethod oauth-client-store-update ((store couchdb-credential-store) record)
  (unless (oauth-client-record-revision record)
    (let ((current (oauth-client-store-get store (oauth-client-record-id record))))
      (unless current (signal-oauth-error "invalid_client" "OAuth client was not found"))
      (setf (oauth-client-record-revision record) (oauth-client-record-revision current))))
  (setf (oauth-client-record-revision record)
        (couch-auth-put-record store record #'oauth-client-record-to-json
                               "invalid_client" "OAuth client update conflicted"))
  record)

(defmethod oauth-code-store-get ((store couchdb-credential-store) code-id)
  (couch-auth-get-record store (oauth-code-document-id code-id)
                         #'json-to-oauth-code-record))

(defmethod oauth-code-store-put ((store couchdb-credential-store) record)
  (setf (oauth-authorization-code-record-revision record)
        (couch-auth-put-record store record #'oauth-code-record-to-json
                               "server_error" "OAuth code identifier conflicted"))
  record)

(defmethod oauth-code-store-update ((store couchdb-credential-store) record)
  (unless (oauth-authorization-code-record-revision record)
    (let ((current (oauth-code-store-get store (oauth-authorization-code-record-id record))))
      (unless current (signal-oauth-error "invalid_grant" "Authorization code is invalid"))
      (setf (oauth-authorization-code-record-revision record)
            (oauth-authorization-code-record-revision current))))
  (setf (oauth-authorization-code-record-revision record)
        (couch-auth-put-record store record #'oauth-code-record-to-json
                               "invalid_grant" "Authorization code is invalid"))
  record)

(defmethod oauth-access-token-store-get ((store couchdb-credential-store) token-id)
  (couch-auth-get-record store (oauth-token-document-id token-id)
                         #'json-to-oauth-access-token-record))

(defmethod oauth-access-token-store-put ((store couchdb-credential-store) record)
  (setf (oauth-access-token-record-revision record)
        (couch-auth-put-record store record #'oauth-access-token-record-to-json
                               "server_error" "OAuth token identifier conflicted"))
  record)

(defmethod oauth-access-token-store-update ((store couchdb-credential-store) record)
  (unless (oauth-access-token-record-revision record)
    (let ((current (oauth-access-token-store-get store (oauth-access-token-record-id record))))
      (unless current (signal-oauth-error "invalid_token" "OAuth access token was not found"))
      (setf (oauth-access-token-record-revision record)
            (oauth-access-token-record-revision current))))
  (setf (oauth-access-token-record-revision record)
        (couch-auth-put-record store record #'oauth-access-token-record-to-json
                               "invalid_token" "OAuth token update conflicted"))
  record)

(defun valid-https-redirect-uri-p (uri)
  (and (stringp uri)
       (<= 1 (length uri) 2048)
       (let ((parsed (ignore-errors (quri:uri uri))))
         (and parsed
              (string-equal "https" (quri:uri-scheme parsed))
              (quri:uri-host parsed)
              (null (quri:uri-fragment parsed))))))

(defun normalize-oauth-scopes (scopes)
  (let ((normalized (normalize-scopes scopes)))
    (unless normalized
      (signal-oauth-error "invalid_scope" "At least one OAuth scope is required"))
    normalized))

(defun scopes-subset-p (requested allowed)
  (every (lambda (scope) (member scope allowed :test #'string=)) requested))

(defun create-oauth-client (redirect-uris allowed-scopes &key (store *credential-store*))
  (unless store
    (signal-oauth-error "server_error" "OAuth store is unavailable"))
  (unless (and (listp redirect-uris) redirect-uris
               (every #'valid-https-redirect-uri-p redirect-uris))
    (signal-oauth-error "invalid_redirect_uri" "OAuth redirect URI is invalid"))
  (let* ((client-id (format nil "oauth_~a" (cms-ulid:ulid)))
         (secret (random-hex star:*auth-key-secret-bytes*))
         (salt (random-hex star:*auth-salt-bytes*))
         (record
           (make-oauth-client-record
            :id client-id
            :status :active
            :allowed-scopes (normalize-oauth-scopes allowed-scopes)
            :redirect-uris (remove-duplicates (copy-list redirect-uris) :test #'string=)
            :secret-salt salt
            :secret-verifier (verifier-hex (decode-hex secret) salt star:*auth-pepper*)
            :created-at (auth-now))))
    (oauth-client-store-put store record)
    (values record secret)))

(defun oauth-client-metadata-json (record)
  (jsown:new-js
    ("client_id" (oauth-client-record-id record))
    ("status" (status-string (oauth-client-record-status record)))
    ("allowed_scopes" (copy-list (oauth-client-record-allowed-scopes record)))
    ("redirect_uris" (copy-list (oauth-client-record-redirect-uris record)))
    ("created_at" (oauth-client-record-created-at record))))

(defun active-oauth-client (client-id store)
  (let ((client (and store (oauth-client-store-get store client-id))))
    (unless (and client (eq :active (oauth-client-record-status client)))
      (signal-oauth-error "invalid_client" "OAuth client authentication failed"))
    client))

(defun oauth-client-secret-valid-p (record secret)
  (and (stringp secret)
       (valid-hex-string-p secret (* 2 star:*auth-key-secret-bytes*))
       (handler-case
           (funcall *verifier-compare-function*
                    (decode-hex (oauth-client-record-secret-verifier record))
                    (derive-verifier (decode-hex secret)
                                     (oauth-client-record-secret-salt record)
                                     star:*auth-pepper*))
         (error () nil))))

(defun pkce-character-p (character)
  (or (alphanumericp character) (find character "-._~")))

(defun valid-pkce-verifier-p (value)
  (and (stringp value)
       (<= 43 (length value) 128)
       (every #'pkce-character-p value)))

(defun base64url-encode-octets (octets)
  (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"))
    (with-output-to-string (stream)
      (loop for index from 0 below (length octets) by 3
            for remaining = (- (length octets) index)
            for a = (aref octets index)
            for b = (if (> remaining 1) (aref octets (1+ index)) 0)
            for c = (if (> remaining 2) (aref octets (+ index 2)) 0)
            for bits = (logior (ash a 16) (ash b 8) c)
            do (write-char (char alphabet (ldb (byte 6 18) bits)) stream)
               (write-char (char alphabet (ldb (byte 6 12) bits)) stream)
               (when (> remaining 1)
                 (write-char (char alphabet (ldb (byte 6 6) bits)) stream))
               (when (> remaining 2)
                 (write-char (char alphabet (ldb (byte 6 0) bits)) stream))))))

(defun pkce-s256-challenge (verifier)
  (unless (valid-pkce-verifier-p verifier)
    (signal-oauth-error "invalid_grant" "Authorization code is invalid"))
  (base64url-encode-octets (sha256 (string-octets verifier))))

(defun make-oauth-secret-material (prefix)
  (let ((id (cms-ulid:ulid))
        (secret (random-hex star:*auth-key-secret-bytes*)))
    (values id secret (format nil "~a~a_~a" prefix id secret))))

(defun parse-oauth-secret-material (raw prefix)
  (handler-case
      (progn
        (unless (and (stringp raw)
                     (> (length raw) (length prefix))
                     (string= prefix raw :end2 (length prefix)))
          (signal-oauth-error "invalid_grant" "OAuth credential is invalid"))
        (let* ((tail (subseq raw (length prefix)))
               (separator (position #\_ tail)))
          (unless separator
            (signal-oauth-error "invalid_grant" "OAuth credential is invalid"))
          (let ((id (subseq tail 0 separator))
                (secret (subseq tail (1+ separator))))
            (unless (and (plusp (length id))
                         (valid-hex-string-p secret (* 2 star:*auth-key-secret-bytes*)))
              (signal-oauth-error "invalid_grant" "OAuth credential is invalid"))
            (values id secret))))
    (oauth-error (condition) (error condition))
    (error () (signal-oauth-error "invalid_grant" "OAuth credential is invalid"))))

(defun issue-oauth-authorization-code
    (client-id redirect-uri owner requested-scopes code-challenge code-challenge-method
     &key (store *credential-store*))
  (let* ((client (active-oauth-client client-id store))
         (scopes (normalize-oauth-scopes requested-scopes))
         (user (and store (user-store-get store (normalize-username owner)))))
    (unless (member redirect-uri (oauth-client-record-redirect-uris client) :test #'string=)
      (signal-oauth-error "invalid_redirect_uri" "OAuth redirect URI is invalid"))
    (unless (and user (user-active-p user))
      (signal-oauth-error "access_denied" "OAuth authorization was denied"))
    (unless (and (scopes-subset-p scopes (oauth-client-record-allowed-scopes client))
                 (scopes-subset-p scopes (user-record-scopes user)))
      (signal-oauth-error "invalid_scope" "Requested OAuth scope is not permitted"))
    (unless (and (stringp code-challenge-method)
                 (string= "S256" code-challenge-method)
                 (stringp code-challenge)
                 (= 43 (length code-challenge))
                 (every (lambda (c) (or (alphanumericp c) (find c "-_"))) code-challenge))
      (signal-oauth-error "invalid_request" "PKCE S256 is required"))
    (multiple-value-bind (id secret raw)
        (make-oauth-secret-material +oauth-code-prefix+)
      (let* ((salt (random-hex star:*auth-salt-bytes*))
             (now (auth-now))
             (record
               (make-oauth-authorization-code-record
                :id id
                :status :active
                :client-id client-id
                :owner (user-record-username user)
                :scopes scopes
                :redirect-uri redirect-uri
                :code-challenge code-challenge
                :code-challenge-method "S256"
                :salt salt
                :verifier (verifier-hex (decode-hex secret) salt star:*auth-pepper*)
                :created-at now
                :expires-at (+ now star:*oauth-authorization-code-seconds*))))
        (oauth-code-store-put store record)
        (values record raw)))))

(defun verify-stored-secret (secret salt verifier)
  (and (stringp secret)
       (valid-hex-string-p secret (* 2 star:*auth-key-secret-bytes*))
       (handler-case
           (funcall *verifier-compare-function*
                    (decode-hex verifier)
                    (derive-verifier (decode-hex secret) salt star:*auth-pepper*))
         (error () nil))))

(defun mint-oauth-access-token (client-id user scopes store)
  (multiple-value-bind (id secret raw)
      (make-oauth-secret-material +oauth-access-token-prefix+)
    (let* ((salt (random-hex star:*auth-salt-bytes*))
           (now (auth-now))
           (record
             (make-oauth-access-token-record
              :id id
              :status :active
              :client-id client-id
              :owner (user-record-username user)
              :principal-type (user-record-principal-type user)
              :scopes (copy-list scopes)
              :salt salt
              :verifier (verifier-hex (decode-hex secret) salt star:*auth-pepper*)
              :created-at now
              :expires-at (+ now star:*oauth-access-token-seconds*))))
      (oauth-access-token-store-put store record)
      (values record raw))))

(defun exchange-oauth-authorization-code
    (raw-code client-id client-secret redirect-uri code-verifier
     &key (store *credential-store*))
  (let ((client (active-oauth-client client-id store)))
    (unless (oauth-client-secret-valid-p client client-secret)
      (signal-oauth-error "invalid_client" "OAuth client authentication failed"))
    (multiple-value-bind (code-id code-secret)
        (parse-oauth-secret-material raw-code +oauth-code-prefix+)
      (let* ((record (oauth-code-store-get store code-id))
             (now (auth-now)))
        (unless (and record
                     (eq :active (oauth-authorization-code-record-status record))
                     (null (oauth-authorization-code-record-consumed-at record))
                     (> (oauth-authorization-code-record-expires-at record) now)
                     (string= client-id (oauth-authorization-code-record-client-id record))
                     (string= redirect-uri (oauth-authorization-code-record-redirect-uri record))
                     (verify-stored-secret code-secret
                                           (oauth-authorization-code-record-salt record)
                                           (oauth-authorization-code-record-verifier record))
                     (valid-pkce-verifier-p code-verifier)
                     (constant-time-secret=
                      (pkce-s256-challenge code-verifier)
                      (oauth-authorization-code-record-code-challenge record)))
          (signal-oauth-error "invalid_grant" "Authorization code is invalid"))
        (let ((user (user-store-get store (oauth-authorization-code-record-owner record))))
          (unless (and user
                       (user-active-p user)
                       (scopes-subset-p (oauth-authorization-code-record-scopes record)
                                        (user-record-scopes user)))
            (signal-oauth-error "invalid_grant" "Authorization code is invalid"))
          (setf (oauth-authorization-code-record-consumed-at record) now
                (oauth-authorization-code-record-status record) :consumed)
          (handler-case
              (oauth-code-store-update store record)
            (oauth-error ()
              (signal-oauth-error "invalid_grant" "Authorization code is invalid")))
          (mint-oauth-access-token
           client-id user (oauth-authorization-code-record-scopes record) store))))))

(defun parse-oauth-access-token (raw-token)
  (handler-case
      (parse-oauth-secret-material raw-token +oauth-access-token-prefix+)
    (oauth-error () (signal-authentication-failure))))

(defun active-oauth-access-token-p (record now)
  (and record
       (eq :active (oauth-access-token-record-status record))
       (> (oauth-access-token-record-expires-at record) now)
       (null (oauth-access-token-record-revoked-at record))))

(defun authenticate-oauth-access-token
    (raw-token correlation-id deadline &key (store *credential-store*))
  (unless store (signal-authentication-failure))
  (multiple-value-bind (token-id secret)
      (parse-oauth-access-token raw-token)
    (let* ((record (oauth-access-token-store-get store token-id))
           (now (auth-now)))
      (unless (and (active-oauth-access-token-p record now)
                   (verify-stored-secret secret
                                         (oauth-access-token-record-salt record)
                                         (oauth-access-token-record-verifier record)))
        (signal-authentication-failure))
      (let ((user (user-store-get store (oauth-access-token-record-owner record))))
        (unless (and user
                     (user-active-p user)
                     (string= (user-record-principal-type user)
                              (oauth-access-token-record-principal-type record))
                     (scopes-subset-p (oauth-access-token-record-scopes record)
                                      (user-record-scopes user)))
          (signal-authentication-failure))
        (%make-request-security-context
         :principal
         (%make-request-principal
          :id (oauth-access-token-record-owner record)
          :type (oauth-access-token-record-principal-type record)
          :scopes (copy-list (oauth-access-token-record-scopes record))
          :credential-id (oauth-access-token-record-id record))
         :correlation-id correlation-id
         :deadline deadline
         :authenticated-at now)))))

(defun revoke-oauth-access-token (token-id &key (store *credential-store*))
  (let ((record (and store (oauth-access-token-store-get store token-id))))
    (unless record
      (signal-oauth-error "invalid_token" "OAuth access token was not found"))
    (setf (oauth-access-token-record-status record) :revoked
          (oauth-access-token-record-revoked-at record) (auth-now))
    (oauth-access-token-store-update store record)))
