(in-package :star.auth)

(defclass memory-credential-store (credential-store)
  ((records
    :initform (make-hash-table :test #'equal)
    :reader memory-store-records)
   (users
    :initform (make-hash-table :test #'equal)
    :reader memory-store-users)
   (oauth-clients
    :initform (make-hash-table :test #'equal)
    :reader memory-store-oauth-clients)
   (oauth-codes
    :initform (make-hash-table :test #'equal)
    :reader memory-store-oauth-codes)
   (oauth-access-tokens
    :initform (make-hash-table :test #'equal)
    :reader memory-store-oauth-access-tokens)
   (lock
    :initform (bt:make-lock "memory-credential-store")
    :reader memory-store-lock)))

(defun make-memory-credential-store ()
  (make-instance 'memory-credential-store))

(defun copy-record-or-nil (record)
  (and record (copy-api-key-record record)))

(defmethod credential-store-get ((store memory-credential-store) credential-id)
  (bt:with-lock-held ((memory-store-lock store))
    (copy-record-or-nil
     (gethash credential-id (memory-store-records store)))))

(defmethod credential-store-put ((store memory-credential-store) record)
  (bt:with-lock-held ((memory-store-lock store))
    (when (gethash (api-key-record-id record)
                   (memory-store-records store))
      (signal-lifecycle-error
       "credential_conflict"
       "Credential identifier already exists"))
    (setf (gethash (api-key-record-id record)
                   (memory-store-records store))
          (copy-api-key-record record)))
  (copy-api-key-record record))

(defmethod credential-store-update ((store memory-credential-store) record)
  (bt:with-lock-held ((memory-store-lock store))
    (unless (gethash (api-key-record-id record)
                     (memory-store-records store))
      (signal-lifecycle-error
       "credential_not_found"
       "Credential was not found"))
    (setf (gethash (api-key-record-id record)
                   (memory-store-records store))
          (copy-api-key-record record)))
  (copy-api-key-record record))

(defmethod credential-store-list ((store memory-credential-store))
  (bt:with-lock-held ((memory-store-lock store))
    (sort
     (loop for record being the hash-values of (memory-store-records store)
           collect (copy-api-key-record record))
     #'<
     :key #'api-key-record-created-at)))

(defmethod credential-store-count ((store memory-credential-store))
  (bt:with-lock-held ((memory-store-lock store))
    (hash-table-count (memory-store-records store))))

(defclass couchdb-credential-store (credential-store)
  ((pool
    :initarg :pool
    :reader couchdb-store-pool)
   (database
    :initarg :database
    :reader couchdb-store-database)))

(defun make-auth-couchdb-pool ()
  (anypool:make-pool
   :name "starintel-auth-couchdb-connections"
   :connector
   (lambda ()
     (let ((client
             (cl-couch:new-couchdb
              star:*couchdb-host*
              star:*couchdb-port*
              :scheme star:*couchdb-scheme*)))
       (cl-couch:password-auth
        client
        star:*couchdb-user*
        star:*couchdb-password*)
       client))
   :disconnector
   (lambda (client)
     (setf (cl-couch:couchdb-headers client) nil))
   :max-open-count 10
   :max-idle-count 5))

(defun make-couchdb-credential-store ()
  (make-instance
   'couchdb-credential-store
   :pool (make-auth-couchdb-pool)
   :database star:*couchdb-auth-database*))

(defun status-string (status)
  (string-downcase (symbol-name status)))

(defun parse-status (status)
  (intern (string-upcase status) :keyword))

(defun api-key-record-to-json (record)
  (let ((document
          (jsown:new-js
            ("_id" (api-key-record-id record))
            ("kind" +credential-kind+)
            ("owner" (api-key-record-owner record))
            ("principal_type" (api-key-record-principal-type record))
            ("scopes" (copy-list (api-key-record-scopes record)))
            ("status" (status-string (api-key-record-status record)))
            ("salt" (api-key-record-salt record))
            ("verifier" (api-key-record-verifier record))
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
                                   (api-key-record-overlap-expires-at record))))))
    (when (api-key-record-revision record)
      (setf (jsown:val document "_rev")
            (api-key-record-revision record)))
    document))

(defun null-json-value-p (value)
  (or (null value) (eq value :null)))

(defun json-value-or-nil (document key)
  (let ((value (jsown:val-safe document key)))
    (unless (null-json-value-p value)
      value)))

(defun json-to-api-key-record (value)
  (let ((document (if (stringp value)
                      (jsown:parse value)
                      value)))
    (make-api-key-record
     :id (jsown:val document "_id")
     :owner (jsown:val document "owner")
     :principal-type (jsown:val document "principal_type")
     :scopes (copy-list (or (jsown:val-safe document "scopes") nil))
     :status (parse-status (jsown:val document "status"))
     :salt (jsown:val document "salt")
     :verifier (jsown:val document "verifier")
     :created-at (jsown:val document "created_at")
     :expires-at (json-value-or-nil document "expires_at")
     :disabled-at (json-value-or-nil document "disabled_at")
     :revoked-at (json-value-or-nil document "revoked_at")
     :rotation-parent-id
     (json-value-or-nil document "rotation_parent_id")
     :superseded-by (json-value-or-nil document "superseded_by")
     :overlap-expires-at
     (json-value-or-nil document "overlap_expires_at")
     :revision (jsown:val-safe document "_rev"))))

(defun update-record-revision-from-response (record response)
  (let ((parsed (ignore-errors (jsown:parse response))))
    (when parsed
      (setf (api-key-record-revision record)
            (jsown:val-safe parsed "rev"))))
  record)

(defmethod credential-store-get ((store couchdb-credential-store) credential-id)
  (anypool:with-connection (client (couchdb-store-pool store))
    (handler-case
        (json-to-api-key-record
         (cl-couch:get-document
          client
          (couchdb-store-database store)
          credential-id))
      (dex:http-request-not-found () nil))))

(defmethod credential-store-put ((store couchdb-credential-store) record)
  (anypool:with-connection (client (couchdb-store-pool store))
    (handler-case
        (update-record-revision-from-response
         record
         (cl-couch:create-document
          client
          (couchdb-store-database store)
          (jsown:to-json (api-key-record-to-json record))))
      (dex:http-request-conflict ()
        (signal-lifecycle-error
         "credential_conflict"
         "Credential identifier already exists")))))

(defmethod credential-store-update ((store couchdb-credential-store) record)
  (unless (api-key-record-revision record)
    (let ((current
            (credential-store-get store (api-key-record-id record))))
      (unless current
        (signal-lifecycle-error
         "credential_not_found"
         "Credential was not found"))
      (setf (api-key-record-revision record)
            (api-key-record-revision current))))
  (anypool:with-connection (client (couchdb-store-pool store))
    (handler-case
        (update-record-revision-from-response
         record
         (cl-couch:create-document
          client
          (couchdb-store-database store)
          (jsown:to-json (api-key-record-to-json record))))
      (dex:http-request-conflict ()
        (signal-lifecycle-error
         "credential_conflict"
         "Credential update conflicted")))))

(defmethod credential-store-list ((store couchdb-credential-store))
  (anypool:with-connection (client (couchdb-store-pool store))
    (let* ((view
             (star.databases.couchdb:query-view
              client
              (couchdb-store-database store)
              "auth"
              "credentials"
              :include-docs t
              :limit 10000
              :reduce nil))
           (rows (or (jsown:val-safe view "rows") nil)))
      (loop for row in rows
            for document = (jsown:val row "doc")
            collect (json-to-api-key-record document)))))

(defmethod credential-store-count ((store couchdb-credential-store))
  (length (credential-store-list store)))

(defun auth-design-document ()
  (jsown:new-js
    ("_id" "_design/auth")
    ("views"
     (jsown:new-js
       ("credentials"
        (jsown:new-js
          ("map"
           "function(doc){if(doc.kind==='api-key'){emit(doc.created_at,null);}}")))
       ("users"
        (jsown:new-js
          ("map"
           "function(doc){if(doc.kind==='user'){emit(doc.username,null);}}")))))))

(defun ensure-auth-database (store)
  (anypool:with-connection (client (couchdb-store-pool store))
    (handler-case
        (cl-couch:get-database client (couchdb-store-database store))
      (dex:http-request-not-found ()
        (cl-couch:create-database client (couchdb-store-database store))))))

(defun ensure-auth-design-document (store)
  (anypool:with-connection (client (couchdb-store-pool store))
    (let* ((database (couchdb-store-database store))
           (document (auth-design-document)))
      (handler-case
          (let* ((existing
                   (jsown:parse
                    (cl-couch:get-document
                     client database "_design/auth")))
                 (revision (jsown:val existing "_rev")))
            (setf (jsown:val document "_rev") revision)
            (cl-couch:create-document
             client database (jsown:to-json document)))
        (dex:http-request-not-found ()
          (cl-couch:create-document
           client database (jsown:to-json document)))))))

(defun loopback-address-p (address)
  (member (string-downcase address)
          '("localhost" "127.0.0.1" "::1")
          :test #'string=))

(defun validate-auth-configuration ()
  (let ((mode (string-downcase star:*auth-mode*)))
    (cond
      ((string= mode "api-key")
       (unless (and (stringp star:*auth-pepper*)
                    (plusp (length star:*auth-pepper*)))
         (error "STAR_AUTH_PEPPER or STAR_AUTH_PEPPER_FILE is required")))
      ((string= mode "disabled")
       (unless (and star:*auth-dev-bypass*
                    (loopback-address-p star:*http-api-address*))
         (error "Disabled authentication requires explicit loopback development bypass")))
      (t
       (error "Unsupported STAR_AUTH_MODE: ~a" star:*auth-mode*))))
  t)

(defun initialize-auth-store (&key force)
  (validate-auth-configuration)
  (when (or force (null *credential-store*))
    (setf *credential-store* (make-couchdb-credential-store)))
  (when (typep *credential-store* 'couchdb-credential-store)
    (ensure-auth-database *credential-store*)
    (ensure-auth-design-document *credential-store*))
  *credential-store*)