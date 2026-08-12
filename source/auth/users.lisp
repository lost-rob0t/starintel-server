(in-package :star.auth)

(defparameter +user-kind+ "user")

(defstruct user-record
  username
  principal-type
  scopes
  status
  password-hash
  created-at
  password-updated-at
  must-change-password
  revision)

(defgeneric user-store-get (store username))
(defgeneric user-store-put (store record))
(defgeneric user-store-update (store record))
(defgeneric user-store-list (store))
(defgeneric user-store-count (store))

(defun normalize-username (username)
  (unless (and (stringp username)
               (<= 1 (length username) 64)
               (every (lambda (character)
                        (or (alphanumericp character)
                            (find character "-_.")))
                      username))
    (signal-lifecycle-error
     "invalid_username"
     "Username must be 1-64 characters using letters, digits, '-', '_', or '.'"))
  (string-downcase username))

(defun password-material (password)
  (concatenate-octet-vectors
   (string-octets star:*auth-pepper*)
   (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)
   (string-octets password)))

(defun validate-user-password (password &key allow-weak-password)
  (unless (and (stringp password) (plusp (length password)))
    (signal-lifecycle-error
     "invalid_password"
     "Password must be a non-empty string"))
  (unless (or allow-weak-password
              (>= (length password) star:*auth-password-min-length*))
    (signal-lifecycle-error
     "password_too_short"
     (format nil "Password must be at least ~d characters"
             star:*auth-password-min-length*)))
  password)

(defun hash-user-password (password &key allow-weak-password)
  (validate-user-password password :allow-weak-password allow-weak-password)
  (ironclad:pbkdf2-hash-password-to-combined-string
   (password-material password)
   :digest :sha256
   :iterations star:*auth-password-iterations*))

(defun user-password-valid-p (record password)
  (and record
       (stringp password)
       (handler-case
           (ironclad:pbkdf2-check-password
            (password-material password)
            (user-record-password-hash record))
         (error () nil))))

(defun user-active-p (record)
  (and record (eq :active (user-record-status record))))

(defun user-metadata-json (record)
  (jsown:new-js
    ("username" (user-record-username record))
    ("principal_type" (user-record-principal-type record))
    ("scopes" (copy-list (user-record-scopes record)))
    ("status" (string-downcase (symbol-name (user-record-status record))))
    ("created_at" (user-record-created-at record))
    ("password_updated_at" (user-record-password-updated-at record))
    ("must_change_password"
     (if (user-record-must-change-password record) :true :false))))

(defun create-user (username password principal-type scopes
                    &key
                      (must-change-password t)
                      allow-weak-password
                      (store *credential-store*))
  (unless store
    (signal-lifecycle-error
     "auth_store_unavailable"
     "Credential store is unavailable"))
  (let* ((normalized-username (normalize-username username))
         (normalized-scopes (normalize-scopes scopes)))
    (when (user-store-get store normalized-username)
      (signal-lifecycle-error
       "user_conflict"
       "User already exists"))
    (let* ((now (auth-now))
           (record
             (make-user-record
              :username normalized-username
              :principal-type (normalize-principal-type principal-type)
              :scopes normalized-scopes
              :status :active
              :password-hash
              (hash-user-password
               password
               :allow-weak-password allow-weak-password)
              :created-at now
              :password-updated-at now
              :must-change-password (not (null must-change-password)))))
      (user-store-put store record))))

(defun list-user-metadata (&key (store *credential-store*))
  (unless store
    (signal-lifecycle-error
     "auth_store_unavailable"
     "Credential store is unavailable"))
  (mapcar #'user-metadata-json (user-store-list store)))

(defun authenticate-user-password (username password
                                    &key (store *credential-store*))
  (unless store
    (signal-authentication-failure))
  (handler-case
      (let* ((normalized-username (normalize-username username))
             (record (user-store-get store normalized-username)))
        (unless (and (user-active-p record)
                     (user-password-valid-p record password))
          (signal-authentication-failure))
        record)
    (authentication-error (condition)
      (error condition))
    (error ()
      (signal-authentication-failure))))

(defun login-user (username password &key (store *credential-store*))
  (let ((user (authenticate-user-password username password :store store)))
    (multiple-value-bind (credential raw-key)
        (create-api-key
         (user-record-username user)
         (user-record-principal-type user)
         (user-record-scopes user)
         :expires-in-seconds star:*auth-login-session-seconds*
         :store store)
      (values user credential raw-key))))

(defun set-user-password (record password
                          &key
                            (must-change-password nil)
                            allow-weak-password
                            (store *credential-store*))
  (unless record
    (signal-lifecycle-error "user_not_found" "User was not found"))
  (setf (user-record-password-hash record)
        (hash-user-password password :allow-weak-password allow-weak-password)
        (user-record-password-updated-at record) (auth-now)
        (user-record-must-change-password record)
        (not (null must-change-password)))
  (user-store-update store record))

(defun change-user-password (username current-password new-password
                             &key (store *credential-store*))
  (let ((record
          (authenticate-user-password
           username current-password :store store)))
    (set-user-password record new-password :store store)))

(defun admin-set-user-password (username new-password
                                &key
                                  (must-change-password t)
                                  (store *credential-store*))
  (let ((record
          (user-store-get store (normalize-username username))))
    (unless record
      (signal-lifecycle-error "user_not_found" "User was not found"))
    (set-user-password
     record
     new-password
     :must-change-password must-change-password
     :store store)))

(defun ensure-initial-user (&key (store *credential-store*))
  (unless store
    (signal-lifecycle-error
     "auth_store_unavailable"
     "Credential store is unavailable"))
  (if (plusp (user-store-count store))
      (user-store-get store (normalize-username star:*auth-initial-username*))
      (create-user
       star:*auth-initial-username*
       star:*auth-initial-password*
       "administrator"
       (list "admin")
       :must-change-password t
       :allow-weak-password t
       :store store)))

(defun user-document-id (username)
  (format nil "user:~a" (normalize-username username)))

(defun copy-user-record-or-nil (record)
  (and record (copy-user-record record)))

(defmethod user-store-get ((store memory-credential-store) username)
  (bt:with-lock-held ((memory-store-lock store))
    (copy-user-record-or-nil
     (gethash (normalize-username username)
              (memory-store-users store)))))

(defmethod user-store-put ((store memory-credential-store) record)
  (bt:with-lock-held ((memory-store-lock store))
    (let ((username (user-record-username record)))
      (when (gethash username (memory-store-users store))
        (signal-lifecycle-error "user_conflict" "User already exists"))
      (setf (gethash username (memory-store-users store))
            (copy-user-record record))))
  (copy-user-record record))

(defmethod user-store-update ((store memory-credential-store) record)
  (bt:with-lock-held ((memory-store-lock store))
    (let ((username (user-record-username record)))
      (unless (gethash username (memory-store-users store))
        (signal-lifecycle-error "user_not_found" "User was not found"))
      (setf (gethash username (memory-store-users store))
            (copy-user-record record))))
  (copy-user-record record))

(defmethod user-store-list ((store memory-credential-store))
  (bt:with-lock-held ((memory-store-lock store))
    (sort
     (loop for record being the hash-values of (memory-store-users store)
           collect (copy-user-record record))
     #'string<
     :key #'user-record-username)))

(defmethod user-store-count ((store memory-credential-store))
  (bt:with-lock-held ((memory-store-lock store))
    (hash-table-count (memory-store-users store))))

(defun user-record-to-json (record)
  (let ((document
          (jsown:new-js
            ("_id" (user-document-id (user-record-username record)))
            ("kind" +user-kind+)
            ("username" (user-record-username record))
            ("principal_type" (user-record-principal-type record))
            ("scopes" (copy-list (user-record-scopes record)))
            ("status" (status-string (user-record-status record)))
            ("password_hash" (user-record-password-hash record))
            ("created_at" (user-record-created-at record))
            ("password_updated_at" (user-record-password-updated-at record))
            ("must_change_password"
             (if (user-record-must-change-password record) :true :false)))))
    (when (user-record-revision record)
      (setf (jsown:val document "_rev") (user-record-revision record)))
    document))

(defun json-true-p (value)
  (or (eq value t) (eq value :true)))

(defun json-to-user-record (value)
  (let ((document (if (stringp value) (jsown:parse value) value)))
    (make-user-record
     :username (jsown:val document "username")
     :principal-type (jsown:val document "principal_type")
     :scopes (copy-list (or (jsown:val-safe document "scopes") nil))
     :status (parse-status (jsown:val document "status"))
     :password-hash (jsown:val document "password_hash")
     :created-at (jsown:val document "created_at")
     :password-updated-at (jsown:val document "password_updated_at")
     :must-change-password
     (json-true-p (jsown:val-safe document "must_change_password"))
     :revision (jsown:val-safe document "_rev"))))

(defun update-user-revision-from-response (record response)
  (let ((parsed (ignore-errors (jsown:parse response))))
    (when parsed
      (setf (user-record-revision record) (jsown:val-safe parsed "rev"))))
  record)

(defmethod user-store-get ((store couchdb-credential-store) username)
  (anypool:with-connection (client (couchdb-store-pool store))
    (handler-case
        (json-to-user-record
         (cl-couch:get-document
          client
          (couchdb-store-database store)
          (user-document-id username)))
      (dex:http-request-not-found () nil))))

(defmethod user-store-put ((store couchdb-credential-store) record)
  (anypool:with-connection (client (couchdb-store-pool store))
    (handler-case
        (update-user-revision-from-response
         record
         (cl-couch:create-document
          client
          (couchdb-store-database store)
          (jsown:to-json (user-record-to-json record))))
      (dex:http-request-conflict ()
        (signal-lifecycle-error "user_conflict" "User already exists")))))

(defmethod user-store-update ((store couchdb-credential-store) record)
  (unless (user-record-revision record)
    (let ((current
            (user-store-get store (user-record-username record))))
      (unless current
        (signal-lifecycle-error "user_not_found" "User was not found"))
      (setf (user-record-revision record) (user-record-revision current))))
  (anypool:with-connection (client (couchdb-store-pool store))
    (handler-case
        (update-user-revision-from-response
         record
         (cl-couch:create-document
          client
          (couchdb-store-database store)
          (jsown:to-json (user-record-to-json record))))
      (dex:http-request-conflict ()
        (signal-lifecycle-error "user_conflict" "User update conflicted")))))

(defmethod user-store-list ((store couchdb-credential-store))
  (anypool:with-connection (client (couchdb-store-pool store))
    (let* ((view
             (star.databases.couchdb:query-view
              client
              (couchdb-store-database store)
              "auth"
              "users"
              :include-docs t
              :limit 10000
              :reduce nil))
           (rows (or (jsown:val-safe view "rows") nil)))
      (loop for row in rows
            for document = (jsown:val row "doc")
            collect (json-to-user-record document)))))

(defmethod user-store-count ((store couchdb-credential-store))
  (length (user-store-list store)))
