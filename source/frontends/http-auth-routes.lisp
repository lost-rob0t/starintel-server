(in-package :star.frontends.http-api)

(defun lifecycle-error-status (code)
  (cond
    ((member code
             '("invalid_owner" "invalid_scopes" "invalid_expiry"
               "invalid_overlap" "invalid_username" "invalid_password"
               "password_too_short")
             :test #'string=)
     422)
    ((member code '("credential_not_found" "user_not_found") :test #'string=)
     404)
    ((member code
             '("credential_conflict" "credential_not_active"
               "bootstrap_complete" "user_conflict")
             :test #'string=)
     409)
    ((string= code "bootstrap_denied") 403)
    ((string= code "auth_store_unavailable") 503)
    (t 400)))

(defmacro with-credential-lifecycle-errors (&body body)
  `(handler-case
       (progn ,@body)
     (star.auth:credential-lifecycle-error (condition)
       (signal-http-input-error
        (lifecycle-error-status
         (star.auth:credential-lifecycle-error-code condition))
        (star.auth:credential-lifecycle-error-code condition)
        (star.auth:credential-lifecycle-error-message condition)))))

(defun require-auth-string (document field)
  (let ((value (jsown:val-safe document field)))
    (unless (and (stringp value) (plusp (length value)))
      (signal-http-input-error
       422
       "invalid_auth_request"
       (format nil "Field ~a must be a non-empty string" field)))
    value))

(defun optional-positive-integer (document field)
  (let ((value (jsown:val-safe document field)))
    (cond
      ((or (null value) (eq value :null)) nil)
      ((and (integerp value) (plusp value)) value)
      (t
       (signal-http-input-error
        422
        "invalid_auth_request"
        (format nil "Field ~a must be a positive integer" field))))))

(defun optional-auth-boolean (document field default)
  ;; JSOWN's default reader maps JSON false to NIL, the same value returned by
  ;; VAL-SAFE for a missing key. Check key presence first so an explicit false
  ;; is not silently replaced by DEFAULT.
  (unless (jsown:keyp document field)
    (return-from optional-auth-boolean default))
  (let ((value (jsown:val-safe document field)))
    (cond
      ((or (eq value t) (eq value :true)) t)
      ((or (null value) (eq value :false)) nil)
      ((eq value :null) default)
      (t
       (signal-http-input-error
        422
        "invalid_auth_request"
        (format nil "Field ~a must be a boolean" field))))))

(defun require-scope-array (document)
  (let ((scopes (jsown:val-safe document "scopes")))
    (unless (and (json-array-p scopes)
                 (every (lambda (scope)
                          (and (stringp scope)
                               (plusp (length scope))))
                        scopes))
      (signal-http-input-error
       422
       "invalid_auth_request"
       "Field scopes must be an array of non-empty strings"))
    scopes))

(defun add-no-store-header ()
  (setf (lack.response:response-headers *response*)
        (append (lack.response:response-headers *response*)
                (list :cache-control "no-store"
                      :pragma "no-cache"))))

(defun credential-secret-response (record raw-key)
  (add-no-store-header)
  (jsown:to-json
   (jsown:new-js
     ("api_key" raw-key)
     ("credential" (star.auth:api-key-metadata-json record))
     ("correlation_id" (current-correlation-id)))))

(defun user-login-response (user record raw-key)
  (add-no-store-header)
  (jsown:to-json
   (jsown:new-js
     ("api_key" raw-key)
     ("credential" (star.auth:api-key-metadata-json record))
     ("user" (star.auth:user-metadata-json user))
     ("correlation_id" (current-correlation-id)))))

(defun user-status-response (record message)
  (jsown:to-json
   (jsown:new-js
     ("status" "ok")
     ("msg" message)
     ("user" (star.auth:user-metadata-json record))
     ("correlation_id" (current-correlation-id)))))

(defun handle-auth-login-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (with-credential-lifecycle-errors
      (let* ((body (require-json-object (parse-json-request)))
             (username (require-auth-string body "username"))
             (password (require-auth-string body "password")))
        (handler-case
            (multiple-value-bind (user record raw-key)
                (star.auth:login-user username password)
              (user-login-response user record raw-key))
          (star.auth:authentication-error ()
            (signal-http-input-error
             401
             "invalid_credential"
             "Authentication failed")))))))

(defun handle-auth-bootstrap-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (with-credential-lifecycle-errors
      (let* ((request (ningle:context :request))
             (headers (lack.request:request-headers request))
             (presented-secret
               (request-header-value headers "X-Star-Bootstrap-Secret"))
             (body (require-json-object (parse-json-request)))
             (owner (or (jsown:val-safe body "owner")
                        "bootstrap-administrator")))
        (unless (and (stringp owner) (plusp (length owner)))
          (signal-http-input-error
           422
           "invalid_auth_request"
           "Field owner must be a non-empty string"))
        (multiple-value-bind (record raw-key)
            (star.auth:bootstrap-api-key presented-secret owner)
          (setf (lack.response:response-status *response*) 201)
          (credential-secret-response record raw-key))))))

(defun handle-auth-create-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (require-administrator-context)
    (with-credential-lifecycle-errors
      (let* ((body (require-json-object (parse-json-request)))
             (owner (require-auth-string body "owner"))
             (principal-type
               (require-auth-string body "principal_type"))
             (scopes (require-scope-array body))
             (expires-in-seconds
               (optional-positive-integer body "expires_in_seconds")))
        (multiple-value-bind (record raw-key)
            (star.auth:create-api-key
             owner
             principal-type
             scopes
             :expires-in-seconds expires-in-seconds)
          (setf (lack.response:response-status *response*) 201)
          (credential-secret-response record raw-key))))))

(defun handle-auth-list-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (require-administrator-context)
    (with-credential-lifecycle-errors
      (jsown:to-json
       (star.auth:list-api-key-metadata)))))

(defun handle-auth-create-user-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (require-administrator-context)
    (with-credential-lifecycle-errors
      (let* ((body (require-json-object (parse-json-request)))
             (username (require-auth-string body "username"))
             (password (require-auth-string body "password"))
             (principal-type
               (or (jsown:val-safe body "principal_type") "user"))
             (scopes (require-scope-array body))
             (must-change-password
               (optional-auth-boolean body "must_change_password" t)))
        (unless (and (stringp principal-type) (plusp (length principal-type)))
          (signal-http-input-error
           422
           "invalid_auth_request"
           "Field principal_type must be a non-empty string"))
        (let ((record
                (star.auth:create-user
                 username
                 password
                 principal-type
                 scopes
                 :must-change-password must-change-password)))
          (setf (lack.response:response-status *response*) 201)
          (user-status-response record "User created"))))))

(defun handle-auth-list-users-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (require-administrator-context)
    (with-credential-lifecycle-errors
      (jsown:to-json (star.auth:list-user-metadata)))))

(defun user-name-param (params)
  (let ((username (query-value params "username")))
    (unless (and (stringp username) (plusp (length username)))
      (signal-http-input-error
       400
       "missing_path_parameter"
       "Username is required"))
    username))

(defun handle-auth-reset-user-password-route (params)
  (with-http-boundary ()
    (require-administrator-context)
    (with-credential-lifecycle-errors
      (let* ((body (require-json-object (parse-json-request)))
             (password (require-auth-string body "password"))
             (must-change-password
               (optional-auth-boolean body "must_change_password" t))
             (record
               (star.auth:admin-set-user-password
                (user-name-param params)
                password
                :must-change-password must-change-password)))
        (user-status-response record "Password updated")))))

(defun handle-auth-change-password-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (with-credential-lifecycle-errors
      (let* ((username (star.auth:current-principal-id))
             (body (require-json-object (parse-json-request)))
             (current-password
               (require-auth-string body "current_password"))
             (new-password
               (require-auth-string body "new_password")))
        (handler-case
            (user-status-response
             (star.auth:change-user-password
              username current-password new-password)
             "Password changed")
          (star.auth:authentication-error ()
            (signal-http-input-error
             401
             "invalid_credential"
             "Authentication failed")))))))

(defun credential-id-param (params)
  (let ((credential-id (query-value params "credential-id")))
    (unless (and (stringp credential-id)
                 (plusp (length credential-id)))
      (signal-http-input-error
       400
       "missing_path_parameter"
       "Credential identifier is required"))
    credential-id))

(defun handle-auth-rotate-route (params)
  (with-http-boundary ()
    (require-administrator-context)
    (with-credential-lifecycle-errors
      (let* ((credential-id (credential-id-param params))
             (body (require-json-object (parse-json-request)))
             (overlap-seconds
               (or (jsown:val-safe body "overlap_seconds") 0)))
        (unless (and (integerp overlap-seconds)
                     (not (minusp overlap-seconds)))
          (signal-http-input-error
           422
           "invalid_auth_request"
           "Field overlap_seconds must be a non-negative integer"))
        (multiple-value-bind (record raw-key)
            (star.auth:rotate-api-key credential-id overlap-seconds)
          (setf (lack.response:response-status *response*) 201)
          (credential-secret-response record raw-key))))))

(defun lifecycle-status-response (record message)
  (jsown:to-json
   (jsown:new-js
     ("status" "ok")
     ("msg" message)
     ("credential" (star.auth:api-key-metadata-json record))
     ("correlation_id" (current-correlation-id)))))

(defun handle-auth-revoke-route (params)
  (with-http-boundary ()
    (require-administrator-context)
    (with-credential-lifecycle-errors
      (lifecycle-status-response
       (star.auth:revoke-api-key (credential-id-param params))
       "Credential revoked"))))

(defun handle-auth-disable-route (params)
  (with-http-boundary ()
    (require-administrator-context)
    (with-credential-lifecycle-errors
      (lifecycle-status-response
       (star.auth:disable-api-key (credential-id-param params))
       "Credential disabled"))))

(defun handle-auth-context-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (let* ((context star.auth:*request-security-context*)
           (principal
             (star.auth:request-security-context-principal context)))
      (jsown:to-json
       (jsown:new-js
         ("principal_id" (star.auth:request-principal-id principal))
         ("principal_type" (star.auth:request-principal-type principal))
         ("credential_id"
          (star.auth:request-principal-credential-id principal))
         ("scopes" (star.auth:request-principal-scopes principal))
         ("correlation_id"
          (star.auth:request-security-context-correlation-id context))
         ("deadline"
          (star.auth:request-security-context-deadline context)))))))

(setf (ningle:route *app* "/auth/login" :method :post)
      #'handle-auth-login-route)

(setf (ningle:route *app* "/auth/bootstrap" :method :post)
      #'handle-auth-bootstrap-route)

(setf (ningle:route *app* "/auth/credentials" :method :post)
      #'handle-auth-create-route)

(setf (ningle:route *app* "/auth/credentials" :method :get)
      #'handle-auth-list-route)

(setf (ningle:route *app* "/auth/users" :method :post)
      #'handle-auth-create-user-route)

(setf (ningle:route *app* "/auth/users" :method :get)
      #'handle-auth-list-users-route)

(setf (ningle:route *app* "/auth/users/:username/password" :method :post)
      #'handle-auth-reset-user-password-route)

(setf (ningle:route *app* "/auth/password" :method :post)
      #'handle-auth-change-password-route)

(setf (ningle:route *app* "/auth/credentials/:credential-id/rotate" :method :post)
      #'handle-auth-rotate-route)

(setf (ningle:route *app* "/auth/credentials/:credential-id/revoke" :method :post)
      #'handle-auth-revoke-route)

(setf (ningle:route *app* "/auth/credentials/:credential-id/disable" :method :post)
      #'handle-auth-disable-route)

(setf (ningle:route *app* "/auth/context" :method :get)
      #'handle-auth-context-route)
