(in-package :star.api.client)

(defun require-json-object-response (value operation-id)
  (unless (and (consp value) (eq (first value) :obj))
    (error 'incompatible-server-response
           :message (format nil "Operation ~a did not return a JSON object" operation-id)
           :operation-id operation-id))
  value)

(defun health (client &key request-options)
  (request-health client :request-options request-options))

(defun server-info (client &key request-options)
  (request-server-info client :request-options request-options))

(defun fetch-openapi-document (client &key request-options)
  (request-openapi-document client :request-options request-options))

(defun fetch-client-manifest (client &key request-options)
  (request-client-manifest client :request-options request-options))

(defun login (client username password &key request-options)
  (multiple-value-bind (value response)
      (request-auth-login
       client
       :body (jsown:new-js
               ("username" username)
               ("password" password))
       :request-options request-options)
    (let ((object (require-json-object-response value "auth.login")))
      (values
       (make-login-result
        :api-key (jsown:val object "api_key")
        :credential (jsown:val object "credential")
        :user (jsown:val object "user")
        :correlation-id (jsown:val-safe object "correlation_id"))
       response))))

(defun bootstrap-credential (client bootstrap-secret
                             &key (owner "bootstrap-administrator") request-options)
  (multiple-value-bind (value response)
      (request-auth-bootstrap
       client
       :headers (list (cons "X-Star-Bootstrap-Secret" bootstrap-secret))
       :body (jsown:new-js ("owner" owner))
       :request-options request-options)
    (let ((object (require-json-object-response value "auth.bootstrap")))
      (values
       (make-credential-secret-result
        :api-key (jsown:val object "api_key")
        :credential (jsown:val object "credential")
        :correlation-id (jsown:val-safe object "correlation_id"))
       response))))

(defun auth-context (client &key request-options)
  (request-auth-context client :request-options request-options))

(defun create-user (client username password scopes
                    &key (principal-type "user")
                      (must-change-password t)
                      request-options)
  (request-auth-create-user
   client
   :body (jsown:new-js
           ("username" username)
           ("password" password)
           ("principal_type" principal-type)
           ("scopes" scopes)
           ("must_change_password" must-change-password))
   :request-options request-options))

(defun list-users (client &key request-options)
  (request-auth-list-users client :request-options request-options))

(defun reset-user-password (client username password
                            &key (must-change-password t) request-options)
  (request-auth-reset-user-password
   client
   :path-parameters (list (cons "username" username))
   :body (jsown:new-js
           ("password" password)
           ("must_change_password" must-change-password))
   :request-options request-options))

(defun change-password (client current-password new-password &key request-options)
  (request-auth-change-password
   client
   :body (jsown:new-js
           ("current_password" current-password)
           ("new_password" new-password))
   :request-options request-options))

(defun create-credential (client owner principal-type scopes
                          &key expires-in-seconds request-options)
  (let ((body
          (jsown:new-js
            ("owner" owner)
            ("principal_type" principal-type)
            ("scopes" scopes))))
    (when expires-in-seconds
      (setf (jsown:val body "expires_in_seconds") expires-in-seconds))
    (multiple-value-bind (value response)
        (request-auth-create-credential
         client :body body :request-options request-options)
      (let ((object
              (require-json-object-response value "auth.credentials.create")))
        (values
         (make-credential-secret-result
          :api-key (jsown:val object "api_key")
          :credential (jsown:val object "credential")
          :correlation-id (jsown:val-safe object "correlation_id"))
         response)))))

(defun list-credentials (client &key request-options)
  (request-auth-list-credentials client :request-options request-options))

(defun rotate-credential (client credential-id &key (overlap-seconds 0) request-options)
  (multiple-value-bind (value response)
      (request-auth-rotate-credential
       client
       :path-parameters (list (cons "credential-id" credential-id))
       :body (jsown:new-js ("overlap_seconds" overlap-seconds))
       :request-options request-options)
    (let ((object
            (require-json-object-response value "auth.credentials.rotate")))
      (values
       (make-credential-secret-result
        :api-key (jsown:val object "api_key")
        :credential (jsown:val object "credential")
        :correlation-id (jsown:val-safe object "correlation_id"))
       response))))

(defun revoke-credential (client credential-id &key request-options)
  (request-auth-revoke-credential
   client
   :path-parameters (list (cons "credential-id" credential-id))
   :request-options request-options))

(defun disable-credential (client credential-id &key request-options)
  (request-auth-disable-credential
   client
   :path-parameters (list (cons "credential-id" credential-id))
   :request-options request-options))
