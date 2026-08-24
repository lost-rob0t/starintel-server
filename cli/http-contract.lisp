(in-package :star.http.contract)

(defstruct (http-operation
            (:constructor make-http-operation
                (&key id client-name method path summary tags authority scopes
                      path-parameters request-schema responses idempotency)))
  id
  client-name
  method
  path
  summary
  tags
  authority
  scopes
  path-parameters
  request-schema
  responses
  idempotency)

(defun json-object (&rest pairs)
  (let ((object (list :obj)))
    (dolist (pair pairs object)
      (setf (jsown:val object (car pair)) (cdr pair)))))

(defun string-schema (&key min-length format description secret)
  (let ((schema (json-object (cons "type" "string"))))
    (when min-length
      (setf (jsown:val schema "minLength") min-length))
    (when format
      (setf (jsown:val schema "format") format))
    (when description
      (setf (jsown:val schema "description") description))
    (when secret
      (setf (jsown:val schema "writeOnly") t
            (jsown:val schema "x-starintel-secret") t))
    schema))

(defun integer-schema (&key minimum description)
  (let ((schema (json-object (cons "type" "integer"))))
    (when minimum
      (setf (jsown:val schema "minimum") minimum))
    (when description
      (setf (jsown:val schema "description") description))
    schema))

(defun boolean-schema (&key description)
  (let ((schema (json-object (cons "type" "boolean"))))
    (when description
      (setf (jsown:val schema "description") description))
    schema))

(defun array-schema (items &key description)
  (let ((schema (json-object
                 (cons "type" "array")
                 (cons "items" items))))
    (when description
      (setf (jsown:val schema "description") description))
    schema))

(defun object-schema (properties &key required additional-properties description)
  (let ((property-object (list :obj))
        (schema (json-object (cons "type" "object"))))
    (dolist (property properties)
      (setf (jsown:val property-object (car property)) (cdr property)))
    (setf (jsown:val schema "properties") property-object)
    (when required
      (setf (jsown:val schema "required") required))
    (setf (jsown:val schema "additionalProperties")
          (if additional-properties t nil))
    (when description
      (setf (jsown:val schema "description") description))
    schema))

(defun generic-object-schema (&optional description)
  (let ((schema (json-object
                 (cons "type" "object")
                 (cons "additionalProperties" t))))
    (when description
      (setf (jsown:val schema "description") description))
    schema))

(defun response (status description &optional schema)
  (list :status status :description description :schema schema))

(defparameter +error-schema+
  (object-schema
   (list
    (cons "status" (string-schema))
    (cons "msg" (string-schema))
    (cons "code" (string-schema))
    (cons "correlation_id" (string-schema)))
   :required '("status" "msg" "correlation_id")
   :additional-properties t
   :description "StarIntel error envelope."))

(defparameter +user-metadata-schema+
  (object-schema
   (list
    (cons "username" (string-schema))
    (cons "principal_type" (string-schema))
    (cons "scopes" (array-schema (string-schema)))
    (cons "status" (string-schema))
    (cons "created_at" (integer-schema :minimum 0))
    (cons "password_updated_at" (integer-schema :minimum 0))
    (cons "must_change_password" (boolean-schema)))
   :required '("username" "principal_type" "scopes" "status")
   :additional-properties t))

(defparameter +credential-metadata-schema+
  (object-schema
   (list
    (cons "id" (string-schema))
    (cons "owner" (string-schema))
    (cons "principal_type" (string-schema))
    (cons "scopes" (array-schema (string-schema)))
    (cons "status" (string-schema))
    (cons "created_at" (integer-schema :minimum 0)))
   :required '("id" "owner" "principal_type" "scopes" "status")
   :additional-properties t))

(defparameter +login-request-schema+
  (object-schema
   (list
    (cons "username" (string-schema :min-length 1))
    (cons "password" (string-schema :min-length 1 :secret t)))
   :required '("username" "password")))

(defparameter +login-response-schema+
  (object-schema
   (list
    (cons "api_key" (string-schema :secret t))
    (cons "credential" +credential-metadata-schema+)
    (cons "user" +user-metadata-schema+)
    (cons "correlation_id" (string-schema)))
   :required '("api_key" "credential" "user" "correlation_id")))

(defparameter +create-user-request-schema+
  (object-schema
   (list
    (cons "username" (string-schema :min-length 1))
    (cons "password" (string-schema :min-length 1 :secret t))
    (cons "principal_type" (string-schema :min-length 1))
    (cons "scopes" (array-schema (string-schema :min-length 1)))
    (cons "must_change_password" (boolean-schema)))
   :required '("username" "password" "scopes")))

(defparameter +reset-password-request-schema+
  (object-schema
   (list
    (cons "password" (string-schema :min-length 1 :secret t))
    (cons "must_change_password" (boolean-schema)))
   :required '("password")))

(defparameter +change-password-request-schema+
  (object-schema
   (list
    (cons "current_password" (string-schema :min-length 1 :secret t))
    (cons "new_password" (string-schema :min-length 1 :secret t)))
   :required '("current_password" "new_password")))

(defparameter +create-credential-request-schema+
  (object-schema
   (list
    (cons "owner" (string-schema :min-length 1))
    (cons "principal_type" (string-schema :min-length 1))
    (cons "scopes" (array-schema (string-schema :min-length 1)))
    (cons "expires_in_seconds" (integer-schema :minimum 1)))
   :required '("owner" "principal_type" "scopes")))

(defparameter +rotate-credential-request-schema+
  (object-schema
   (list
    (cons "overlap_seconds" (integer-schema :minimum 0)))
   :required '("overlap_seconds")))

(defparameter +bootstrap-request-schema+
  (object-schema
   (list (cons "owner" (string-schema :min-length 1)))
   :additional-properties nil))

(defun standard-errors ()
  (list
   (response 400 "Malformed or invalid request." +error-schema+)
   (response 401 "Authentication failed." +error-schema+)
   (response 403 "Authorization failed." +error-schema+)
   (response 404 "Resource not found." +error-schema+)
   (response 409 "Resource conflict." +error-schema+)
   (response 422 "Request validation failed." +error-schema+)
   (response 503 "Authentication store unavailable." +error-schema+)))

(defun user-status-schema ()
  (object-schema
   (list
    (cons "status" (string-schema))
    (cons "msg" (string-schema))
    (cons "user" +user-metadata-schema+)
    (cons "correlation_id" (string-schema)))
   :required '("status" "msg" "user" "correlation_id")))

(defun credential-secret-schema ()
  (object-schema
   (list
    (cons "api_key" (string-schema :secret t))
    (cons "credential" +credential-metadata-schema+)
    (cons "correlation_id" (string-schema)))
   :required '("api_key" "credential" "correlation_id")))

(defun credential-status-schema ()
  (object-schema
   (list
    (cons "status" (string-schema))
    (cons "msg" (string-schema))
    (cons "credential" +credential-metadata-schema+)
    (cons "correlation_id" (string-schema)))
   :required '("status" "msg" "credential" "correlation_id")))

(defparameter *http-operations*
  (list
   (make-http-operation
    :id "health.get"
    :client-name "health"
    :method :get
    :path "/health"
    :summary "Process health check"
    :tags '("system")
    :authority :public
    :responses (list (response 200 "Server process is healthy." (generic-object-schema))))
   (make-http-operation
    :id "server.get"
    :client-name "server-info"
    :method :get
    :path "/"
    :summary "Server metadata"
    :tags '("system")
    :authority :public
    :responses (list (response 200 "Server metadata." (generic-object-schema))))
   (make-http-operation
    :id "schema.openapi.get"
    :client-name "openapi-document"
    :method :get
    :path "/openapi.json"
    :summary "OpenAPI description of the contracted HTTP surface"
    :tags '("schema")
    :authority :public
    :responses (list (response 200 "OpenAPI 3.1 document." (generic-object-schema))))
   (make-http-operation
    :id "schema.client-manifest.get"
    :client-name "client-manifest"
    :method :get
    :path "/client-manifest.json"
    :summary "Normalized client generation manifest"
    :tags '("schema")
    :authority :public
    :responses (list (response 200 "StarIntel client manifest." (generic-object-schema))))
   (make-http-operation
    :id "auth.login"
    :client-name "auth-login"
    :method :post
    :path "/auth/login"
    :summary "Authenticate a human user and mint a session API key"
    :tags '("auth")
    :authority :public
    :request-schema +login-request-schema+
    :responses (append
                (list (response 200 "Authenticated session." +login-response-schema+))
                (standard-errors)))
   (make-http-operation
    :id "auth.bootstrap"
    :client-name "auth-bootstrap"
    :method :post
    :path "/auth/bootstrap"
    :summary "Bootstrap the first API-key administrator"
    :tags '("auth")
    :authority :bootstrap
    :request-schema +bootstrap-request-schema+
    :responses (append
                (list (response 201 "Bootstrap credential created." (credential-secret-schema)))
                (standard-errors)))
   (make-http-operation
    :id "auth.context.get"
    :client-name "auth-context"
    :method :get
    :path "/auth/context"
    :summary "Inspect the authenticated request context"
    :tags '("auth")
    :authority :authenticated
    :responses (append
                (list (response 200 "Authenticated principal context." (generic-object-schema)))
                (standard-errors)))
   (make-http-operation
    :id "auth.users.create"
    :client-name "auth-create-user"
    :method :post
    :path "/auth/users"
    :summary "Create a human user"
    :tags '("auth" "users")
    :authority :administrator
    :scopes '("admin")
    :request-schema +create-user-request-schema+
    :responses (append
                (list (response 201 "User created." (user-status-schema)))
                (standard-errors)))
   (make-http-operation
    :id "auth.users.list"
    :client-name "auth-list-users"
    :method :get
    :path "/auth/users"
    :summary "List human users"
    :tags '("auth" "users")
    :authority :administrator
    :scopes '("admin")
    :responses (append
                (list (response 200 "User metadata list."
                                (array-schema +user-metadata-schema+)))
                (standard-errors)))
   (make-http-operation
    :id "auth.users.password.reset"
    :client-name "auth-reset-user-password"
    :method :post
    :path "/auth/users/:username/password"
    :summary "Administratively reset a human user's password"
    :tags '("auth" "users")
    :authority :administrator
    :scopes '("admin")
    :path-parameters '("username")
    :request-schema +reset-password-request-schema+
    :responses (append
                (list (response 200 "Password reset." (user-status-schema)))
                (standard-errors)))
   (make-http-operation
    :id "auth.password.change"
    :client-name "auth-change-password"
    :method :post
    :path "/auth/password"
    :summary "Change the authenticated human user's password"
    :tags '("auth" "users")
    :authority :authenticated
    :request-schema +change-password-request-schema+
    :responses (append
                (list (response 200 "Password changed." (user-status-schema)))
                (standard-errors)))
   (make-http-operation
    :id "auth.credentials.create"
    :client-name "auth-create-credential"
    :method :post
    :path "/auth/credentials"
    :summary "Create an API credential"
    :tags '("auth" "credentials")
    :authority :administrator
    :scopes '("admin")
    :request-schema +create-credential-request-schema+
    :responses (append
                (list (response 201 "Credential created." (credential-secret-schema)))
                (standard-errors)))
   (make-http-operation
    :id "auth.credentials.list"
    :client-name "auth-list-credentials"
    :method :get
    :path "/auth/credentials"
    :summary "List API credentials"
    :tags '("auth" "credentials")
    :authority :administrator
    :scopes '("admin")
    :responses (append
                (list (response 200 "Credential metadata list."
                                (array-schema +credential-metadata-schema+)))
                (standard-errors)))
   (make-http-operation
    :id "auth.credentials.rotate"
    :client-name "auth-rotate-credential"
    :method :post
    :path "/auth/credentials/:credential-id/rotate"
    :summary "Rotate an API credential"
    :tags '("auth" "credentials")
    :authority :administrator
    :scopes '("admin")
    :path-parameters '("credential-id")
    :request-schema +rotate-credential-request-schema+
    :responses (append
                (list (response 201 "Credential rotated." (credential-secret-schema)))
                (standard-errors)))
   (make-http-operation
    :id "auth.credentials.revoke"
    :client-name "auth-revoke-credential"
    :method :post
    :path "/auth/credentials/:credential-id/revoke"
    :summary "Revoke an API credential"
    :tags '("auth" "credentials")
    :authority :administrator
    :scopes '("admin")
    :path-parameters '("credential-id")
    :responses (append
                (list (response 200 "Credential revoked." (credential-status-schema)))
                (standard-errors)))
   (make-http-operation
    :id "auth.credentials.disable"
    :client-name "auth-disable-credential"
    :method :post
    :path "/auth/credentials/:credential-id/disable"
    :summary "Disable an API credential"
    :tags '("auth" "credentials")
    :authority :administrator
    :scopes '("admin")
    :path-parameters '("credential-id")
    :responses (append
                (list (response 200 "Credential disabled." (credential-status-schema)))
                (standard-errors)))))

(defun all-http-operations ()
  (copy-list *http-operations*))

(defun find-http-operation (operation-id &key (errorp t))
  (or (find operation-id *http-operations*
            :key #'http-operation-id
            :test #'string=)
      (when errorp
        (error "Unknown StarIntel HTTP operation: ~a" operation-id))))

(defun operation-request-symbol-name (operation)
  (format nil "REQUEST-~a"
          (string-upcase (http-operation-client-name operation))))

(defun replace-path-parameter (path parameter)
  (let* ((needle (format nil ":~a" parameter))
         (position (search needle path :test #'char-equal)))
    (if position
        (concatenate 'string
                     (subseq path 0 position)
                     "{"
                     parameter
                     "}"
                     (subseq path (+ position (length needle))))
        path)))

(defun openapi-path (operation)
  (reduce #'replace-path-parameter
          (http-operation-path-parameters operation)
          :initial-value (http-operation-path operation)))

(defun response-openapi-object (response)
  (let* ((description (getf response :description))
         (schema (getf response :schema))
         (object (json-object (cons "description" description))))
    (when schema
      (setf (jsown:val object "content")
            (json-object
             (cons "application/json"
                   (json-object (cons "schema" schema))))))
    object))

(defun operation-openapi-object (operation)
  (let ((object
          (json-object
           (cons "operationId" (http-operation-client-name operation))
           (cons "summary" (http-operation-summary operation))
           (cons "tags" (http-operation-tags operation))
           (cons "x-starintel-authority"
                 (string-downcase (symbol-name (http-operation-authority operation))))
           (cons "x-starintel-scopes" (or (http-operation-scopes operation) nil)))))
    (setf (jsown:val object "security")
          (if (member (http-operation-authority operation) '(:public :bootstrap))
              nil
              (list (json-object (cons "bearerAuth" nil)))))
    (when (http-operation-path-parameters operation)
      (setf (jsown:val object "parameters")
            (loop for parameter in (http-operation-path-parameters operation)
                  collect
                  (json-object
                   (cons "name" parameter)
                   (cons "in" "path")
                   (cons "required" t)
                   (cons "schema" (string-schema :min-length 1))))))
    (when (eq :bootstrap (http-operation-authority operation))
      (push (json-object
             (cons "name" "X-Star-Bootstrap-Secret")
             (cons "in" "header")
             (cons "required" t)
             (cons "schema" (string-schema :min-length 1 :secret t)))
            (jsown:val object "parameters")))
    (when (http-operation-request-schema operation)
      (setf (jsown:val object "requestBody")
            (json-object
             (cons "required" t)
             (cons "content"
                   (json-object
                    (cons "application/json"
                          (json-object
                           (cons "schema"
                                 (http-operation-request-schema operation)))))))))
    (let ((responses (list :obj)))
      (dolist (response (http-operation-responses operation))
        (setf (jsown:val responses (princ-to-string (getf response :status)))
              (response-openapi-object response)))
      (setf (jsown:val object "responses") responses))
    object))

(defun openapi-document ()
  (let ((paths (list :obj)))
    (dolist (operation *http-operations*)
      (let* ((path (openapi-path operation))
             (path-item (or (jsown:val-safe paths path)
                            (let ((object (list :obj)))
                              (setf (jsown:val paths path) object)
                              object))))
        (setf (jsown:val path-item
                         (string-downcase
                          (symbol-name (http-operation-method operation))))
              (operation-openapi-object operation))))
    (json-object
     (cons "openapi" "3.1.2")
     (cons "info"
           (json-object
            (cons "title" "StarIntel GServer HTTP API")
            (cons "version" "1.0.0")
            (cons "description"
                  "Machine-readable contracted StarIntel control-plane HTTP surface.")))
     (cons "paths" paths)
     (cons "components"
           (json-object
            (cons "securitySchemes"
                  (json-object
                   (cons "bearerAuth"
                         (json-object
                          (cons "type" "http")
                          (cons "scheme" "bearer")
                          (cons "bearerFormat" "StarIntel API key"))))))))))

(defun openapi-json ()
  (jsown:to-json (openapi-document)))

(defun response-manifest-object (response)
  (json-object
   (cons "status" (getf response :status))
   (cons "description" (getf response :description))
   (cons "schema" (or (getf response :schema) :null))))

(defun operation-manifest-object (operation)
  (json-object
   (cons "operation_id" (http-operation-id operation))
   (cons "client_name" (http-operation-client-name operation))
   (cons "method"
         (string-downcase (symbol-name (http-operation-method operation))))
   (cons "path" (http-operation-path operation))
   (cons "openapi_path" (openapi-path operation))
   (cons "authority"
         (string-downcase (symbol-name (http-operation-authority operation))))
   (cons "scopes" (or (http-operation-scopes operation) nil))
   (cons "path_parameters"
         (or (http-operation-path-parameters operation) nil))
   (cons "request_schema"
         (or (http-operation-request-schema operation) :null))
   (cons "responses"
         (mapcar #'response-manifest-object
                 (http-operation-responses operation)))
   (cons "idempotency"
         (or (http-operation-idempotency operation) :null))))

(defun client-manifest-document ()
  (json-object
   (cons "schema" "starintel-client-manifest-v1")
   (cons "openapi" "3.1.2")
   (cons "operations"
         (mapcar #'operation-manifest-object *http-operations*))))

(defun client-manifest-json ()
  (jsown:to-json (client-manifest-document)))
