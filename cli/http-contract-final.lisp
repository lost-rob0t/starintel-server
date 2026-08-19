(in-package :star.http.contract)

;; Final normalization layer kept separate so generated consumers and the
;; server load the same corrected contract object graph.

(setf +credential-metadata-schema+
      (object-schema
       (list
        (cons "credential_id" (string-schema))
        (cons "owner" (string-schema))
        (cons "principal_type" (string-schema))
        (cons "scopes" (array-schema (string-schema)))
        (cons "status" (string-schema))
        (cons "created_at" (integer-schema :minimum 0)))
       :required '("credential_id" "owner" "principal_type" "scopes" "status")
       :additional-properties t)
      +login-response-schema+
      (object-schema
       (list
        (cons "api_key" (string-schema :secret t))
        (cons "credential" +credential-metadata-schema+)
        (cons "user" +user-metadata-schema+)
        (cons "correlation_id" (string-schema)))
       :required '("api_key" "credential" "user" "correlation_id")))

(defun refresh-operation-response-schema (operation-id schema)
  (let* ((operation (find-http-operation operation-id))
         (success (first (http-operation-responses operation))))
    (setf (getf success :schema) schema)
    operation))

(refresh-operation-response-schema "auth.login" +login-response-schema+)
(refresh-operation-response-schema "auth.bootstrap" (credential-secret-schema))
(refresh-operation-response-schema "auth.credentials.create" (credential-secret-schema))
(refresh-operation-response-schema "auth.credentials.list"
                                   (array-schema +credential-metadata-schema+))
(refresh-operation-response-schema "auth.credentials.rotate" (credential-secret-schema))
(refresh-operation-response-schema "auth.credentials.revoke" (credential-status-schema))
(refresh-operation-response-schema "auth.credentials.disable" (credential-status-schema))

(defun operation-openapi-object (operation)
  (let* ((object
           (json-object
            (cons "operationId" (http-operation-client-name operation))
            (cons "summary" (http-operation-summary operation))
            (cons "tags" (http-operation-tags operation))
            (cons "x-starintel-operation-id" (http-operation-id operation))
            (cons "x-starintel-authority"
                  (string-downcase (symbol-name (http-operation-authority operation))))
            (cons "x-starintel-scopes" (or (http-operation-scopes operation) nil))))
         (parameters
           (loop for parameter in (http-operation-path-parameters operation)
                 collect
                 (json-object
                  (cons "name" parameter)
                  (cons "in" "path")
                  (cons "required" t)
                  (cons "schema" (string-schema :min-length 1))))))
    (when (eq :bootstrap (http-operation-authority operation))
      (setf parameters
            (append parameters
                    (list
                     (json-object
                      (cons "name" "X-Star-Bootstrap-Secret")
                      (cons "in" "header")
                      (cons "required" t)
                      (cons "schema" (string-schema :min-length 1 :secret t)))))))
    (when parameters
      (setf (jsown:val object "parameters") parameters))
    (setf (jsown:val object "security")
          (if (member (http-operation-authority operation) '(:public :bootstrap))
              nil
              (list (json-object (cons "bearerAuth" nil)))))
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
