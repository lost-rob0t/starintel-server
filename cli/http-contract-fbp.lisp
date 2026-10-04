(in-package :star.http.contract)

(defun query-parameter-manifest-object (parameter)
  (json-object
   (cons "name" (getf parameter :name))
   (cons "required" (and (getf parameter :required) t))
   (cons "schema" (or (getf parameter :schema) (string-schema)))))

(defun operation-manifest-object (operation)
  "Lossless client projection of one canonical HTTP operation."
  (json-object
   (cons "operation_id" (http-operation-id operation))
   (cons "client_name" (http-operation-client-name operation))
   (cons "summary" (http-operation-summary operation))
   (cons "tags" (or (http-operation-tags operation) nil))
   (cons "method" (string-downcase (symbol-name (http-operation-method operation))))
   (cons "path" (http-operation-path operation))
   (cons "openapi_path" (openapi-path operation))
   (cons "authority" (string-downcase (symbol-name (http-operation-authority operation))))
   (cons "scopes" (or (http-operation-scopes operation) nil))
   (cons "path_parameters" (or (http-operation-path-parameters operation) nil))
   (cons "query_parameters"
         (mapcar #'query-parameter-manifest-object
                 (http-operation-query-parameters operation)))
   (cons "request_schema" (or (http-operation-request-schema operation) :null))
   (cons "responses" (mapcar #'response-manifest-object
                              (http-operation-responses operation)))
   (cons "idempotency" (or (http-operation-idempotency operation) :null))))

(defun schema-property-pairs (schema)
  (let ((properties (and (listp schema) (jsown:val-safe schema "properties"))))
    (if (and (consp properties) (eq (first properties) :obj))
        (rest properties)
        nil)))

(defun schema-required-fields (schema)
  (or (and (listp schema) (jsown:val-safe schema "required")) nil))

(defun request-input-ports (operation)
  (let* ((schema (http-operation-request-schema operation))
         (required (schema-required-fields schema))
         (ports
           (loop for (name . property-schema) in (schema-property-pairs schema)
                 collect (json-object
                          (cons "name" name)
                          (cons "source" "body")
                          (cons "required" (and (member name required :test #'string=) t))
                          (cons "schema" property-schema)))))
    (unless ports
      (when schema
        (push (json-object (cons "name" "request")
                           (cons "source" "body")
                           (cons "required" t)
                           (cons "schema" schema))
              ports)))
    (dolist (name (http-operation-path-parameters operation))
      (push (json-object (cons "name" name)
                         (cons "source" "path")
                         (cons "required" t)
                         (cons "schema" (string-schema :min-length 1)))
            ports))
    (dolist (parameter (http-operation-query-parameters operation))
      (push (json-object (cons "name" (getf parameter :name))
                         (cons "source" "query")
                         (cons "required" (and (getf parameter :required) t))
                         (cons "schema" (or (getf parameter :schema) (string-schema))))
            ports))
    ;; A node without a required input is otherwise perpetually runnable in an
    ;; FBP scheduler.  Keep optional request inputs, but require one explicit
    ;; control packet to start the operation.
    (unless (some (lambda (port) (jsown:val port "required")) ports)
      (push (json-object (cons "name" "trigger")
                         (cons "source" "control")
                         (cons "required" t)
                         (cons "schema" (generic-object-schema)))
            ports))
    (nreverse ports)))

(defun response-output-ports (operation)
  (loop for response in (http-operation-responses operation)
        for status = (getf response :status)
        when (<= 200 status 299)
          collect (json-object
                   (cons "name" (format nil "status-~D" status))
                   (cons "status" status)
                   (cons "schema" (or (getf response :schema)
                                      (generic-object-schema))))))

(defun operation-fbp-node-object (operation)
  "Project one canonical HTTP operation into a typed FBP component descriptor."
  (json-object
   (cons "id" (format nil "starintel.operation/~A" (http-operation-id operation)))
   (cons "component" "starintel.operation")
   (cons "operation_id" (http-operation-id operation))
   (cons "client_name" (http-operation-client-name operation))
   (cons "summary" (http-operation-summary operation))
   (cons "tags" (or (http-operation-tags operation) nil))
   (cons "method" (string-downcase (symbol-name (http-operation-method operation))))
   (cons "path" (http-operation-path operation))
   (cons "openapi_path" (openapi-path operation))
   (cons "path_parameters" (or (http-operation-path-parameters operation) nil))
   (cons "query_parameters"
         (mapcar #'query-parameter-manifest-object
                 (http-operation-query-parameters operation)))
   (cons "request_schema" (or (http-operation-request-schema operation) :null))
   (cons "responses" (mapcar #'response-manifest-object
                              (http-operation-responses operation)))
   (cons "label" (http-operation-summary operation))
   (cons "category"
         (if (http-operation-tags operation)
             (format nil "StarIntel · ~A" (first (http-operation-tags operation)))
             "StarIntel API"))
   (cons "inputs" (request-input-ports operation))
   (cons "outputs" (response-output-ports operation))
   (cons "authority" (string-downcase (symbol-name (http-operation-authority operation))))
   (cons "scopes" (or (http-operation-scopes operation) nil))
   (cons "idempotency" (or (http-operation-idempotency operation) :null))
   (cons "config_schema"
         (object-schema
          (list
           (cons "operation" (json-object (cons "const" (http-operation-id operation))))
           (cons "credentialReference"
                 (json-object
                  (cons "type" "string")
                  (cons "pattern" "^credential:[A-Za-z0-9_.-]+$")
                  (cons "description" "Reference only; secret values are forbidden."))))
          :required '("operation" "credentialReference")
          :additional-properties nil))))

(defun fbp-node-catalog-document ()
  "Typed FBP descriptors derived only from the canonical HTTP operation registry."
  (json-object
   (cons "schema" "starintel-fbp-node-catalog-v1")
   (cons "nodes" (mapcar #'operation-fbp-node-object (all-http-operations)))))

(defun fbp-node-catalog-json ()
  "Serialize the canonical HTTP-operation FBP projection as JSON."
  (jsown:to-json (fbp-node-catalog-document)))

(defun client-manifest-document ()
  "The full client manifest and its lossless FBP projection."
  (json-object
   (cons "schema" "starintel-client-manifest-v1")
   (cons "openapi" "3.1.2")
   (cons "operations" (mapcar #'operation-manifest-object (all-http-operations)))
   (cons "fbp_nodes" (mapcar #'operation-fbp-node-object (all-http-operations)))))

(defun client-manifest-json ()
  "Serialize the full client manifest and its lossless FBP projection as JSON."
  (jsown:to-json (client-manifest-document)))
