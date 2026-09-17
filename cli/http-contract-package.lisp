(uiop:define-package :star.http.contract
  (:use :cl)
  (:export
   #:http-operation
   #:http-operation-id
   #:http-operation-client-name
   #:http-operation-method
   #:http-operation-path
   #:http-operation-summary
   #:http-operation-tags
   #:http-operation-authority
   #:http-operation-scopes
   #:http-operation-path-parameters
   #:http-operation-query-parameters
   #:http-operation-request-schema
   #:http-operation-responses
   #:http-operation-idempotency
   #:all-http-operations
   #:find-http-operation
   #:openapi-document
   #:openapi-json
   #:client-manifest-document
   #:client-manifest-json
   #:openapi-path
   #:operation-request-symbol-name
   #:operation-fbp-node-object
   #:fbp-node-catalog-document
   #:fbp-node-catalog-json))

(in-package :star.http.contract)
