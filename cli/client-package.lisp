(uiop:define-package :starintel-gserver-client
  (:nicknames :star.api.client)
  (:use :cl)
  (:export
   ;; Client configuration and transport.
   #:star-client
   #:make-star-client
   #:client-with-api-key
   #:base-url
   #:star-client-transport
   #:client-transport
   #:dexador-transport
   #:function-transport
   #:make-function-transport
   #:perform-client-request
   #:client-request
   #:client-request-method
   #:client-request-uri
   #:client-request-headers
   #:client-request-body
   #:client-request-timeout-ms
   #:client-request-operation-id
   #:client-response
   #:client-response-status
   #:client-response-headers
   #:client-response-body
   #:client-response-uri
   #:client-response-correlation-id
   #:client-response-content-type
   #:request-options
   #:make-request-options
   #:request-options-timeout-ms
   #:request-options-correlation-id
   #:request-options-idempotency-key
   #:request-options-headers
   ;; Authentication and secret-bearing results.
   #:authentication-provider
   #:anonymous-authentication
   #:bearer-authentication
   #:login-result
   #:login-result-api-key
   #:login-result-credential
   #:login-result-user
   #:login-result-correlation-id
   #:credential-secret-result
   #:credential-secret-result-api-key
   #:credential-secret-result-credential
   #:credential-secret-result-correlation-id
   ;; Conditions.
   #:star-client-error
   #:client-configuration-error
   #:client-request-error
   #:client-transport-error
   #:client-timeout-error
   #:client-connection-error
   #:client-protocol-error
   #:malformed-server-response
   #:incompatible-server-response
   #:client-http-error
   #:client-authentication-error
   #:client-authorization-error
   #:client-not-found-error
   #:client-conflict-error
   #:client-validation-error
   #:client-rate-limit-error
   #:client-server-unavailable-error
   #:client-http-error-status
   #:client-http-error-code
   #:client-http-error-message
   #:client-http-error-correlation-id
   #:client-http-error-operation-id
   ;; Operation runtime.
   #:make-url
   #:call-operation
   #:api-request
   ;; Contracted convenience operations.
   #:health
   #:server-info
   #:fetch-openapi-document
   #:fetch-client-manifest
   #:login
   #:bootstrap-credential
   #:auth-context
   #:create-user
   #:list-users
   #:reset-user-password
   #:change-password
   #:create-credential
   #:list-credentials
   #:rotate-credential
   #:revoke-credential
   #:disable-credential
   ;; Existing compatibility operations.
   #:get-targets
   #:new-target
   #:submit-document
   #:bulk-submit
   #:get-document
   #:fts
   #:messages-by-user
   #:messages-by-channel
   #:messages-by-platform
   #:messages-by-group
   #:social-posts-by-user
   #:dataset-size
   #:do-view
   #:groups))

(in-package :star.api.client)
