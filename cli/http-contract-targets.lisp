(in-package :star.http.contract)

(defparameter +target-create-request-schema+
  (object-schema
   (list
    (cons "actor" (string-schema :min-length 1))
    (cons "target" (string-schema :min-length 1))
    (cons "dataset" (string-schema :min-length 1))
    (cons "delay" (integer-schema :minimum 1))
    (cons "recurring" (boolean-schema))
    (cons "options" (array-schema (generic-object-schema)))
    (cons "idempotency_key" (string-schema :min-length 1)))
   :required '("actor" "target" "dataset" "idempotency_key")
   :additional-properties nil
   :description "Generic StarIntel target submission request."))

(defparameter +target-create-receipt-schema+
  (object-schema
   (list
    (cons "status" (string-schema))
    (cons "target_id" (string-schema :min-length 1))
    (cons "request_id" (string-schema :min-length 1))
    (cons "correlation_id" (string-schema :min-length 1)))
   :required '("status" "target_id" "request_id" "correlation_id")
   :additional-properties nil
   :description "Narrow target acceptance receipt."))

(upsert-http-operation
 (make-http-operation
  :id "targets.create"
  :client-name "target-create"
  :method :post
  :path "/api/v1/targets"
  :summary "Create or safely retry a canonical StarIntel target"
  :tags '("targets")
  :authority :authenticated
  :scopes '("targets:dispatch")
  :request-schema +target-create-request-schema+
  :idempotency "principal-bound idempotency_key"
  :responses
  (append
   (list
    (response 201 "Target accepted." +target-create-receipt-schema+)
    (response 200 "Equivalent target request already accepted."
              +target-create-receipt-schema+))
   (standard-errors))))