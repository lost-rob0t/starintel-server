(in-package :star-server-tests)

(def-suite http-target-v1-tests
  :description "Versioned target creation, authorization and idempotency contract")

(in-suite http-target-v1-tests)

(defun make-v1-target-request (&key
                                 (actor "subfinder")
                                 (target "example.org")
                                 (dataset "star-intel")
                                 (delay 1)
                                 (recurring nil)
                                 (options #())
                                 (idempotency-key "bixby-draft-123"))
  (jsown:new-js
    ("actor" actor)
    ("target" target)
    ("dataset" dataset)
    ("delay" delay)
    ("recurring" (if recurring :true :false))
    ("options" options)
    ("idempotency_key" idempotency-key)))

(test v1-target-create-is-a-generic-target-dispatch-capability
  (is (string= "targets:dispatch"
               (star.frontends.http-api::route-action
                :post "/api/v1/targets")))
  (let ((operation
          (star.http.contract:find-http-operation "targets.create")))
    (is (eq :post (star.http.contract:http-operation-method operation)))
    (is (string= "/api/v1/targets"
                 (star.http.contract:http-operation-path operation)))
    (is (equal '("targets:dispatch")
               (star.http.contract:http-operation-scopes operation)))))

(test v1-target-idempotency-is-principal-bound-and-deterministic
  (let* ((request (make-v1-target-request))
         (first
           (star.frontends.http-api::target-v1-document-from-request
            request "human:alice"))
         (retry
           (star.frontends.http-api::target-v1-document-from-request
            request "human:alice"))
         (other-user
           (star.frontends.http-api::target-v1-document-from-request
            request "human:bob")))
    (is (string= (jsown:val first "_id")
                 (jsown:val retry "_id")))
    (is (string= (jsown:val first "schedule_id")
                 (jsown:val retry "schedule_id")))
    (is (not (string= (jsown:val first "_id")
                      (jsown:val other-user "_id"))))
    (is (string= "target" (jsown:val first "dtype")))
    (is (string= starintel:+starintel-doc-version+
                 (jsown:val first "schema_version")))))

(test v1-target-request-rejects-missing-idempotency-and-invalid-delay
  (let ((missing-key (make-v1-target-request))
        (zero-delay (make-v1-target-request :delay 0)))
    (jsown:remkey missing-key "idempotency_key")
    (let ((missing-condition
            (capture-http-input-error
             (lambda ()
               (star.frontends.http-api::target-v1-document-from-request
                missing-key "human:alice"))))
          (delay-condition
            (capture-http-input-error
             (lambda ()
               (star.frontends.http-api::target-v1-document-from-request
                zero-delay "human:alice")))))
      (is (= 400
             (star.frontends.http-api:http-input-error-status
              missing-condition)))
      (is (string= "idempotency_key_required"
                   (star.frontends.http-api:http-input-error-code
                    missing-condition)))
      (is (= 422
             (star.frontends.http-api:http-input-error-status
              delay-condition)))
      (is (string= "invalid_target_delay"
                   (star.frontends.http-api:http-input-error-code
                    delay-condition))))))

(test durable-target-fingerprint-detects-content-change-under-same-key
  (let* ((left-doc
           (star.frontends.http-api::target-v1-document-from-request
            (make-v1-target-request :target "example.org")
            "human:alice"))
         (right-doc
           (star.frontends.http-api::target-v1-document-from-request
            (make-v1-target-request :target "example.net")
            "human:alice"))
         (destination
           (star.actors::make-target-destination-handle
            :rabbit "subfinder"
            :routing-key "documents.target.dispatch.subfinder"))
         (left-envelope
           (star.actors::make-target-dispatch-envelope
            (star.actors::parse-target-record left-doc)
            :destination destination))
         (right-envelope
           (star.actors::make-target-dispatch-envelope
            (star.actors::parse-target-record right-doc)
            :destination destination)))
    (is (not (string=
              (star.actors::target-dispatch-fingerprint left-envelope)
              (star.actors::target-dispatch-fingerprint right-envelope))))))

(test v1-target-receipt-is-narrow-and-retry-aware
  (let* ((document
           (star.frontends.http-api::target-v1-document-from-request
            (make-v1-target-request) "human:alice"))
         (record (star.actors::parse-target-record document))
         (destination
           (star.actors::make-target-destination-handle
            :rabbit "subfinder"
            :routing-key "documents.target.dispatch.subfinder"))
         (envelope
           (star.actors::make-target-dispatch-envelope
            record :destination destination))
         (accepted
           (star.actors::make-target-dispatch-outcome
            :accepted
            :acceptance-id "target-acceptance:test"
            :envelope envelope))
         (duplicate
           (star.actors::make-target-dispatch-outcome
            :duplicate
            :acceptance-id "target-acceptance:test"
            :envelope envelope))
         (accepted-json
           (star.frontends.http-api::target-v1-receipt accepted))
         (duplicate-json
           (star.frontends.http-api::target-v1-receipt duplicate)))
    (is (string= "accepted" (jsown:val accepted-json "status")))
    (is (string= "duplicate" (jsown:val duplicate-json "status")))
    (is (string= (jsown:val document "_id")
                 (jsown:val accepted-json "target_id")))
    (is (string= "target-acceptance:test"
                 (jsown:val accepted-json "acceptance_id")))
    (is-false (jsown:keyp accepted-json "target_document"))
    (is-false (jsown:keyp accepted-json "routing_key"))))