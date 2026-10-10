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
                                 (options (jsown:empty-object))
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
            request "human:bob"))
         (extensions (jsown:val first "extensions")))
    (is (string= (jsown:val first "id")
                 (jsown:val retry "id")))
    (is (not (string= (jsown:val first "id")
                      (jsown:val other-user "id"))))
    (is (string= "target" (jsown:val first "dtype")))
    (is (string= (starintel.canonical:schema-version)
                 (jsown:val first "schemaVersion")))
    (is (stringp (jsown:val extensions "idempotency_key")))
    (is (null (search "bixby-draft-123"
                      (jsown:val extensions "idempotency_key")
                      :test #'char-equal)))))

(test v1-target-document-is-flat-and-schema-valid
  (let* ((document
           (star.frontends.http-api::target-v1-document-from-request
            (make-v1-target-request) "human:alice"))
         (extensions (jsown:val document "extensions")))
    (dolist (key '("_id" "schema_version" "data" "date_added" "date_updated"))
      (is-false (jsown:keyp document key)))
    (is (string= "subfinder" (jsown:val document "actor")))
    (is (string= "example.org" (jsown:val document "target")))
    (is (= 1 (jsown:val document "delay")))
    (is (eq :false (jsown:val document "recurring")))
    (is (star.frontends.http-api::json-object-p (jsown:val document "options")))
    (is (string= "target-request:"
                 (subseq (jsown:val extensions "schedule_id") 0 15)))
    (is (integerp (jsown:val document "createdAt")))
    (is (eq document (star.documents:validate-document document)))))

(test v1-schedule-identity-is-read-from-extensions
  (let* ((document
           (star.frontends.http-api::target-v1-document-from-request
            (make-v1-target-request) "human:alice"))
         (record (star.actors::parse-target-record document)))
    (is (string= (jsown:val (jsown:val document "extensions") "schedule_id")
                 (star.actors::target-record-schedule-id record)))
    (is (string= (jsown:val document "id")
                 (star.actors::target-record-id record)))))

(test legacy-persisted-target-keeps-its-schedule-identity
  ;; Recovery of pre-fix persisted documents still reads top-level fields.
  (let* ((document
           (jsown:new-js
             ("_id" "legacy-persisted-target")
             ("dataset" "star-intel")
             ("dtype" "target")
             ("actor" "subfinder")
             ("target" "legacy.example.invalid")
             ("delay" 60)
             ("schedule_id" "legacy-schedule-1")))
         (record (star.actors::parse-target-record document)))
    (is (string= "subfinder" (star.actors::target-record-actor record)))
    (is (string= "legacy.example.invalid" (star.actors::target-record-target record)))
    (is (= 60 (star.actors::target-record-delay record)))
    (is (string= "legacy-schedule-1"
                 (star.actors::target-record-schedule-id record)))))

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

(test request-ledger-detects-content-change-under-same-idempotency-key
  (let* ((left-request (make-v1-target-request :target "example.org"))
         (right-request (make-v1-target-request :target "example.net"))
         (left-document
           (star.frontends.http-api::target-v1-document-from-request
            left-request "human:alice"))
         (right-document
           (star.frontends.http-api::target-v1-document-from-request
            right-request "human:alice"))
         (left-ledger
           (star.frontends.http-api::target-v1-request-ledger
            left-request left-document "human:alice"))
         (right-ledger
           (star.frontends.http-api::target-v1-request-ledger
            right-request right-document "human:alice")))
    ;; The idempotency identity is stable, but its semantic fingerprint is not.
    (is (string= (jsown:val left-ledger "_id")
                 (jsown:val right-ledger "_id")))
    (is (not (string= (jsown:val left-ledger "fingerprint")
                      (jsown:val right-ledger "fingerprint"))))
    (is-false
     (star.frontends.http-api::target-v1-request-equivalent-p
      left-ledger right-ledger))))

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
  (let* ((request (make-v1-target-request))
         (document
           (star.frontends.http-api::target-v1-document-from-request
            request "human:alice"))
         (ledger
           (star.frontends.http-api::target-v1-request-ledger
            request document "human:alice"))
         (accepted-json
           (star.frontends.http-api::target-v1-receipt ledger :created))
         (duplicate-json
           (star.frontends.http-api::target-v1-receipt ledger :duplicate)))
    (is (string= "accepted" (jsown:val accepted-json "status")))
    (is (string= "duplicate" (jsown:val duplicate-json "status")))
    (is (string= (jsown:val document "id")
                 (jsown:val accepted-json "target_id")))
    (is (string= (jsown:val ledger "_id")
                 (jsown:val accepted-json "request_id")))
    (is-false (jsown:keyp accepted-json "target_document"))
    (is-false (jsown:keyp accepted-json "principal_id"))))
(test canonical-target-options-map-creates-flat-wire-document
  (let* ((options (jsown:new-js ("opaque_key" (jsown:new-js ("false" :false) ("null" :null)))))
         (document
           (star.frontends.http-api::target-v1-document-from-request
            (make-v1-target-request :options options) "human:canonical")))
    (is (string= "0.10.1" (jsown:val document "schemaVersion")))
    (is (stringp (jsown:val document "id")))
    (is (equal options (jsown:val document "options")))
    (dolist (key '("_id" "schema_version" "data"))
      (is-false (jsown:keyp document key)))
    (is (eq document (star.documents:validate-document document)))))

(test canonical-target-options-reject-nonobjects
  (dolist (options (list #() "not-an-object" :null nil))
    (let ((condition
            (capture-http-input-error
             (lambda ()
               (star.frontends.http-api::target-v1-document-from-request
                (make-v1-target-request :options options) "human:canonical")))))
      (is (typep condition 'star.frontends.http-api:http-input-error))
      (when condition
        (is (string= "invalid_target_options"
                     (star.frontends.http-api:http-input-error-code condition)))))))

(test canonical-wire-target-recovers-without-couch-id
  (let* ((document (jsown:new-js ("id" "target:canonical-red")
                                 ("dataset" "canonical-tests") ("dtype" "target")
                                 ("schemaVersion" "0.10.1") ("actor" "subfinder")
                                 ("target" "example.org") ("delay" 1)
                                 ("options" (jsown:empty-object))))
         (record (star.actors:parse-target-record document)))
    (is (string= "target:canonical-red" (star.actors:target-record-id record)))
    (is (equal (jsown:val document "options") (star.actors:target-record-options record)))))

(test canonical-target-options-default-and-recursive-fingerprint
  (let* ((left (make-v1-target-request
                :options (jsown:new-js ("opaque_key" (jsown:new-js ("b" :false) ("a" :null)))
                                       ("items" (vector 1 2)) ("empty" (jsown:empty-object)))))
         (right (make-v1-target-request
                 :options (jsown:new-js ("empty" (jsown:empty-object)) ("items" (vector 1 2))
                                        ("opaque_key" (jsown:new-js ("a" :null) ("b" :false)))))))
    (is (string= (star.frontends.http-api::target-v1-request-fingerprint left "human:alice")
                 (star.frontends.http-api::target-v1-request-fingerprint right "human:alice")))
    (setf (jsown:val (jsown:val right "options") "items") (vector 2 1))
    (is (not (string= (star.frontends.http-api::target-v1-request-fingerprint left "human:alice")
                      (star.frontends.http-api::target-v1-request-fingerprint right "human:alice")))))
  (let ((request (make-v1-target-request)))
    (jsown:remkey request "options")
    (is (star.frontends.http-api::json-object-p
         (jsown:val (star.frontends.http-api::target-v1-document-from-request request "human:alice")
                    "options"))))
  (is (not (string= (star.frontends.http-api::target-v1-request-identity "alice|b" "c")
                    (star.frontends.http-api::target-v1-request-identity "alice" "b|c")))))

(test canonical-target-storage-recovery-and-dispatch-preserve-wire
  (let* ((wire (star.frontends.http-api::target-v1-document-from-request
                (make-v1-target-request :options (jsown:new-js ("source_url" "https://example.org/")))
                "human:alice"))
         (stored (star.documents:ensure-document (star.documents:clone-document-object wire)))
         (sent nil))
    (setf (jsown:val stored "_rev") "2-stored"
          (jsown:val stored "tenant_id") "private-tenant")
    (let* ((record (star.actors:parse-target-record stored))
           (envelope (star.actors:make-target-dispatch-envelope
                      record :destination
                      (star.actors::make-target-destination-handle
                       :rabbit "subfinder" :routing-key "documents.target.dispatch.subfinder"))))
      (is (string= (jsown:val wire "id") (star.actors:target-record-id record)))
      (is (string= "2-stored" (star.actors:target-record-revision record)))
      (star.actors:dispatch-target-envelope-now
       envelope :remote-send-fn
       (lambda (routing-key document)
         (is (string= "documents.target.dispatch.subfinder" routing-key))
         (setf sent document)))
      (is (string= (jsown:val wire "id") (jsown:val sent "id")))
      (is (equal (jsown:val wire "options") (jsown:val sent "options")))
      (is (string= "2-stored" (jsown:val sent "rev")))
      (dolist (key '("_id" "_rev" "tenant_id" "schema_version" "data"))
        (is-false (jsown:keyp sent key)))
      (is (eq sent (star.documents:validate-document sent))))
    (setf (jsown:val stored "_id") "wrong-identity")
    (signals error (star.actors:parse-target-record stored))))

(test canonical-target-validation-and-unix-deadline-precede-dispatch
  (let* ((document (star.frontends.http-api::target-v1-document-from-request
                    (make-v1-target-request) "human:alice"))
         (now (- (get-universal-time) 2208988800)))
    (setf (jsown:val document "deadline") (+ now 60))
    (let ((record (star.actors:parse-target-record document)))
      (is (eq record (star.actors:validate-target-dispatch-record record))))
    (setf (jsown:val document "deadline") (- now 60))
    (signals star.actors:invalid-target-dispatch
      (star.actors:validate-target-dispatch-record
       (star.actors:parse-target-record document)))
    (jsown:remkey document "deadline")
    (setf (jsown:val document "options") #())
    (signals star.documents:document-schema-validation-error
      (star.actors:parse-target-record document))))
