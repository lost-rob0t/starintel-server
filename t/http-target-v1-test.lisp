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


(defun issue319-historical-http-acceptance (principal key &optional (status "scheduled"))
  (let* ((identity (star.frontends.http-api::target-v1-digest
                    (format nil "~a|~a" principal key)))
         (schedule-id (format nil "target-request:~a" identity))
         (document
           (jsown:new-js
             ("_id" (format nil "target:~a" identity))
             ("dataset" "star-intel") ("dtype" "target")
             ("schema_version" starintel.legacy:+starintel-doc-version+)
             ("version" 1) ("date_added" "2026-10-01T00:00:00Z")
             ("date_updated" "2026-10-01T00:00:00Z")
             ("sources" #()) ("evidence" #())
             ("data" (jsown:new-js ("actor" "subfinder") ("target" "example.org")
                                    ("delay" 1) ("recurring" :false) ("options" #())))
             ("extensions"
              (jsown:new-js ("idempotency_key" identity)
                            ("submitted_by" (star.frontends.http-api::target-v1-digest principal))
                            ("schedule_id" schedule-id)))))
         (envelope
           (star.actors:make-target-dispatch-envelope
            (star.actors:parse-target-record document)
            :destination
            (star.actors::make-target-destination-handle
             :rabbit "subfinder" :routing-key "documents.target.dispatch.subfinder")))
         (acceptance (star.actors:target-acceptance-document envelope)))
    (setf (jsown:val acceptance "status") status)
    acceptance))

(defun issue319-assert-historical-http-conflict (request principal existing)
  (let* ((accept-calls 0) (lookup-calls 0)
         (before (jsown:to-json existing))
         (record (star.actors:parse-target-record
                  (star.frontends.http-api::target-v1-document-from-request request principal)))
         (condition
           (capture-http-input-error
            (lambda ()
              (star.frontends.http-api::target-v1-accept-record
               request principal record
               :lookup-fn
               (lambda (id)
                 (incf lookup-calls)
                 (is (string=
                      (star.actors:target-acceptance-id
                       (format nil "target-request:~a"
                               (star.frontends.http-api::target-v1-digest
                                (format nil "~a|~a" principal
                                        (jsown:val request "idempotency_key")))))
                      id))
                 existing)
               :accept-fn
               (lambda (ignored-record)
                 (declare (ignore ignored-record))
                 (incf accept-calls)
                 (error "Historical reuse reached canonical acceptance")))))))
    (is (typep condition 'star.frontends.http-api:http-input-error))
    (when condition
      (is (= 409 (star.frontends.http-api:http-input-error-status condition)))
      (is (string= "target_idempotency_version_conflict"
                   (star.frontends.http-api:http-input-error-code condition))))
    (is (= 1 lookup-calls))
    (is (= 0 accept-calls))
    (is (string= before (jsown:to-json existing)))
    condition))

(test canonical-http-blocks-historical-idempotency-before-acceptance
  (dolist (status '("pending" "scheduled" "accepted" "dispatched"))
    (let* ((principal "human:alice") (key "historical-key")
           (existing (issue319-historical-http-acceptance principal key status))
           (identity (star.frontends.http-api::target-v1-digest
                      (format nil "~a|~a" principal key))))
      (is (star.frontends.http-api::target-v1-historical-identity-matches-p
           existing principal identity))
      (issue319-assert-historical-http-conflict
       (make-v1-target-request :idempotency-key key) principal existing)
      (issue319-assert-historical-http-conflict
       (make-v1-target-request :idempotency-key key :target "example.net")
       principal existing))))

(test canonical-http-historical-delimiter-collision-does-not-return-other-owner-receipt
  (let* ((existing (issue319-historical-http-acceptance "alice|b" "c"))
         (principal "alice") (key "b|c")
         (identity (star.frontends.http-api::target-v1-digest "alice|b|c")))
    (is-false (star.frontends.http-api::target-v1-historical-identity-matches-p
               existing principal identity))
    (let* ((condition (issue319-assert-historical-http-conflict
                       (make-v1-target-request :idempotency-key key) principal existing))
           (message (princ-to-string condition)))
      (is-false (search "alice|b" message))
      (is-false (search (jsown:val existing "target_id") message))
      (is-false (search (jsown:val existing "_id") message)))))

(test canonical-http-historical-malformed-identity-evidence-fails-closed
  (let* ((principal "human:alice") (key "historical-corrupt")
         (original (issue319-historical-http-acceptance principal key)))
    (dolist (mutate
              (list
               (lambda (doc) (jsown:remkey doc "target_document"))
               (lambda (doc) (setf (jsown:val doc "_id") "wrong-acceptance"))
               (lambda (doc) (setf (jsown:val doc "target_id") "wrong-target"))
               (lambda (doc) (setf (jsown:val doc "schedule_id") "wrong-schedule"))
               (lambda (doc) (setf (jsown:val doc "target_revision") "wrong-revision"))
               (lambda (doc) (setf (jsown:val (jsown:val doc "target_document") "_id")
                                   "wrong-target"))
               (lambda (doc) (jsown:remkey (jsown:val (jsown:val doc "target_document") "extensions")
                                          "submitted_by"))
               (lambda (doc) (setf (jsown:val (jsown:val (jsown:val doc "target_document") "extensions")
                                             "idempotency_key") "wrong-hash"))))
      (let ((broken (star.documents:clone-document-object original)))
        (funcall mutate broken)
        (issue319-assert-historical-http-conflict
         (make-v1-target-request :idempotency-key key) principal broken)))
    (issue319-assert-historical-http-conflict
     (make-v1-target-request :idempotency-key key) principal (jsown:empty-object))))

(test canonical-http-historical-lookup-failure-cannot-create-new-acceptance
  (let* ((request (make-v1-target-request)) (principal "human:alice")
         (record (star.actors:parse-target-record
                  (star.frontends.http-api::target-v1-document-from-request request principal)))
         (accept-calls 0))
    (signals error
      (star.frontends.http-api::target-v1-accept-record
       request principal record
       :lookup-fn (lambda (id) (declare (ignore id)) (error "lookup unavailable"))
       :accept-fn (lambda (record) (declare (ignore record)) (incf accept-calls))))
    (is (= 0 accept-calls))))

(test canonical-http-new-key-keeps-normal-acceptance-and-duplicate-path
  (let ((star.actors::*active-target-schedules* (make-hash-table :test #'equal))
        (request (make-v1-target-request :idempotency-key "new-canonical-key"))
        (principal "human:alice")
        (stored nil) (creates 0) (updates 0) (schedules 0) (lookups 0)
        (historical-id nil))
    (labels ((accept (record)
               (star.actors:accept-target-record
                record :destination
                (star.actors::make-target-destination-handle
                 :rabbit "subfinder" :routing-key "documents.target.dispatch.subfinder")
                :persist-fn
                (lambda (desired equivalent-p)
                  (cond ((null stored)
                         (incf creates) (setf stored desired) (values stored :created))
                        ((funcall equivalent-p stored desired) (values stored :duplicate))
                        (t (values stored :conflict))))
                :update-fn
                (lambda (id updater)
                  (is (string= id (jsown:val stored "_id")))
                  (incf updates) (setf stored (funcall updater stored)))
                :schedule-once-fn
                (lambda (&rest args) (declare (ignore args)) (incf schedules))))
             (submit ()
               (star.frontends.http-api::target-v1-accept-record
                request principal
                (star.actors:parse-target-record
                 (star.frontends.http-api::target-v1-document-from-request request principal))
                :lookup-fn (lambda (id) (incf lookups) (setf historical-id id) nil)
                :accept-fn #'accept)))
      (is (eq :accepted (star.actors:target-dispatch-outcome-status (submit))))
      (is (eq :duplicate (star.actors:target-dispatch-outcome-status (submit))))
      (is (= 1 creates updates schedules))
      (is (= 2 lookups))
      (is (not (string= historical-id (jsown:val stored "_id"))))
      (is (string= "0.10.1"
                   (jsown:val (jsown:val stored "target_document") "schemaVersion"))))))
