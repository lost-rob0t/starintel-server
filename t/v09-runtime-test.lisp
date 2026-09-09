(in-package :star-server-tests)

(def-suite v09-runtime-tests
  :description "Canonical StarIntel v0.9 validator and Rabbit mutation boundary")

(in-suite v09-runtime-tests)

(defun v09-test-host-document ()
  (starintel:encode
   (starintel:new-host
    "v09-runtime-tests"
    :ip "192.0.2.20"
    :os "linux")))

(defun v09-test-operation-document ()
  (let* ((data (jsown:empty-object))
         (targets (jsown:empty-object))
         (discovery (jsown:empty-object))
         (analysis (jsown:empty-object)))
    (setf (jsown:val targets "primary")
          (list "starintel:target:operation-runtime-primary")
          (jsown:val targets "supporting")
          (list "starintel:investigation-target:operation-runtime-question")
          (jsown:val discovery "phase_id") "discovery"
          (jsown:val discovery "objective") "Discover approved public sources"
          (jsown:val discovery "state") "completed"
          (jsown:val discovery "completion_evidence")
          (list "starintel:evidence-record:operation-runtime-discovery")
          (jsown:val analysis "phase_id") "analysis"
          (jsown:val analysis "objective") "Analyze collected operation data"
          (jsown:val analysis "state") "ready"
          (jsown:val analysis "depends_on") (list "discovery")
          (jsown:val data "mission") "Exercise the operation runtime boundary"
          (jsown:val data "status") "active"
          (jsown:val data "in_scope") (list "public-source research")
          (jsown:val data "out_of_scope") (list "private contact information")
          (jsown:val data "targets") targets
          (jsown:val data "phases") (list discovery analysis))
    (starintel:encode
     (make-instance 'starintel:operation
                    :dataset "v09-runtime-tests"
                    :title "Operation runtime boundary"
                    :data data))))

(defun capture-schema-invalid (thunk)
  (handler-case
      (progn
        (funcall thunk)
        nil)
    (star.consumers:schema-invalid-delivery-error (condition)
      condition)))

(defun invalid-rabbit-delivery ()
  (cons
   (jsown:to-json
    (jsown:new-js
      ("_id" "invalid-rabbit-document")
      ("dataset" "v09-runtime-tests")
      ("dtype" "host")
      ("version" 7)))
   1))

(test canonical-starintel-encoding-passes-server-validator
  (let ((document (v09-test-host-document)))
    (is (eq document (star.documents:validate-v09-document document)))
    (is (string= "0.9.0" (jsown:val document "schema_version")))
    (is (= 1 (jsown:val document "version")))))

(test operation-encoding-passes-server-validator
  (let* ((document (v09-test-operation-document))
         (data (jsown:val document "data"))
         (targets (jsown:val data "targets")))
    (is (eq document (star.documents:validate-v09-document document)))
    (is (string= "operation" (jsown:val document "dtype")))
    (is (string= "Action"
                 (jsown:val (jsown:val document "schema_org") "@type")))
    (is (string= "starintel:target:operation-runtime-primary"
                 (first (jsown:val targets "primary"))))
    (is (string= "starintel:investigation-target:operation-runtime-question"
                 (first (jsown:val targets "supporting"))))))

(test rabbit-ingest-invalid-schema-cannot-reach-persistence
  (let ((persisted nil))
    (let ((condition
            (capture-schema-invalid
             (lambda ()
               (star.rabbit::process-rabbit-document-mutation
                (invalid-rabbit-delivery)
                :new
                :persist-fn
                (lambda (document operation)
                  (declare (ignore document operation))
                  (setf persisted t)))))))
      (is-true condition)
      (is (typep condition 'star.consumers:schema-invalid-delivery-error))
      (is-false persisted))))

(test rabbit-update-invalid-schema-cannot-reach-persistence
  (let ((persisted nil))
    (let ((condition
            (capture-schema-invalid
             (lambda ()
               (star.rabbit::process-rabbit-document-mutation
                (invalid-rabbit-delivery)
                :updated
                :persist-fn
                (lambda (document operation)
                  (declare (ignore document operation))
                  (setf persisted t)))))))
      (is-true condition)
      (is (typep condition 'star.consumers:schema-invalid-delivery-error))
      (is-false persisted))))

(test decode-escape-hatch-permits-transport-metadata-inspection
  ;; transient-p inspects transport metadata without strict schema
  ;; validation; it is the only remaining non-strict decode.
  (let* ((message
           (cons
            (jsown:to-json
             (jsown:new-js
               ("_id" "legacy-target")
               ("dtype" "target")
               ("actor" "nmap")
               ("legacy_flat_field" "compatibility")))
            1))
         (document
           (star.rabbit:decode-rabbit-document
            message
            :route-dtype "target"
            :strict-schema-p nil)))
    (is (string= "legacy-target" (jsown:val document "_id")))
    (is (string= "target" (jsown:val document "dtype")))))

(test target-deliveries-are-strictly-validated
  ;; The target compatibility consumer validates like every other dtype:
  ;; the historical top-level actor envelope is now a schema violation.
  (let* ((message
           (cons
            (jsown:to-json
             (jsown:new-js
               ("_id" "legacy-target")
               ("dtype" "target")
               ("actor" "nmap")
               ("legacy_flat_field" "compatibility")))
            1))
         (condition
           (capture-schema-invalid
            (lambda ()
              (star.rabbit:decode-rabbit-document
               message :route-dtype "target")))))
    (is-true condition)))

(test rabbit-strict-validation-is-default
  (is-true
   (capture-schema-invalid
    (lambda ()
      (star.rabbit:decode-rabbit-document (invalid-rabbit-delivery))))))

(test target-compatibility-routing-key-is-isolated
  (is (string= "documents.new.target.nmap"
               (star.actors:compatibility-target-ingress-routing-key "Nmap")))
  (signals star.actors:invalid-target-dispatch
    (star.actors:compatibility-target-ingress-routing-key "nmap.#")))

(test http-update-schema-validation-precedes-save
  (let* ((existing (v09-test-host-document))
         (patch (jsown:new-js
                  ("legacy_flat_field" "must-not-persist")))
         (saved nil)
         (outcome
           (star.databases.couchdb:upsert-document-update
            (lambda (document-id)
              (declare (ignore document-id))
              existing)
            (lambda (candidate)
              (setf saved candidate))
            (jsown:val existing "_id")
            patch)))
    (is (eq :validation-failed
            (star.databases.couchdb:document-update-outcome-status outcome)))
    (is (string= "invalid_document_schema"
                 (star.databases.couchdb:document-update-outcome-code outcome)))
    (is-false saved)))

(defun run-v09-runtime-tests ()
  (run! 'v09-runtime-tests))