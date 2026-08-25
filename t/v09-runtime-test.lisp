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

(test target-rabbit-adapter-explicitly-skips-strict-schema
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
