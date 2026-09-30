(in-package :star-server-tests)

(def-suite v0101-ingest-tests
  :description "Star-Lang 0.10.1 migration and resilient batch ingress")

(in-suite v0101-ingest-tests)

(defun legacy-person (id)
  (jsown:new-js
    ("_id" id)
    ("dataset" "fixture")
    ("dtype" "person")
    ("schema_version" "0.9.0")
    ("data" (jsown:new-js
              ("fname" "Ada")
              ("external_ids" (jsown:new-js ("passport" "A123")))))))

(test legacy-person-migrates-to-canonical-person-and-identifier
  (let ((documents
          (star.frontends.http-api:canonical-document-input
           (legacy-person "person-old") :path-dtype "person")))
    (is (= 2 (length documents)))
    (let ((person (aref documents 0))
          (identifier (aref documents 1)))
      (is (string= "person-old" (jsown:val person "id")))
      (is (string= "0.10.1" (jsown:val person "schemaVersion")))
      (is-false (jsown:keyp person "_id"))
      (is-false (jsown:keyp person "schema_version"))
      (is (string= "person-identifier" (jsown:val identifier "dtype")))
      (is (string= "a123" (jsown:val identifier "normalizedValue"))))))

(test invalid-legacy-document-is-quarantined-without-ending-batch
  (let ((collision
          (jsown:new-js
            ("_id" "legacy-id")
            ("id" "canonical-id")
            ("dataset" "fixture")
            ("dtype" "person")
            ("schema_version" "0.9.0"))))
    (multiple-value-bind (documents quarantine)
        (star.frontends.http-api:canonical-document-batch
         (list collision (legacy-person "person-valid")))
      (is (= 2 (length documents)))
      (is (= 1 (length quarantine)))
      (is (= 0 (jsown:val (aref quarantine 0) "index")))
      (is (string= "ambiguousFieldCollision"
                   (jsown:val (aref quarantine 0) "reasonCode")))
      (is (string= "invalid_document_schema"
                   (jsown:val (aref quarantine 0) "errorCode")))
      (is (string= "person-valid" (jsown:val (first documents) "id"))))))

(test canonical-wire-json-does-not-add-couchdb-identity
  (let* ((document (aref (star.frontends.http-api:canonical-document-input
                           (legacy-person "wire-person"))
                         0))
         (wire (jsown:parse (star.documents:document-json document))))
    (is (string= "wire-person" (jsown:val wire "id")))
    (is-false (jsown:keyp wire "_id"))))

(test rabbit-adds-private-couchdb-identity-after-canonical-validation
  (let* ((document (aref (star.frontends.http-api:canonical-document-input
                           (legacy-person "stored-person"))
                         0))
         (decoded
           (star.rabbit:decode-rabbit-document
            (cons (jsown:to-json document) 1))))
    (is (string= "stored-person" (jsown:val decoded "id")))
    (is (string= "stored-person" (jsown:val decoded "_id")))))
