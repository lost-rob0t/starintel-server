(in-package :star-server-tests)

(in-suite actor-registry-tests)

(defun projected-manifest-fixture ()
  ;; Synthetic public wire fixture; no collector implementation or credentials.
  (jsown:new-js
    ("id" "manifest:fixture") ("dataset" "starintel-system")
    ("dtype" "actor-manifest") ("schemaVersion" "0.10.1")
    ("actor" "fixture-worker") ("actorVersion" "1.0.0") ("runtime" "python")
    ("accepts" (list "target")) ("produces" (list "account"))
    ("capabilities" (list "collect"))
    ("targetOptions"
     (jsown:new-js
       ("limit" (jsown:new-js ("description" "Result limit") ("required" :false)
                             ("schema" (jsown:new-js ("type" "integer")))))))))

(test nested-option-schemas-reject-non-json-before-registration
  (dolist (schema (list (jsown:new-js ("nested" #'identity))
                        (list (cons "dotted" "tail"))
                        (list :obj (cons 3 "bad-key"))
                        (list (list 1/3))))
    (let ((star.actors::*actor-registry* (make-hash-table :test #'equal))
          (manifest (actor-registry-test-manifest "star://local/actor/bad")))
      (setf (jsown:val manifest "options")
            (list (jsown:new-js ("key" "k") ("description" "Hint")
                                ("required" :true) ("schema" schema))))
      (signals star.actors::invalid-actor-manifest
        (star.actors::register-actor-manifest manifest))
      (is (zerop (hash-table-count star.actors::*actor-registry*))))))

(test nested-option-schemas-reject-cycles-and-preserve-valid-json
  (let* ((cycle (list 1))
         (manifest (actor-registry-test-manifest "star://local/actor/hints")))
    (setf (cdr cycle) cycle)
    (setf (jsown:val manifest "options")
          (list (jsown:new-js ("key" "k") ("description" "Hint")
                              ("required" :false) ("schema" cycle))))
    (signals star.actors::invalid-actor-manifest
      (star.actors::build-actor-registry (list manifest)))
    (let ((schema (jsown:new-js ("enum" (list :true :false :null 1 1.5 "ok")))))
      (setf (jsown:val (first (jsown:val manifest "options")) "schema") schema)
      (let* ((registry (star.actors::build-actor-registry (list manifest)))
             (before (jsown:to-json (star.actors:actor-registry-public-entries registry))))
        (setf (jsown:val schema "poison") #'identity)
        (let ((public (first (star.actors:actor-registry-public-entries registry))))
          (setf (jsown:val (jsown:val (first (jsown:val public "options")) "schema") "poison")
                #'identity))
        (is (string= before
                     (jsown:to-json (star.actors:actor-registry-public-entries registry))))))))

(test persisted-manifest-projection-replays-without-changing-local-registry
  (let* ((star.actors::*actor-registry*
           (star.actors::build-actor-registry
            (list (actor-registry-test-manifest "star://local/actor/local"))))
         (document (projected-manifest-fixture))
         (first (star.actors:actor-registry-document (list document)))
         (second (star.actors:actor-registry-document (list document))))
    (is (string= (jsown:to-json first) (jsown:to-json second)))
    (is (= 1 (hash-table-count star.actors::*actor-registry*)))
    (let* ((entries (jsown:val (jsown:val first "data") "actors"))
           (projected (find "star://document-manifests/actor/fixture-worker" entries
                            :key (lambda (item) (jsown:val item "resourceUri")) :test #'string=)))
      (is (= 2 (length entries)))
      (is (not (null projected)))
      (when projected
        (is (string= "fixture-worker" (jsown:val (jsown:val projected "semantic") "name")))
        (is (equal '("target") (jsown:val (jsown:val projected "accepts") "targets")))
        (is (equal '("account") (jsown:val (jsown:val projected "produces") "documents")))
        (is (equal '("collect") (jsown:val projected "capabilities")))
        (is (eq :false (jsown:val projected "ready")))
        (is (string= "limit" (jsown:val (first (jsown:val projected "options")) "key")))))))

(test persisted-manifest-projection-isolated-invalid-and-conflicting-records
  (let* ((document (projected-manifest-fixture))
         (bad (projected-manifest-fixture))
         (star.actors::*actor-registry* (make-hash-table :test #'equal)))
    (setf (jsown:val bad "actor") "bad/identity")
    (let ((result (star.actors:actor-registry-document (list bad document))))
      (is (= 1 (jsown:val (jsown:val result "data") "count"))))
    (star.actors::register-actor-manifest
     (actor-registry-test-manifest "star://document-manifests/actor/fixture-worker"
                                   :name "trusted-local"))
    (let* ((result (star.actors:actor-registry-document (list document)))
           (entry (first (jsown:val (jsown:val result "data") "actors"))))
      (is (string= "trusted-local" (jsown:val (jsown:val entry "semantic") "name"))))))

(test persisted-manifest-query-is-bounded-and-uses-existing-dtype-view
  (let* ((document (projected-manifest-fixture))
         (calls nil)
         (result
           (star.actors::query-actor-manifest-documents
            :client :client :database "db"
            :query-fn (lambda (&rest arguments)
                        (setf calls arguments)
                        (jsown:new-js ("rows" (list (jsown:new-js ("doc" document)))))))))
    (is (equal (list document) result))
    (is (equal '(:client "db" "data" "count_by_dtype") (subseq calls 0 4)))
    (is (equal "actor-manifest" (getf (nthcdr 4 calls) :key)))
    (is (eq t (getf (nthcdr 4 calls) :include-docs)))
    (is (null (getf (nthcdr 4 calls) :reduce)))
    (is (= 129 (getf (nthcdr 4 calls) :limit))))
  (signals error
    (star.actors::query-actor-manifest-documents
     :query-fn (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (jsown:new-js ("rows" (make-list 129)))))))

(test persisted-manifest-snapshot-follows-updates-and-deletions
  (let ((document (projected-manifest-fixture))
        (star.actors::*actor-registry* (make-hash-table :test #'equal)))
    (let ((before (jsown:to-json (star.actors:actor-registry-document (list document)))))
      (setf (jsown:val document "actorVersion") "2.0.0")
      (let ((after (jsown:to-json (star.actors:actor-registry-document (list document)))))
        (is-false (string= before after))))
    (setf (jsown:val document "deleted") :true)
    (is (= 0 (jsown:val (jsown:val (star.actors:actor-registry-document (list document)) "data") "count")))
    (is (= 0 (jsown:val (jsown:val (star.actors:actor-registry-document nil) "data") "count")))))

(test persisted-manifest-projection-excludes-private-and-legacy-documents
  (dolist (change '(("visibility" . "private") ("dataset" . "tenant-data")
                    ("tenant_id" . "private-tenant") ("tenant" . "private-tenant")
                    ("schemaVersion" . "0.9.0")))
    (let ((document (projected-manifest-fixture))
          (star.actors::*actor-registry* (make-hash-table :test #'equal)))
      (setf (jsown:val document (car change)) (cdr change))
      (is (= 0 (jsown:val (jsown:val (star.actors:actor-registry-document (list document)) "data") "count")))))
  (let ((document (projected-manifest-fixture))
        (star.actors::*actor-registry* (make-hash-table :test #'equal)))
    (setf (jsown:val document "tenant_id") "public")
    (is (= 1 (jsown:val (jsown:val (star.actors:actor-registry-document (list document)) "data") "count")))))
