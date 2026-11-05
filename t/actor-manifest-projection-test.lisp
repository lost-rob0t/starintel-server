(in-package :star-server-tests)

(in-suite actor-registry-tests)

(defun projected-manifest-fixture ()
  ;; Synthetic public wire fixture; no collector implementation or credentials.
  (jsown:new-js
    ("id" "manifest:fixture") ("dataset" "starintel-system")
    ("dtype" "actor-manifest") ("schemaVersion" "0.10.1") ("tenant_id" "public")
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
                        (jsown:new-js ("rows" (list (jsown:new-js ("id" "manifest:fixture") ("doc" document)))))))))
    (is (equal (list document) result))
    (is (equal '(:client "db" "data" "count_by_dtype") (subseq calls 0 4)))
    (is (equal "actor-manifest" (getf (nthcdr 4 calls) :start-key)))
    (is (equal "actor-manifest" (getf (nthcdr 4 calls) :end-key)))
    (is (eq t (getf (nthcdr 4 calls) :include-docs)))
    (is (null (getf (nthcdr 4 calls) :reduce)))
    (is (= 129 (getf (nthcdr 4 calls) :limit)))))

(defun manifest-test-rows (documents)
  "Give synthetic persisted documents distinct, ordered CouchDB identities."
  (loop for document in documents for index from 0
        for id = (format nil "manifest-~6,'0d" index)
        do (setf (jsown:val document "id") id)
        collect (jsown:new-js ("id" id) ("key" "actor-manifest") ("doc" document))))

(defun manifest-paged-query-fixture (rows calls)
  "Model CouchDB's inclusive document-ID cursor and bounded result limit."
  (lambda (&rest arguments)
    (push arguments (car calls))
    (let* ((options (nthcdr 4 arguments))
           (cursor (getf options :start-key-doc-id))
           (remaining (remove-if
                       (lambda (row) (and cursor (string< (jsown:val row "id") cursor))) rows)))
      (jsown:new-js
        ("rows" (subseq remaining 0 (min (length remaining) (getf options :limit))))))))

(test manifest-capacity-excludes-ineligible-records-across-pages
  (let* ((documents
           (loop for index below 260 for document = (projected-manifest-fixture)
                 do (case (mod index 4)
                      (0 (setf (jsown:val document "tenant_id") "private"))
                      (1 (setf (jsown:val document "deleted") :true))
                      (2 (setf (jsown:val document "dataset") "unrelated"))
                      (3 (setf (jsown:val document "actor") "invalid/name")))
                 collect document))
         (calls (list nil))
         (rows (manifest-test-rows (append documents (list (projected-manifest-fixture)))))
         (star.actors::*actor-registry*
           (star.actors::build-actor-registry
            (list (actor-registry-test-manifest "star://local/actor/local"))))
         (eligible (star.actors::query-actor-manifest-documents
                    :query-fn (manifest-paged-query-fixture rows calls)))
         (catalog (star.actors:actor-registry-document eligible)))
    (is (= 1 (length eligible)))
    (is (= 2 (jsown:val (jsown:val catalog "data") "count")))
    (is (= 3 (length (car calls))))
    (dolist (call (car calls))
      (is (<= (getf (nthcdr 4 call) :limit) 129)))))

(test manifest-capacity-counts-distinct-eligible-identities
  (let ((star.actors::*actor-registry* (make-hash-table :test #'equal)))
    (dolist (count '(128 129))
      (let* ((calls (list nil))
             (rows (manifest-test-rows
                    (loop repeat count collect (projected-manifest-fixture)))))
        (is (= 1 (length (star.actors::query-actor-manifest-documents
                         :query-fn (manifest-paged-query-fixture rows calls)))))))
    (dolist (count '(128 129))
      (let* ((calls (list nil))
             (rows (manifest-test-rows
                    (loop for index below count for document = (projected-manifest-fixture)
                          do (setf (jsown:val document "actor") (format nil "worker-~d" index))
                          collect document)))
             (query (manifest-paged-query-fixture rows calls)))
        (if (= count 128)
            (is (= 128 (length (star.actors::query-actor-manifest-documents :query-fn query))))
            (let ((reason (handler-case
                              (progn (star.actors::query-actor-manifest-documents :query-fn query) nil)
                            (error (condition) (princ-to-string condition)))))
              (is (not (null (search "exceeds 128 eligible" (or reason "")))))))))))

(test manifest-scan-exhaustion-is-distinct-from-catalog-overflow
  (let* ((calls (list nil))
         (rows (manifest-test-rows
                (loop repeat 260 for document = (projected-manifest-fixture)
                      do (setf (jsown:val document "deleted") :true) collect document)))
         (reason
           (handler-case
               (progn
                 (star.actors::query-actor-manifest-documents
                  :query-fn (manifest-paged-query-fixture rows calls) :max-pages 1)
                 nil)
             (error (condition) (princ-to-string condition)))))
    (is (not (null (search "scan budget" (or reason "")))))))

(test manifest-pagination-does-not-skip-after-cursor-deletion
  (let* ((calls (list nil))
         (star.actors::*actor-registry* (make-hash-table :test #'equal))
         (rows (manifest-test-rows
                (append
                 (loop repeat 129 for document = (projected-manifest-fixture)
                       do (setf (jsown:val document "deleted") :true) collect document)
                 (loop for actor in '("first-worker" "second-worker")
                       for document = (projected-manifest-fixture)
                       do (setf (jsown:val document "actor") actor) collect document))))
         (documents
           (star.actors::query-actor-manifest-documents
            :query-fn
            (lambda (&rest arguments)
              (let ((cursor (getf (nthcdr 4 arguments) :start-key-doc-id)))
                (when cursor
                  (setf rows (remove cursor rows :key (lambda (row) (jsown:val row "id"))
                                     :test #'equal)))
                (apply (manifest-paged-query-fixture rows calls) arguments))))))
    (is (= 2 (length documents)))
    (is (equal '("first-worker" "second-worker")
               (mapcar (lambda (document) (jsown:val document "actor")) documents)))
    (is (equal "manifest-000128"
               (getf (nthcdr 4 (first (car calls))) :start-key-doc-id)))))

(test manifest-capacity-preserves-trusted-local-precedence
  (let* ((calls (list nil))
         (star.actors::*actor-registry*
           (star.actors::build-actor-registry
            (list (actor-registry-test-manifest
                   "star://document-manifests/actor/fixture-worker" :name "trusted-local"))))
         (rows (manifest-test-rows
                (loop repeat 129 collect (projected-manifest-fixture))))
         (documents (star.actors::query-actor-manifest-documents
                     :query-fn (manifest-paged-query-fixture rows calls)))
         (actors (jsown:val (jsown:val (star.actors:actor-registry-document documents) "data") "actors")))
    (is (null documents))
    (is (= 1 (length actors)))
    (is (equal "trusted-local" (jsown:val (jsown:val (first actors) "semantic") "name")))))

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

(test unstamped-manifests-use-effective-authorization-tenancy
  (dolist (scenario '((nil nil 0)
                      (nil "private-team" 0)
                      (nil "public" 1)
                      ((("starintel-system" . "private-team")) nil 0)
                      ((("starintel-system" . "private-team")) "public" 0)
                      ((("starintel-system" . "public")) "private-team" 1)))
    (let ((star:*tenant-dataset-map* (first scenario))
          (star:*tenant-fallback* (second scenario))
          (document (projected-manifest-fixture))
          (star.actors::*actor-registry* (make-hash-table :test #'equal)))
      (jsown:remkey document "tenant_id")
      (is (= (third scenario)
             (jsown:val (jsown:val (star.actors:actor-registry-document (list document)) "data") "count")))
      (let ((calls (list nil)))
        (is (= (third scenario)
               (length (star.actors::query-actor-manifest-documents
                        :query-fn (manifest-paged-query-fixture
                                   (manifest-test-rows (list document)) calls)))))))))

(test invalid-stored-manifest-identity-does-not-poison-discovery
  (let* ((bad (projected-manifest-fixture))
         (good (projected-manifest-fixture))
         (calls (list nil))
         (star.actors::*actor-registry*
           (star.actors::build-actor-registry
            (list (actor-registry-test-manifest "star://local/actor/local")))))
    (setf (jsown:val bad "_id") "other-stored-identity")
    (is (null (star.actors::eligible-actor-manifest-entry bad)))
    (let* ((documents (star.actors::query-actor-manifest-documents
                       :query-fn (manifest-paged-query-fixture
                                  (manifest-test-rows (list bad good)) calls)))
           (catalog (star.actors:actor-registry-document documents)))
      (is (= 1 (length documents)))
      (is (= 2 (jsown:val (jsown:val catalog "data") "count"))))))

(test manifest-storage-failures-remain-visible
  (let* ((failure (make-condition 'simple-error :format-control "Storage unavailable"))
         (caught (handler-case
                     (progn
                       (star.actors::query-actor-manifest-documents
                        :query-fn (lambda (&rest arguments)
                                    (declare (ignore arguments)) (error failure)))
                       nil)
                   (error (condition) condition))))
    (is (eq failure caught))))
