(in-package :star-server-tests)

(def-suite event-source-tests
  :description "Optional capture, archive crash boundary and pure replay")
(in-suite event-source-tests)

(defstruct replay-test-store
  document (revision 0) archive published fail-archive-p)

(defun replay-test-document (&key (value 1) (mutation "mutation-1") request)
  (let ((extensions (jsown:new-js ("mutation_id" mutation))))
    (when request
      (setf (jsown:val extensions "event_source_request") request))
    (jsown:new-js
      ("_id" "doc-1") ("dtype" "person") ("dataset" "fixture")
      ("tenant_id" "tenant-a")
      ("data" (jsown:new-js ("value" value) ("false" :false)
                             ("null" :null) ("empty" #())))
      ("extensions" extensions))))

(defun replay-test-request (sequence &optional (protocol "starintel.event-source/1"))
  (jsown:new-js ("protocol" protocol) ("expected_sequence" sequence)))

(defun replay-test-load (store id)
  (declare (ignore id))
  (when (replay-test-store-document store)
    (star.databases.couchdb::clone-outbox-json (replay-test-store-document store))))

(defun replay-test-save (store state)
  (let* ((current (replay-test-store-document store))
         (get #'star.databases.couchdb::outbox-object-value))
    (unless (equal (funcall get current "_rev") (funcall get state "_rev"))
      (error 'star.databases.couchdb::outbox-store-conflict))
    (let ((saved (star.databases.couchdb::clone-outbox-json state)))
      (setf (jsown:val saved "_rev")
            (format nil "~d-fixture" (incf (replay-test-store-revision store)))
            (replay-test-store-document store) saved)
      saved)))

(defun replay-test-publish (store route payload id)
  (push (list route payload id) (replay-test-store-published store)))

(defun replay-test-archive (store record)
  (when (replay-test-store-fail-archive-p store)
    (error "simulated archive unavailable"))
  (let ((previous (find (jsown:val record "_id") (replay-test-store-archive store)
                        :test #'equal :key (lambda (r) (jsown:val r "_id")))))
    (if previous
        (unless (equal (jsown:val previous "hash") (jsown:val record "hash"))
          (error "archive identity conflict"))
        (push (star.databases.couchdb::clone-outbox-json record)
              (replay-test-store-archive store))))
  record)

(defmacro with-replay-test-store ((store &optional (mode :smart)) &body body)
  `(let* ((,store (make-replay-test-store))
          (star.databases.couchdb:*event-source-mode* ,mode)
          (star.databases.couchdb::*outbox-source-database* "starintel")
          (star.databases.couchdb::*event-source-archive-function*
            (lambda (record) (replay-test-archive ,store record))))
     ,@body))

(defun replay-test-write (store document operation)
  (star.databases.couchdb::process-outbox-mutation
   (lambda (id) (replay-test-load store id))
   (lambda (state) (replay-test-save store state))
   (lambda (route payload id) (replay-test-publish store route payload id))
   document operation))

(test off-mode-preserves-outbox-without-archive-effects
  (with-replay-test-store (store :off)
    (replay-test-write store (replay-test-document) :new)
    (is (null (replay-test-store-archive store)))
    (is (= 1 (length (replay-test-store-published store))))
    (is (null (star.databases.couchdb::replay-records-in-document
               (replay-test-store-document store))))))

(test smart-mode-creates-history-without-event-request
  (with-replay-test-store (store)
    (replay-test-write store (replay-test-document) :new)
    (replay-test-write store (replay-test-document :value 2 :mutation "mutation-2") :updated)
    (let* ((records (reverse (replay-test-store-archive store)))
           (last (second records))
           (expected (make-hash-table :test #'equal)))
      (is (= 2 (length records)))
      (is (equal "origin" (jsown:val (first records) "coverage")))
      (is (equal (jsown:val (first records) "hash") (jsown:val last "previous_hash")))
      (setf (gethash (jsown:val last "stream_id") expected)
            (list 2 (jsown:val last "hash")))
      (multiple-value-bind (states heads complete)
          (star.databases.couchdb:replay-event-records
           (append records records) :database "starintel" :tenant "tenant-a"
           :dataset "fixture" :expected-heads expected)
        (declare (ignore heads))
        (is (eq t complete))
        (is (= 1 (hash-table-count states)))
        (let ((state (gethash (jsown:val last "stream_id") states)))
          (is (eq :false (jsown:val (jsown:val state "data") "false")))
          (is (eq :null (jsown:val (jsown:val state "data") "null"))))))))

(test explicit-mode-requires-metadata-before-any-commit
  (with-replay-test-store (store :explicit)
    (signals star.databases.couchdb:event-source-error
      (replay-test-write store (replay-test-document) :new))
    (is (null (replay-test-store-document store)))
    (is (null (replay-test-store-published store)))))

(test explicit-mode-retries-original-event-and-rejects-stale-sequence
  (with-replay-test-store (store :explicit)
    (let ((document (replay-test-document :request (replay-test-request 0))))
      (replay-test-write store document :new)
      (replay-test-write store document :new)
      (is (= 1 (length (replay-test-store-archive store))))
      (is (= 1 (length (replay-test-store-published store)))))
    (signals star.databases.couchdb:event-source-error
      (replay-test-write store
        (replay-test-document :mutation "mutation-2" :request (replay-test-request 0))
        :updated))
    (is (= 1 (length (replay-test-store-archive store))))))

(test smart-mode-does-not-downgrade-unknown-explicit-protocol
  (with-replay-test-store (store)
    (signals star.databases.couchdb:event-source-error
      (replay-test-write store
        (replay-test-document :request (replay-test-request 0 "unknown/7")) :new))
    (is (null (replay-test-store-document store)))))

(test actor-cannot-submit-server-owned-scope-fields
  (with-replay-test-store (store)
    (let ((request (replay-test-request 0)))
      (setf (jsown:val request "tenant") "forged")
      (signals star.databases.couchdb:event-source-error
        (replay-test-write store (replay-test-document :request request) :new)))
    (is (null (replay-test-store-document store)))))

(test archive-failure-retains-committed-event-before-publication
  (with-replay-test-store (store)
    (setf (replay-test-store-fail-archive-p store) t)
    (signals error (replay-test-write store (replay-test-document) :new))
    (is (not (null (replay-test-store-document store))))
    (is (null (replay-test-store-published store)))
    (let* ((entry (first (star.databases.couchdb::document-outbox-entries
                          (replay-test-store-document store))))
           (record (jsown:val entry "replay_record"))
           (identity (jsown:val record "_id")))
      (is (equal "pending" (jsown:val entry "status")))
      (setf (replay-test-store-fail-archive-p store) nil)
      ;; Captured obligations survive disabling capture and a recovery pass.
      (let ((star.databases.couchdb:*event-source-mode* :off))
        (star.databases.couchdb::recover-outbox-documents
         (lambda (id) (replay-test-load store id))
         (lambda (state) (replay-test-save store state))
         (lambda (route payload id) (replay-test-publish store route payload id))
         (list (replay-test-store-document store))))
      (is (= 1 (length (replay-test-store-published store))))
      (is (equal identity (jsown:val (first (replay-test-store-archive store)) "_id"))))))

(test same-mutation-id-with-different-content-conflicts
  (with-replay-test-store (store)
    (replay-test-write store (replay-test-document) :new)
    (signals star.databases.couchdb::mutation-conflict
      (replay-test-write store (replay-test-document :value 99) :new))
    (is (= 1 (length (replay-test-store-archive store))))))

(test disable-reenable-gap-fails-closed
  (with-replay-test-store (store)
    (replay-test-write store (replay-test-document) :new)
    (let ((star.databases.couchdb:*event-source-mode* :off))
      (replay-test-write store (replay-test-document :value 2 :mutation "mutation-2") :updated))
    (signals star.databases.couchdb:event-source-error
      (replay-test-write store (replay-test-document :value 3 :mutation "mutation-3") :updated))))

(test untracked-update-cannot-silently-continue-a-captured-stream
  (with-replay-test-store (store)
    (replay-test-write store (replay-test-document) :new)
    (setf (jsown:val (jsown:val (replay-test-store-document store) "data") "value") 99)
    (signals star.databases.couchdb:event-source-error
      (replay-test-write store (replay-test-document :value 2 :mutation "mutation-2") :updated))))

(test replay-rejects-tampering-and-missing-tail
  (with-replay-test-store (store)
    (replay-test-write store (replay-test-document) :new)
    (let* ((record (first (replay-test-store-archive store)))
           (changed (star.databases.couchdb::clone-outbox-json record))
           (expected (make-hash-table :test #'equal)))
      (setf (gethash (jsown:val record "stream_id") expected) (list 2 "missing-tail"))
      (signals star.databases.couchdb:event-source-error
        (star.databases.couchdb:replay-event-records
         (list record) :database "starintel" :tenant "tenant-a" :dataset "fixture"
         :expected-heads expected))
      (setf (jsown:val changed "state_json") "{}")
      (signals star.databases.couchdb:event-source-error
        (star.databases.couchdb:replay-event-records
         (list changed) :database "starintel" :tenant "tenant-a" :dataset "fixture")))))

(test replay-does-not-call-publication-or-archive-hooks
  (with-replay-test-store (store)
    (replay-test-write store (replay-test-document) :new)
    (let ((star.databases.couchdb::*outbox-before-publish*
            (lambda (&rest arguments) (declare (ignore arguments)) (error "forbidden")))
          (star.databases.couchdb::*event-source-archive-function*
            (lambda (&rest arguments) (declare (ignore arguments)) (error "forbidden"))))
      (multiple-value-bind (states heads complete)
          (star.databases.couchdb:replay-event-records
           (replay-test-store-archive store) :database "starintel" :tenant "tenant-a"
           :dataset "fixture")
        (declare (ignore heads))
        (is (= 1 (hash-table-count states)))
        (is (null complete))))))
