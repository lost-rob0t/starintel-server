(in-package :star-server-tests)

(def-suite drain-resilience-tests
  :description "Recurring re-observation upsert, quarantine idempotency, and
supervised worker restart for backlog drains (issue #38).")

(in-suite drain-resilience-tests)

;;; --- Fixtures -------------------------------------------------------

(defun make-drain-mutation-document (document-id dtype &key (title "first"))
  (let ((document (jsown:empty-object)))
    (setf (jsown:val document "_id") document-id
          (jsown:val document "dtype") dtype
          (jsown:val document "dataset") "drain-test"
          (jsown:val document "schema_version") "0.9.0"
          (jsown:val document "title") title)
    document))

(defun ingest-new-document (existing document)
  "Run one :new mutation through prepare-outbox-mutation."
  (star.databases.couchdb::prepare-outbox-mutation existing document :new))

(defun persist-stub (state)
  "Simulate couchdb-save-outbox-document assigning the storage revision."
  (setf (jsown:val state "_rev") "1-persisted")
  state)

;;; --- Upsert: changed :new re-observation of an existing document -----

(test changed-new-observation-upserts-as-update
  (let* ((document (make-drain-mutation-document "drain:person:1" "person"
                                                 :title "first"))
         (first-state
           (persist-stub (nth-value 0 (ingest-new-document nil document))))
         (changed (make-drain-mutation-document "drain:person:1" "person"
                                                :title "second"))
         (entry nil)
         (state nil))
    (multiple-value-setq (state entry)
      (ingest-new-document first-state changed))
    (is (string= "documents.updated.person"
                 (jsown:val entry "routing_key"))
        "re-observation must publish as an update event")
    (is (string= "updated" (jsown:val entry "operation")))
    (is (< (length (star.databases.couchdb::document-outbox-entries
                    first-state))
           (length (star.databases.couchdb::document-outbox-entries state)))
        "upsert appends a new outbox entry")))

(test identical-new-redelivery-remains-duplicate
  (let* ((document (make-drain-mutation-document "drain:person:2" "person"))
         (first-state
           (persist-stub (nth-value 0 (ingest-new-document nil document)))))
    (multiple-value-bind (state entry disposition)
        (ingest-new-document first-state document)
      (declare (ignore state entry))
      (is (eq :duplicate disposition)
          "identical redelivery must stay an idempotent duplicate"))))

(test changed-new-redelivery-after-upsert-is-duplicate
  (let* ((document (make-drain-mutation-document "drain:person:3" "person"
                                                 :title "first"))
         (first-state
           (persist-stub (nth-value 0 (ingest-new-document nil document))))
         (changed (make-drain-mutation-document "drain:person:3" "person"
                                                :title "second"))
         (upsert-state
           (persist-stub (nth-value 0 (ingest-new-document first-state changed)))))
    (multiple-value-bind (state entry disposition)
        (ingest-new-document upsert-state changed)
      (declare (ignore state entry))
      (is (eq :duplicate disposition)
          "redelivery of the same re-observation must not upsert twice"))))

(test explicit-idempotency-key-reuse-still-conflicts
  (let* ((document
           (jsown:extend-js
               (make-drain-mutation-document "drain:person:4" "person")
             ("extensions"
              (jsown:extend-js (jsown:empty-object)
                ("mutation_id" "drain-key-1")))))
         (first-state
           (persist-stub (nth-value 0 (ingest-new-document nil document))))
         (reused
           (jsown:extend-js
               (make-drain-mutation-document "drain:person:4" "person"
                                             :title "different")
             ("extensions"
              (jsown:extend-js (jsown:empty-object)
                ("mutation_id" "drain-key-1"))))))
    (signals star.databases.couchdb:mutation-conflict
      (ingest-new-document first-state reused))
    "reusing an explicit idempotency key for different content stays a conflict"))

;;; --- Quarantine save idempotency ------------------------------------

(defclass test-conflict-couch-client ()
  ((existing-json :initarg :existing-json :reader test-client-existing-json)))

(defmethod cl-couch:create-document ((client test-conflict-couch-client)
                                     database doc)
  (declare (ignore database doc))
  (error (make-condition 'dexador:http-request-conflict
                         :uri (quri:uri "http://couchdb:5984/starintel")
                         :status 409
                         :body "{\"error\":\"conflict\"}")))

(defmethod cl-couch:get-document ((client test-conflict-couch-client)
                                  database id &optional (revision nil))
  (declare (ignore database revision))
  (format nil "{\"_id\":\"~a\",\"_rev\":\"9-existing\"}" id))

(test quarantine-save-conflict-returns-existing-record
  (let* ((client (make-instance 'test-conflict-couch-client))
         (record (jsown:extend-js (jsown:empty-object)
                   ("_id" "quarantine:01TEST"))))
    (let ((saved
            (star.databases.couchdb:couchdb-save-quarantine-record
             client "starintel" record)))
      (is (string= "9-existing" (jsown:val saved "_rev"))
          "a conflicting quarantine save must return the existing record"))))

;;; --- Supervised worker restart --------------------------------------

(defclass test-supervised-stream ()
  ((script :initarg :script :accessor test-supervised-script)
   (settle-errors :initarg :settle-errors :initform 0
                  :accessor test-supervised-settle-errors)
   (opens :initform 0 :accessor test-supervised-opens)
   (settled :initform 0 :accessor test-supervised-settled)))

(defmethod star.consumers:open-stream ((stream test-supervised-stream))
  (incf (test-supervised-opens stream))
  stream)

(defmethod star.consumers:close-stream ((stream test-supervised-stream))
  stream)

(defmethod star.consumers:stream-read ((stream test-supervised-stream))
  (let ((next (pop (test-supervised-script stream))))
    (cond
      ((null next) (signal 'end-of-file))
      (t next))))

(defmethod star.consumers:stream-settle
    ((stream test-supervised-stream) delivery settlement)
  (declare (ignore delivery))
  (when (plusp (test-supervised-settle-errors stream))
    (decf (test-supervised-settle-errors stream))
    (error "simulated unsettled delivery"))
  (incf (test-supervised-settled stream))
  settlement)

(defun make-supervised-consumer (stream)
  (make-instance
   'star.consumers:consumer
   :name "supervised-test"
   :workers 1
   :test-fn #'identity
   :fn (lambda (consumer message)
         (declare (ignore consumer message))
         (star.consumers:settlement-ack))
   :stream stream))

(test settlement-failure-restarts-worker
  (let* ((stream (make-instance 'test-supervised-stream
                                :script '("d1" "d2" "d3" :eof)
                                :settle-errors 1))
         (consumer (make-supervised-consumer stream)))
    (let ((star.consumers::*retry-sleep-function* (nosleep-counting)))
      (star.consumers:run-consumer consumer))
    (is (= 2 (test-supervised-opens stream))
        "worker must restart after an unsettled delivery")
    (is (= 2 (test-supervised-settled stream))
        "work must keep settling after the restart")
    (is (eq :stopped (star.consumers:consumer-state consumer)))))

(test supervised-restart-is-honest-during-backoff
  (let* ((stream (make-instance 'test-supervised-stream
                                :script '("d1" "d2" :eof)
                                :settle-errors 1))
         (consumer (make-supervised-consumer stream))
         (observed nil))
    (let ((star.consumers::*retry-sleep-function*
            (lambda (seconds)
              (declare (ignore seconds))
              (push (star.consumers:consumer-running-p consumer) observed))))
      (star.consumers:run-consumer consumer))
    (is (member nil observed :test #'eq)
        "readiness must report not-running while the worker is down")))

(test stopping-fences-supervised-restart
  (let* ((stream (make-instance 'test-supervised-stream
                                :script '("d1" "d2" :eof)
                                :settle-errors 99))
         (consumer (make-supervised-consumer stream))
         (stop-during-backoff
           (lambda (seconds)
             (declare (ignore seconds))
             (star.consumers:consumer-update-state consumer :stopping))))
    (let ((star.consumers::*retry-sleep-function* stop-during-backoff))
      (star.consumers:run-consumer consumer))
    (is (= 1 (test-supervised-opens stream))
        "no restart once the consumer is stopping")
    (is (eq :stopped (star.consumers:consumer-state consumer)))))

(defun run-drain-resilience-tests ()
  (run! 'drain-resilience-tests))
