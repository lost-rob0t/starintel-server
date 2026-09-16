(in-package :star.databases.couchdb)

(defvar *event-source-mode* :off
  "OFF preserves legacy writes; SMART captures them; EXPLICIT requires metadata.")
(defvar *event-source-max-state-bytes* (* 1024 1024))
(defvar *event-source-max-records* 10000)
(defparameter +replay-protocol+ "starintel.event-source/1")
(defparameter +replay-entry-key+ "replay_record")
(defparameter +replay-request-key+ "event_source_request")
(defparameter +replay-hash-fields+
  '("protocol" "database" "tenant" "dataset" "document_id" "stream_id"
    "event_id" "mutation_id" "sequence" "previous_hash" "coverage"
    "operation" "recorded_at" "causation_id" "state_json"))

(define-condition event-source-error (error)
  ((code :initarg :code :reader event-source-error-code))
  (:report (lambda (condition stream)
             (format stream "Event source rejected operation: ~a"
                     (event-source-error-code condition)))))

(defun replay-fail (code)
  (error 'event-source-error :code code))

(defun replay-value (object key &optional default)
  (outbox-object-value object key default))

(defun replay-string (object key &key (empty nil))
  (let ((value (replay-value object key)))
    (unless (and (stringp value) (or empty (plusp (length value))))
      (replay-fail :invalid-string))
    value))

(defun replay-array-digest (values)
  "SHA-256 of length-prefixed typed UTF-8 scalars, not a JSON reserialization.
Each string is s+value and each integer is i+decimal; prefix its UTF-8 byte
length and a colon. Callers include a domain tag as the first scalar."
  (outbox-digest-string
   (with-output-to-string (stream)
     (dolist (value values)
       (let ((text (cond ((stringp value) (concatenate 'string "s" value))
                         ((integerp value) (format nil "i~d" value))
                         (t (replay-fail :invalid-hash-scalar)))))
         (format stream "~d:~a"
                 (length (babel:string-to-octets text :encoding :utf-8)) text))))))

(defun replay-record-hash (record)
  (replay-array-digest
   (cons "record/v1"
         (mapcar (lambda (key) (replay-value record key)) +replay-hash-fields+))))

(defun replay-stream-id (database tenant dataset document-id)
  (replay-array-digest (list "stream/v1" database tenant dataset document-id)))

(defun replay-record-id (stream-id mutation-id)
  (concatenate 'string "replay:"
               (replay-array-digest (list "record-id/v1" stream-id mutation-id))))

(defun replay-records-in-document (document)
  (when document
    (loop for entry in (document-outbox-entries document)
          for record = (replay-value entry +replay-entry-key+)
          when record collect record)))

(defun validate-event-source-request (document)
  "Validate actor hints only. Scope, sequence and state stay server-owned."
  (unless (member *event-source-mode* '(:off :smart :explicit))
    (replay-fail :invalid-mode))
  (let* ((extensions (document-extensions document))
         (present (outbox-object-has-key-p extensions +replay-request-key+))
         (request (replay-value extensions +replay-request-key+)))
    (when (and (eq *event-source-mode* :explicit) (not present))
      (replay-fail :explicit-request-required))
    (when present
      (unless (json-object-p request) (replay-fail :invalid-request))
      (jsown:do-json-keys (key value) request
        (unless (member key '("protocol" "expected_sequence" "causation_id")
                        :test #'string=)
          (replay-fail :unknown-request-field)))
      (unless (equal (replay-value request "protocol") +replay-protocol+)
        (replay-fail :unsupported-protocol))
      (unless (typep (replay-value request "expected_sequence") '(integer 0 *))
        (replay-fail :invalid-expected-sequence))
      (unless (explicit-mutation-id document)
        (replay-fail :mutation-id-required))
      (when (outbox-object-has-key-p request "causation_id")
        (unless (<= (length (replay-string request "causation_id")) 256)
          (replay-fail :causation-id-too-long))))
    request))

(defun replay-public-document-copy (document)
  "Remove persistence/private extension state from the deterministic projection."
  (let* ((copy (public-document-copy document))
         (extensions (document-extensions copy))
         (private-keys nil))
    (jsown:do-json-keys (key value) extensions
      (when (and (>= (length key) 8) (string= "_server_" key :end2 8))
        (push key private-keys)))
    (setf (jsown:val copy "extensions")
          (copy-json-object-excluding extensions private-keys))
    copy))

(defun decorate-replay-entry (existing document entry)
  "Add immutable replay evidence BEFORE the existing atomic outbox CAS.
A duplicate returns from PREPARE-OUTBOX-MUTATION before this hook: its original
record remains authoritative even after later mutations advance the stream."
  (when (eq *event-source-mode* :off)
    (return-from decorate-replay-entry entry))
  (let* ((request (validate-event-source-request document))
         (database *outbox-source-database*)
         (tenant (replay-value document "tenant_id" ""))
         (dataset (replay-string document "dataset"))
         (document-id (replay-string document "_id"))
         (sequence (outbox-entry-sequence entry))
         (history (replay-records-in-document existing))
         (previous (car (last history)))
         (record (jsown:empty-object))
         (state-json (jsown:to-json (replay-public-document-copy document))))
    (unless (and (stringp database) (plusp (length database)) (stringp tenant))
      (replay-fail :missing-server-database-context))
    (when existing
      (dolist (key '("_id" "dataset" "tenant_id"))
        (unless (equal (replay-value existing key) (replay-value document key))
          (replay-fail :stream-scope-changed))))
    (when (and request
               (/= (replay-value request "expected_sequence") (1- sequence)))
      (replay-fail :stale-expected-sequence))
    (when previous
      (validate-replay-record previous)
      (unless (and (= (1+ (replay-value previous "sequence")) sequence)
                   (equal (replay-value previous "state_json")
                          (jsown:to-json (replay-public-document-copy existing))))
        (replay-fail :capture-gap-requires-baseline)))
    (when (>= (length history) *event-source-max-records*)
      (replay-fail :history-limit))
    (when (> (length (babel:string-to-octets state-json :encoding :utf-8))
             *event-source-max-state-bytes*)
      (replay-fail :state-too-large))
    (let* ((stream-id (replay-stream-id database tenant dataset document-id))
           (mutation-id (replay-string entry "mutation_id")))
      (setf (jsown:val record "_id") (replay-record-id stream-id mutation-id)
            (jsown:val record "protocol") +replay-protocol+
            (jsown:val record "database") database
            (jsown:val record "tenant") tenant
            (jsown:val record "dataset") dataset
            (jsown:val record "document_id") document-id
            (jsown:val record "stream_id") stream-id
            (jsown:val record "event_id") (replay-string entry "event_id")
            (jsown:val record "mutation_id") mutation-id
            (jsown:val record "sequence") sequence
            (jsown:val record "previous_hash")
            (if previous (replay-string previous "hash") "")
            (jsown:val record "coverage")
            (cond (previous "continuation")
                  ((and (null existing) (= sequence 1)) "origin")
                  (t "baseline"))
            (jsown:val record "operation") (replay-string entry "operation")
            (jsown:val record "recorded_at") (replay-value entry "created_at")
            (jsown:val record "causation_id")
            (if request (replay-value request "causation_id" "") "")
            (jsown:val record "state_json") state-json
            (jsown:val record "hash") (replay-record-hash record)
            (jsown:val entry +replay-entry-key+) record))
    entry))

(defun validate-replay-record (record)
  "Reject unknown protocol, malformed identity, changed bytes or unknown effects."
  (unless (json-object-p record) (replay-fail :invalid-record))
  (jsown:do-json-keys (key value) record
    (unless (member key (append +replay-hash-fields+ '("_id" "_rev" "hash"))
                    :test #'string=)
      (replay-fail :unknown-record-field)))
  (unless (equal (replay-value record "protocol") +replay-protocol+)
    (replay-fail :unsupported-protocol))
  (dolist (key '("_id" "database" "dataset" "document_id" "stream_id"
                 "event_id" "mutation_id" "hash"))
    (replay-string record key))
  (dolist (key '("tenant" "previous_hash" "causation_id" "state_json" "coverage" "operation"))
    (replay-string record key :empty t))
  (unless (or (integerp (replay-value record "recorded_at"))
              (stringp (replay-value record "recorded_at")))
    (replay-fail :invalid-recorded-at))
  (unless (and (typep (replay-value record "sequence") '(integer 1 *))
               (member (replay-value record "operation")
                       '("new" "updated" "deleted") :test #'equal)
               (member (replay-value record "coverage")
                       '("origin" "baseline" "continuation") :test #'equal))
    (replay-fail :invalid-transition))
  (unless (and
           (equal (replay-value record "stream_id")
                  (replay-stream-id (replay-value record "database")
                                    (replay-value record "tenant")
                                    (replay-value record "dataset")
                                    (replay-value record "document_id")))
           (equal (replay-value record "_id")
                  (replay-record-id (replay-value record "stream_id")
                                    (replay-value record "mutation_id")))
           (equal (replay-value record "hash") (replay-record-hash record)))
    (replay-fail :checksum-or-identity-mismatch))
  (when (> (length (babel:string-to-octets (replay-value record "state_json")
                                          :encoding :utf-8))
           *event-source-max-state-bytes*)
    (replay-fail :state-too-large))
  record)

(defun couchdb-archive-replay-record (record)
  "Insert-only archive of an already committed outbox entry, on its owned client.
Do not acquire another pool connection while the caller holds one. A conflict
counts as a duplicate only after checking the complete immutable record."
  (validate-replay-record record)
  (unless *outbox-store-client* (replay-fail :missing-store-client))
  (let ((database star::*couchdb-event-log-database*))
    (when (equal database (replay-value record "database"))
      (replay-fail :archive-is-projection-database))
    (handler-case
        (cl-couch:create-document *outbox-store-client* database
                                  (jsown:to-json
                                   (copy-json-object-excluding record '("_rev"))))
      (dexador:http-request-conflict ()
        (let ((stored (couchdb-load-outbox-document
                       *outbox-store-client* database (replay-value record "_id"))))
          (validate-replay-record stored)
          (unless (every (lambda (key)
                           (equal (replay-value stored key) (replay-value record key)))
                         (append +replay-hash-fields+ '("_id" "hash")))
            (replay-fail :archive-identity-conflict))))))
  record)

(defparameter *event-source-archive-function* #'couchdb-archive-replay-record)

(defun archive-replay-entry (entry)
  "Honor already committed capture obligations even if capture is later disabled."
  (let ((record (replay-value entry +replay-entry-key+)))
    (when record (funcall *event-source-archive-function* record))))

(defun replay-record-state (record)
  (let ((document
          (jsown:with-injective-reader
            (jsown:parse (replay-value record "state_json")))))
    (unless (and (json-object-p document)
                 (equal (replay-value document "_id")
                        (replay-value record "document_id"))
                 (equal (replay-value document "dataset")
                        (replay-value record "dataset"))
                 (equal (replay-value document "tenant_id" "")
                        (replay-value record "tenant"))
                 (not (outbox-object-has-key-p document "_rev")))
      (replay-fail :state-scope-mismatch))
    (jsown:do-json-keys (key value) (document-extensions document)
        (when (and (>= (length key) 8) (string= "_server_" key :end2 8))
        (replay-fail :private-state-in-projection)))
    document))

(defun verify-replay-heads (heads expected)
  (unless (and (hash-table-p expected) (= (hash-table-count heads)
                                         (hash-table-count expected)))
    (replay-fail :head-manifest-mismatch))
  (maphash (lambda (stream head)
             (unless (equal head (gethash stream expected))
               (replay-fail :head-manifest-mismatch))) heads)
  t)

(defun replay-event-records (records &key database tenant dataset expected-heads
                                         allow-baselines
                                         (max-records *event-source-max-records*))
  "Pure, bounded shadow projection. Never publishes or calls a live actor.
Returns state-table, heads-table and COMPLETE-P. Each head is (sequence hash).
COMPLETE-P requires an independent expected-head manifest and no baseline gaps;
it concerns ONLY the supplied scope, never server-wide write-path coverage.
Input must already be ordered per stream. Do not sort away evidence of disorder."
  (unless (and (stringp database) (stringp tenant) (stringp dataset)
               (typep max-records '(integer 1 *)))
    (replay-fail :invalid-replay-scope))
  (when (> (length records) max-records) (replay-fail :replay-limit))
  (let ((states (make-hash-table :test #'equal))
        (heads (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (baseline-p nil))
    (map nil
         (lambda (record)
           (validate-replay-record record)
           (unless (and (equal database (replay-value record "database"))
                        (equal tenant (replay-value record "tenant"))
                        (equal dataset (replay-value record "dataset")))
             (replay-fail :replay-scope-mismatch))
           (let* ((id (replay-value record "_id"))
                  (hash (replay-value record "hash"))
                  (known (gethash id seen))
                  (stream (replay-value record "stream_id"))
                  (head (gethash stream heads))
                  (sequence (replay-value record "sequence"))
                  (previous (replay-value record "previous_hash"))
                  (coverage (replay-value record "coverage")))
             (when (and known (not (equal known hash)))
               (replay-fail :duplicate-id-conflict))
             (unless known
               (if head
                   (unless (and (= sequence (1+ (first head)))
                                (equal previous (second head))
                                (equal coverage "continuation"))
                     (replay-fail :sequence-or-predecessor-gap))
                   (unless (and (equal previous "")
                                (or (and (= sequence 1) (equal coverage "origin")
                                         (equal (replay-value record "operation") "new"))
                                    (and allow-baselines (equal coverage "baseline")
                                         (setf baseline-p t))))
                     (replay-fail :missing-origin)))
               (when head
                 (let ((operation (replay-value record "operation")))
                   (when (and (equal operation "new") (gethash stream states))
                     (replay-fail :new-over-existing-state))
                   (when (and (member operation '("updated" "deleted") :test #'equal)
                              (not (gethash stream states)))
                     (replay-fail :missing-live-state))))
               (if (equal (replay-value record "operation") "deleted")
                   (progn
                     (unless (equal (replay-value record "state_json") "")
                       (replay-fail :invalid-tombstone))
                     (remhash stream states))
                   (setf (gethash stream states) (replay-record-state record)))
               (setf (gethash id seen) hash
                     (gethash stream heads) (list sequence hash)))))
         records)
    (when expected-heads (verify-replay-heads heads expected-heads))
    (values states heads (and expected-heads (not baseline-p) t))))

;; Passive hooks; OFF allocates no archive client or worker and records no events.
(setf *outbox-entry-decorator* #'decorate-replay-entry
      *outbox-before-publish* #'archive-replay-entry)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*event-source-mode* *event-source-max-state-bytes*
            *event-source-max-records* event-source-error event-source-error-code
            replay-event-records) :star.databases.couchdb))
