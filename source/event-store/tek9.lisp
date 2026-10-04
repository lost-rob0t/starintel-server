(in-package :star.event-store)

(defparameter +event-records-db+ "event-records")
(defparameter +event-stream-log-db+ "event-stream-log")
(defparameter +event-stream-heads-db+ "event-stream-heads")
(defparameter +event-store-database-names+
  (list +event-records-db+ +event-stream-log-db+ +event-stream-heads-db+))

(defparameter *event-store-name*
  (or (uiop:getenv "STAR_EVENT_STORE_NAME") "starintel-event-source")
  "Logical Tek9 event-store name.")

(defparameter *event-store-path*
  (uiop:ensure-directory-pathname
   (or (uiop:getenv "STAR_EVENT_STORE_PATH")
       "./starintel-event-source/"))
  "Filesystem path of the canonical Tek9 event source.")

(defvar *event-store* nil
  "Current process-owned Tek9 event store.")

(define-condition event-store-error (error)
  ((message :initarg :message :reader event-store-error-message))
  (:report
   (lambda (condition stream)
     (write-string (event-store-error-message condition) stream))))

(define-condition event-store-conflict (event-store-error)
  ((event-id :initarg :event-id :reader event-store-conflict-event-id)
   (existing :initarg :existing :reader event-store-conflict-existing)
   (incoming :initarg :incoming :reader event-store-conflict-incoming)))

(define-condition event-store-version-conflict (event-store-error)
  ((stream-id
    :initarg :stream-id
    :reader event-store-version-conflict-stream-id)
   (expected
    :initarg :expected
    :reader event-store-version-conflict-expected)
   (actual
    :initarg :actual
    :reader event-store-version-conflict-actual)))

(define-condition event-store-gap (event-store-error)
  ((stream-id
    :initarg :stream-id
    :reader event-store-gap-stream-id)
   (expected-sequence
    :initarg :expected-sequence
    :reader event-store-gap-expected-sequence)
   (actual-sequence
    :initarg :actual-sequence
    :reader event-store-gap-actual-sequence)))

(defstruct event-store-record
  event-id
  stream-id
  sequence
  predecessor-id
  event-type
  payload
  payload-digest
  recorded-at
  metadata)

(defstruct event-store-append-result
  status
  event-id
  stream-id
  sequence
  payload-digest)

(defun fail-event-store (control &rest arguments)
  (error 'event-store-error
         :message (apply #'format nil control arguments)))

(defun required-string (value context)
  (unless (and (stringp value) (plusp (length value)))
    (fail-event-store "~A requires a non-empty string." context))
  value)

(defun event-payload-digest (payload)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (babel:string-to-octets payload :encoding :utf-8))))

(defun stream-key-prefix (stream-id)
  (let ((octet-length
          (length (babel:string-to-octets stream-id :encoding :utf-8))))
    (format nil "~8,'0X:~A:" octet-length stream-id)))

(defun stream-sequence-key (stream-id sequence)
  (format nil "~A~20,'0D" (stream-key-prefix stream-id) sequence))

(defun stream-sequence-end-key (stream-id sequence)
  (stream-sequence-key stream-id sequence))

(defun record->plist (record)
  (list :event-id (event-store-record-event-id record)
        :stream-id (event-store-record-stream-id record)
        :sequence (event-store-record-sequence record)
        :predecessor-id (event-store-record-predecessor-id record)
        :event-type (event-store-record-event-type record)
        :payload (event-store-record-payload record)
        :payload-digest (event-store-record-payload-digest record)
        :recorded-at (event-store-record-recorded-at record)
        :metadata (copy-tree (event-store-record-metadata record))))

(defun plist->record (value)
  (when value
    (make-event-store-record
     :event-id (getf value :event-id)
     :stream-id (getf value :stream-id)
     :sequence (getf value :sequence)
     :predecessor-id (getf value :predecessor-id)
     :event-type (getf value :event-type)
     :payload (getf value :payload)
     :payload-digest (getf value :payload-digest)
     :recorded-at (getf value :recorded-at)
     :metadata (copy-tree (getf value :metadata)))))

(defun same-logical-event-p (stored stream-id event-type payload digest metadata)
  (and (string= (getf stored :stream-id) stream-id)
       (string= (getf stored :event-type) event-type)
       (string= (getf stored :payload-digest) digest)
       (string= (getf stored :payload) payload)
       (equal (getf stored :metadata) metadata)))

(defun event-store-open-p (&optional (store *event-store*))
  (and store (tek9:db-is-open-p store)))

(defun open-event-store (&key
                           (path *event-store-path*)
                           (name *event-store-name*))
  "Open and return the process-owned Tek9 event source.

The store uses full LMDB durability. Repeated calls are idempotent while the
same process-owned store remains open."
  (when (event-store-open-p)
    (return-from open-event-store *event-store*))
  (let* ((directory (uiop:ensure-directory-pathname path))
         (database
           (tek9:new-database
            name
            :path directory
            :durability :full
            :max-dbs 16
            :index-definitions nil)))
    (tek9:open-database database)
    ;; Pre-open fixed DBIs before any explicit transaction uses them.
    (dolist (database-name +event-store-database-names+)
      (tek9:database-db database database-name
                        :key-encoding :utf-8
                        :value-encoding :octets))
    (setf *event-store* database)))

(defun close-event-store (&optional (store *event-store*))
  "Close STORE and clear the process-owned event-store reference."
  (when (and store (tek9:db-is-open-p store))
    (tek9:close-database store))
  (when (eq store *event-store*)
    (setf *event-store* nil))
  t)

(defun require-open-store (&optional (store *event-store*))
  (unless (event-store-open-p store)
    (fail-event-store "StarIntel event store is not open."))
  store)

(defun fetch-event (event-id &key (store *event-store*))
  "Fetch EVENT-ID from the canonical event source."
  (required-string event-id "event id")
  (let ((database (require-open-store store)))
    (plist->record
     (tek9:fetch* database event-id :database-name +event-records-db+))))

(defun event-store-stream-head (stream-id &key (store *event-store*))
  "Return a copy of STREAM-ID's head metadata plist, or NIL."
  (required-string stream-id "stream id")
  (let* ((database (require-open-store store))
         (head
           (tek9:fetch* database stream-id
                        :database-name +event-stream-heads-db+)))
    (and head (copy-tree head))))

(defun make-replay-result (status record)
  (make-event-store-append-result
   :status status
   :event-id (event-store-record-event-id record)
   :stream-id (event-store-record-stream-id record)
   :sequence (event-store-record-sequence record)
   :payload-digest (event-store-record-payload-digest record)))

(defun append-event (event-id stream-id event-type payload
                     &key
                       (metadata nil)
                       (recorded-at (spec:unix-now))
                       (store *event-store*)
                       (expected-sequence nil expected-sequence-p)
                       (expected-predecessor nil expected-predecessor-p))
  "Atomically append one immutable event to STREAM-ID.

EVENT-ID is globally idempotent. Reusing it with byte-identical logical content
returns :REPLAYED even after the stream has advanced. Reusing it with different
content signals EVENT-STORE-CONFLICT.

EXPECTED-SEQUENCE and EXPECTED-PREDECESSOR implement optimistic stream-version
checks for protocol-aware callers. The event record, ordered stream row and head
advance commit in one Tek9/LMDB transaction."
  (required-string event-id "event id")
  (required-string stream-id "stream id")
  (required-string event-type "event type")
  (unless (stringp payload)
    (fail-event-store "event payload must be a string."))
  (when (and expected-sequence-p
             (or (not (integerp expected-sequence))
                 (minusp expected-sequence)))
    (fail-event-store "expected sequence must be a non-negative integer."))
  (let* ((database (require-open-store store))
         (digest (event-payload-digest payload))
         (metadata (copy-tree metadata)))
    (tek9:with-write-transaction
        (database :database-names
                  '("event-records" "event-stream-log" "event-stream-heads"))
      (let ((existing
              (tek9:fetch* database event-id
                           :database-name +event-records-db+)))
        (when existing
          (if (same-logical-event-p
               existing stream-id event-type payload digest metadata)
              (return-from append-event
                (make-replay-result :replayed (plist->record existing)))
              (error 'event-store-conflict
                     :message (format nil
                                      "Event id ~A already names different content."
                                      event-id)
                     :event-id event-id
                     :existing (copy-tree existing)
                     :incoming
                     (list :stream-id stream-id
                           :event-type event-type
                           :payload payload
                           :payload-digest digest
                           :metadata metadata))))
        (let* ((head
                 (tek9:fetch* database stream-id
                              :database-name +event-stream-heads-db+))
               (current-sequence (and head (getf head :sequence)))
               (current-event-id (and head (getf head :event-id)))
               (next-sequence (if head (1+ current-sequence) 0)))
          (when (and expected-sequence-p
                     (/= expected-sequence next-sequence))
            (error 'event-store-version-conflict
                   :message
                   (format nil
                           "Stream ~A expected sequence ~D, actual next sequence is ~D."
                           stream-id expected-sequence next-sequence)
                   :stream-id stream-id
                   :expected expected-sequence
                   :actual next-sequence))
          (when (and expected-predecessor-p
                     (not (equal expected-predecessor current-event-id)))
            (error 'event-store-version-conflict
                   :message
                   (format nil
                           "Stream ~A predecessor mismatch: expected ~S, actual ~S."
                           stream-id expected-predecessor current-event-id)
                   :stream-id stream-id
                   :expected expected-predecessor
                   :actual current-event-id))
          (let* ((record
                   (make-event-store-record
                    :event-id event-id
                    :stream-id stream-id
                    :sequence next-sequence
                    :predecessor-id current-event-id
                    :event-type event-type
                    :payload payload
                    :payload-digest digest
                    :recorded-at recorded-at
                    :metadata metadata))
                 (record-plist (record->plist record)))
            (tek9:put* database record-plist
                       :id event-id
                       :database-name +event-records-db+)
            (tek9:put* database event-id
                       :id (stream-sequence-key stream-id next-sequence)
                       :database-name +event-stream-log-db+)
            (tek9:put* database
                       (list :stream-id stream-id
                             :sequence next-sequence
                             :event-id event-id
                             :payload-digest digest)
                       :id stream-id
                       :database-name +event-stream-heads-db+)
            (make-replay-result :appended record)))))))

(defun event-store-record-at-sequence (stream-id sequence
                                       &key (store *event-store*))
  "Return STREAM-ID event at SEQUENCE, or NIL."
  (required-string stream-id "stream id")
  (unless (and (integerp sequence) (not (minusp sequence)))
    (fail-event-store "sequence must be a non-negative integer."))
  (let* ((database (require-open-store store))
         (event-id
           (tek9:fetch* database
                        (stream-sequence-key stream-id sequence)
                        :database-name +event-stream-log-db+)))
    (and event-id (fetch-event event-id :store database))))

(defun signal-gap (stream-id expected actual)
  (error 'event-store-gap
         :message
         (format nil "Event stream ~A has a gap at sequence ~D (actual ~S)."
                 stream-id expected actual)
         :stream-id stream-id
         :expected-sequence expected
         :actual-sequence actual))

(defun replay-event-stream (stream-id
                            &key
                              (from-sequence 0)
                              (limit 0)
                              (store *event-store*)
                              (verify-continuity t))
  "Read STREAM-ID in deterministic sequence order.

When VERIFY-CONTINUITY is true, missing rows, missing event bodies, sequence
jumps, cross-stream records and predecessor mismatches signal EVENT-STORE-GAP.
A LIMIT of zero means the complete tail through the recorded stream head."
  (required-string stream-id "stream id")
  (unless (and (integerp from-sequence) (not (minusp from-sequence)))
    (fail-event-store "from-sequence must be a non-negative integer."))
  (unless (and (integerp limit) (not (minusp limit)))
    (fail-event-store "limit must be a non-negative integer."))
  (let* ((database (require-open-store store))
         (head (event-store-stream-head stream-id :store database)))
    (unless head
      (return-from replay-event-stream nil))
    (let ((head-sequence (getf head :sequence)))
      (when (> from-sequence head-sequence)
        (return-from replay-event-stream nil))
      (let* ((rows
               (tek9:select-primary-range
                database
                (stream-sequence-key stream-id from-sequence)
                :end (stream-sequence-end-key stream-id head-sequence)
                :database-name +event-stream-log-db+
                :limit limit))
             (expected from-sequence)
             (previous-id
               (when (plusp from-sequence)
                 (let ((previous
                         (event-store-record-at-sequence
                          stream-id (1- from-sequence) :store database)))
                   (when (and verify-continuity (null previous))
                     (signal-gap stream-id (1- from-sequence) nil))
                   (and previous
                        (event-store-record-event-id previous)))))
             records)
        (dolist (row rows)
          (let* ((event-id (cdr row))
                 (record (and event-id (fetch-event event-id :store database))))
            (when (and verify-continuity (null record))
              (signal-gap stream-id expected nil))
            (when record
              (let ((actual (event-store-record-sequence record)))
                (when (and verify-continuity (/= actual expected))
                  (signal-gap stream-id expected actual))
                (when (and verify-continuity
                           (not (string=
                                 stream-id
                                 (event-store-record-stream-id record))))
                  (signal-gap stream-id expected actual))
                (when (and verify-continuity
                           (not (equal previous-id
                                       (event-store-record-predecessor-id record))))
                  (signal-gap stream-id expected actual))
                (push record records)
                (setf previous-id (event-store-record-event-id record))
                (incf expected)))))
        (when (and verify-continuity
                   (zerop limit)
                   (<= expected head-sequence))
          (signal-gap stream-id expected nil))
        (nreverse records)))))

(defun verify-event-stream (stream-id &key (store *event-store*))
  "Verify complete continuity of STREAM-ID and return its head metadata."
  (replay-event-stream stream-id :store store :verify-continuity t)
  (event-store-stream-head stream-id :store store))
