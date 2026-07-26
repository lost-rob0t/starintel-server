(in-package :star.databases.couchdb)

(defparameter +outbox-extension-key+ "_server_outbox")
(defparameter +mutation-ledger-extension-key+ "_server_mutations")
(defparameter +mutation-id-extension-key+ "mutation_id")
(defparameter +idempotency-key-extension-key+ "idempotency_key")

(define-condition mutation-conflict (error)
  ((mutation-id
    :initarg :mutation-id
    :reader mutation-conflict-id)
   (document-id
    :initarg :document-id
    :reader mutation-conflict-document-id)
   (reason
    :initarg :reason
    :reader mutation-conflict-reason))
  (:report
   (lambda (condition stream)
     (format stream
             "Mutation ~a conflicts for document ~a: ~a"
             (mutation-conflict-id condition)
             (mutation-conflict-document-id condition)
             (mutation-conflict-reason condition)))))

(define-condition outbox-store-conflict (error) ())

(define-condition missing-document-for-update (error)
  ((document-id
    :initarg :document-id
    :reader missing-update-document-id))
  (:report
   (lambda (condition stream)
     (format stream
             "Cannot apply update mutation: document ~a does not exist"
             (missing-update-document-id condition)))))

(defun outbox-object-has-key-p (object key)
  (handler-case
      (progn
        (jsown:val object key)
        t)
    (error () nil)))

(defun outbox-object-value (object key &optional default)
  (if (and object (outbox-object-has-key-p object key))
      (jsown:val object key)
      default))

(defun json-object-p (value)
  (and (consp value)
       (eq (first value) :obj)))

(defun clone-outbox-json (object)
  (jsown:with-injective-reader
    (jsown:parse (jsown:to-json object))))

(defun copy-json-object-excluding (object excluded-keys)
  (let ((copy (jsown:empty-object)))
    (when object
      (jsown:do-json-keys (key value) object
        (unless (member key excluded-keys :test #'string=)
          (setf (jsown:val copy key) value))))
    copy))

(defun document-extensions (document)
  (let ((extensions
          (outbox-object-value document "extensions" (jsown:empty-object))))
    (unless (json-object-p extensions)
      (error "Document extensions must be a JSON object"))
    extensions))

(defun public-document-copy (document)
  "Return DOCUMENT without CouchDB revision or server-private outbox state."
  (let* ((source (clone-outbox-json document))
         (public (copy-json-object-excluding source '("_rev")))
         (extensions
           (copy-json-object-excluding
            (document-extensions source)
            (list +outbox-extension-key+
                  +mutation-ledger-extension-key+))))
    (setf (jsown:val public "extensions") extensions)
    public))

(defun outbox-digest-string (text)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (babel:string-to-octets text :encoding :utf-8))))

(defun mutation-content-hash (operation document)
  (outbox-digest-string
   (format nil
           "~(~a~)|~a"
           operation
           (jsown:to-json (public-document-copy document)))))

(defun explicit-mutation-id (document)
  (let* ((extensions (document-extensions document))
         (value
           (or (outbox-object-value extensions +mutation-id-extension-key+)
               (outbox-object-value extensions +idempotency-key-extension-key+))))
    (and (stringp value)
         (> (length value) 0)
         value)))

(defun document-mutation-id (operation document)
  (or (explicit-mutation-id document)
      (mutation-content-hash operation document)))

(defun outbox-event-id (mutation-id)
  (outbox-digest-string (format nil "event|~a" mutation-id)))

(defun operation-event-name (operation)
  (ecase operation
    (:new "new")
    (:updated "updated")))

(defun operation-routing-key (operation dtype)
  (format nil
          "documents.~a.~a"
          (operation-event-name operation)
          (star.documents:canonical-dtype dtype)))

(defun sequence-list (value)
  (cond
    ((null value) nil)
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t
     (error "Expected JSON array, got ~s" value))))

(defun document-outbox-entries (document)
  (sequence-list
   (outbox-object-value
    (document-extensions document)
    +outbox-extension-key+
    nil)))

(defun document-mutation-ledger (document)
  (let ((ledger
          (outbox-object-value
           (document-extensions document)
           +mutation-ledger-extension-key+
           (jsown:empty-object))))
    (unless (json-object-p ledger)
      (error "Document mutation ledger must be a JSON object"))
    ledger))

(defun outbox-entry-mutation-id (entry)
  (jsown:val entry "mutation_id"))

(defun outbox-entry-sequence (entry)
  (jsown:val entry "sequence"))

(defun outbox-entry-published-p (entry)
  (and entry
       (string= "published" (jsown:val entry "status"))))

(defun find-outbox-entry (document mutation-id)
  (find mutation-id
        (document-outbox-entries document)
        :key #'outbox-entry-mutation-id
        :test #'string=))

(defun next-outbox-sequence (document)
  (1+
   (loop for entry in (document-outbox-entries document)
         maximize (outbox-entry-sequence entry) into maximum
         finally (return (or maximum 0)))))

(defun event-payload (document mutation-id operation sequence)
  (let* ((payload (public-document-copy document))
         (extensions (document-extensions payload)))
    (setf (jsown:val extensions "event_id")
          (outbox-event-id mutation-id)
          (jsown:val extensions "mutation_id")
          mutation-id
          (jsown:val extensions "event_operation")
          (operation-event-name operation)
          (jsown:val extensions "event_sequence")
          sequence
          (jsown:val payload "extensions")
          extensions)
    payload))

(defun make-outbox-entry (document mutation-id content-hash operation sequence)
  (let ((entry (jsown:empty-object)))
    (setf (jsown:val entry "event_id")
          (outbox-event-id mutation-id)
          (jsown:val entry "mutation_id")
          mutation-id
          (jsown:val entry "content_hash")
          content-hash
          (jsown:val entry "document_id")
          (jsown:val document "_id")
          (jsown:val entry "sequence")
          sequence
          (jsown:val entry "operation")
          (operation-event-name operation)
          (jsown:val entry "routing_key")
          (operation-routing-key operation (jsown:val document "dtype"))
          (jsown:val entry "status")
          "pending"
          (jsown:val entry "created_at")
          (star.documents:utc-now)
          (jsown:val entry "published_at")
          :null
          (jsown:val entry "payload")
          (event-payload document mutation-id operation sequence))
    entry))

(defun merge-server-state (target existing entry mutation-id content-hash)
  (let* ((target-extensions (document-extensions target))
         (existing-entries
           (if existing
               (document-outbox-entries existing)
               nil))
         (ledger
           (if existing
               (clone-outbox-json (document-mutation-ledger existing))
               (jsown:empty-object))))
    (setf (jsown:val ledger mutation-id)
          content-hash
          (jsown:val target-extensions +mutation-ledger-extension-key+)
          ledger
          (jsown:val target-extensions +outbox-extension-key+)
          (coerce (append existing-entries (list entry)) 'vector)
          (jsown:val target "extensions")
          target-extensions)
    target))

(defun prepare-outbox-mutation (existing incoming operation)
  "Prepare one atomic document revision containing document and pending event state.

Returns STATE, ENTRY, and either :CREATED or :DUPLICATE."
  (let* ((document (public-document-copy incoming))
         (document-id (jsown:val document "_id"))
         (mutation-id (document-mutation-id operation document))
         (content-hash (mutation-content-hash operation document)))
    (when (and (eq operation :updated)
               (null existing))
      (error 'missing-document-for-update
             :document-id document-id))
    (when existing
      (let ((known-hash
              (outbox-object-value
               (document-mutation-ledger existing)
               mutation-id)))
        (when known-hash
          (unless (string= known-hash content-hash)
            (error 'mutation-conflict
                   :mutation-id mutation-id
                   :document-id document-id
                   :reason "idempotency key was reused for different content"))
          (let ((entry (find-outbox-entry existing mutation-id)))
            (unless entry
              (error "Mutation ledger contains ~a without an outbox entry"
                     mutation-id))
            (return-from prepare-outbox-mutation
              (values existing entry :duplicate)))))
      (when (eq operation :new)
        (error 'mutation-conflict
               :mutation-id mutation-id
               :document-id document-id
               :reason "new-document mutation conflicts with an existing document")))
    (when existing
      (setf (jsown:val document "_rev")
            (jsown:val existing "_rev")))
    (let* ((sequence (next-outbox-sequence (or existing document)))
           (entry
             (make-outbox-entry
              document
              mutation-id
              content-hash
              operation
              sequence)))
      (values
       (merge-server-state
        document
        existing
        entry
        mutation-id
        content-hash)
       entry
       :created))))

(defun persist-outbox-mutation (load-fn save-fn incoming operation
                                &key (max-attempts 8))
  (let ((document-id (jsown:val incoming "_id")))
    (loop for attempt from 1 to max-attempts
          do
             (let ((existing (funcall load-fn document-id)))
               (multiple-value-bind (state entry disposition)
                   (prepare-outbox-mutation existing incoming operation)
                 (when (eq disposition :duplicate)
                   (return (values state entry disposition)))
                 (handler-case
                     (return
                       (values
                        (funcall save-fn state)
                        entry
                        disposition))
                   (outbox-store-conflict ()
                     (when (= attempt max-attempts)
                       (error
                        "Outbox persistence conflict retry budget exhausted for ~a"
                        document-id)))))))))

(defun update-outbox-entry (document mutation-id updater)
  (let* ((copy (clone-outbox-json document))
         (extensions (document-extensions copy))
         (entries (document-outbox-entries copy))
         (found nil)
         (updated
           (mapcar
            (lambda (entry)
              (if (string= mutation-id
                           (outbox-entry-mutation-id entry))
                  (progn
                    (setf found t)
                    (funcall updater entry))
                  entry))
            entries)))
    (unless found
      (error "Outbox entry ~a not found" mutation-id))
    (setf (jsown:val extensions +outbox-extension-key+)
          (coerce updated 'vector)
          (jsown:val copy "extensions")
          extensions)
    copy))

(defun mark-outbox-published (load-fn save-fn document-id mutation-id
                              &key (max-attempts 8))
  (loop for attempt from 1 to max-attempts
        do
           (let* ((current
                    (or (funcall load-fn document-id)
                        (error
                         "Document ~a disappeared while marking outbox"
                         document-id)))
                  (entry (find-outbox-entry current mutation-id)))
             (unless entry
               (error "Outbox entry ~a not found" mutation-id))
             (when (outbox-entry-published-p entry)
               (return current))
             (let ((updated
                     (update-outbox-entry
                      current
                      mutation-id
                      (lambda (outbox-entry)
                        (setf (jsown:val outbox-entry "status")
                              "published"
                              (jsown:val outbox-entry "published_at")
                              (star.documents:utc-now))
                        outbox-entry))))
               (handler-case
                   (return (funcall save-fn updated))
                 (outbox-store-conflict ()
                   (when (= attempt max-attempts)
                     (error
                      "Outbox publication marker retry budget exhausted for ~a"
                      mutation-id))))))))

(defun publish-outbox-entry (publish-fn entry)
  (funcall publish-fn
           (jsown:val entry "routing_key")
           (jsown:val entry "payload")
           (jsown:val entry "event_id")))

(defun process-outbox-mutation (load-fn save-fn publish-fn incoming operation)
  "Persist mutation state, publish at least once, then mark the logical event.

If publication fails, the durable pending entry remains recoverable."
  (multiple-value-bind (state entry disposition)
      (persist-outbox-mutation load-fn save-fn incoming operation)
    (declare (ignore disposition))
    (unless (outbox-entry-published-p entry)
      (publish-outbox-entry publish-fn entry)
      (setf state
            (mark-outbox-published
             load-fn
             save-fn
             (jsown:val state "_id")
             (outbox-entry-mutation-id entry))))
    state))

(defun pending-outbox-tuples (documents)
  (sort
   (loop for document in documents
         append
         (loop for entry in (document-outbox-entries document)
               unless (outbox-entry-published-p entry)
                 collect (list (jsown:val document "_id") entry)))
   (lambda (left right)
     (let ((left-id (first left))
           (right-id (first right)))
       (if (string= left-id right-id)
           (< (outbox-entry-sequence (second left))
              (outbox-entry-sequence (second right)))
           (string< left-id right-id))))))

(defun recover-outbox-documents (load-fn save-fn publish-fn documents)
  "Publish all pending entries in stable per-document sequence order."
  (dolist (tuple (pending-outbox-tuples documents))
    (destructuring-bind (document-id entry) tuple
      (publish-outbox-entry publish-fn entry)
      (mark-outbox-published
       load-fn
       save-fn
       document-id
       (outbox-entry-mutation-id entry))))
  t)

(defun couchdb-load-outbox-document (client database document-id)
  (handler-case
      (jsown:with-injective-reader
        (jsown:parse
         (cl-couch:get-document client database document-id)))
    (dexador:http-request-not-found ()
      nil)))

(defun couchdb-save-outbox-document (client database document)
  (handler-case
      (let* ((response
               (jsown:parse
                (cl-couch:create-document
                 client
                 database
                 (jsown:to-json document))))
             (saved (clone-outbox-json document)))
        (when (outbox-object-has-key-p response "rev")
          (setf (jsown:val saved "_rev")
                (jsown:val response "rev")))
        saved)
    (dexador:http-request-conflict ()
      (error 'outbox-store-conflict))))

(defun couchdb-process-outbox-mutation
    (client database publish-fn document operation)
  (process-outbox-mutation
   (lambda (document-id)
     (couchdb-load-outbox-document client database document-id))
   (lambda (state)
     (couchdb-save-outbox-document client database state))
   publish-fn
   document
   operation))

(defun couchdb-pending-outbox-documents (client database)
  (let* ((result
           (query-view
            client
            database
            "outbox"
            "pending"
            :include-docs t
            :reduce nil
            :limit 10000))
         (rows (jsown:val result "rows"))
         (seen (make-hash-table :test #'equal)))
    (loop for row in rows
          for document = (jsown:val row "doc")
          for document-id = (jsown:val document "_id")
          unless (gethash document-id seen)
            collect
            (progn
              (setf (gethash document-id seen) t)
              document))))

(defun recover-couchdb-outbox (client database publish-fn)
  (recover-outbox-documents
   (lambda (document-id)
     (couchdb-load-outbox-document client database document-id))
   (lambda (state)
     (couchdb-save-outbox-document client database state))
   publish-fn
   (couchdb-pending-outbox-documents client database)))
