(in-package :star.databases.couchdb)

(defstruct (document-update-outcome
             (:constructor make-document-update-outcome
                 (status &key document attempts reason)))
  status
  document
  (attempts 0 :type (integer 0 *))
  reason)

(define-condition document-update-validation-error (error)
  ((reason
    :initarg :reason
    :reader document-update-validation-reason))
  (:report
   (lambda (condition stream)
     (format stream "Document update validation failed: ~a"
             (document-update-validation-reason condition)))))

(define-condition document-update-store-conflict (error) ())

(defparameter +document-update-persistence-keys+
  '("_id" "_rev")
  "Envelope fields controlled only by CouchDB persistence.")

(defparameter +document-update-invariant-keys+
  '("_id" "dtype" "schema_version" "dataset" "date_added")
  "Envelope fields that a patch cannot change after document creation.")

(defun document-update-json-object-p (value)
  (and (consp value) (eq (first value) :obj)))

(defun clone-document-update-json (value)
  (jsown:with-injective-reader
    (jsown:parse (jsown:to-json value))))

(defun document-update-value (object key &optional default)
  (if (and object (outbox-object-has-key-p object key))
      (jsown:val object key)
      default))

(defun document-update-string= (left right)
  (and (stringp left) (stringp right) (string= left right)))

(defun server-private-extension-key-p (key)
  (and (stringp key)
       (>= (length key) 8)
       (string= "_server_" key :end2 8)))

(defun assert-compatible-update-field (existing patch key)
  (when (outbox-object-has-key-p patch key)
    (let ((incoming (jsown:val patch key))
          (current (document-update-value existing key)))
      (unless (equal incoming current)
        (error 'document-update-validation-error
               :reason
               (format nil "patch cannot change ~a from ~s to ~s"
                       key current incoming))))))

(defun validate-document-update-compatibility (document-id existing patch)
  (unless (document-update-json-object-p patch)
    (error 'document-update-validation-error
           :reason "patch must be a JSON object"))
  (let ((patch-id (document-update-value patch "_id")))
    (when (and patch-id (not (document-update-string= patch-id document-id)))
      (error 'document-update-validation-error
             :reason
             (format nil "patch _id ~s does not match route id ~s"
                     patch-id document-id))))
  (when existing
    (dolist (key +document-update-invariant-keys+)
      (assert-compatible-update-field existing patch key)))
  t)

(defun merge-document-update-value (current incoming path)
  (if (and (document-update-json-object-p current)
           (document-update-json-object-p incoming))
      (let ((result (clone-document-update-json current)))
        (jsown:do-json-keys (key value) incoming
          (unless (or (and (null path)
                           (member key +document-update-persistence-keys+
                                   :test #'string=))
                      (and (null path)
                           (member key +document-update-invariant-keys+
                                   :test #'string=))
                      (and (equal path '("extensions"))
                           (server-private-extension-key-p key)))
            (setf (jsown:val result key)
                  (if (outbox-object-has-key-p result key)
                      (merge-document-update-value
                       (jsown:val result key)
                       value
                       (append path (list key)))
                      (clone-document-update-json value)))))
        result)
      (clone-document-update-json incoming)))

(defun merge-document-update (document-id existing patch)
  "Return a new merged document; EXISTING and PATCH remain unchanged.

Caller `_rev` is never copied. The current `_rev` and server-private extension
fields remain controlled by persistence."
  (validate-document-update-compatibility document-id existing patch)
  (let ((merged (merge-document-update-value existing patch nil)))
    (setf (jsown:val merged "_id") document-id)
    (when (outbox-object-has-key-p existing "_rev")
      (setf (jsown:val merged "_rev") (jsown:val existing "_rev")))
    merged))

(defun prepare-document-insert (document-id patch)
  "Return a new insert candidate with caller `_rev` removed."
  (validate-document-update-compatibility document-id nil patch)
  (let ((candidate
          (copy-json-object-excluding
           (clone-document-update-json patch)
           '("_rev"))))
    (setf (jsown:val candidate "_id") document-id)
    candidate))

(defun validated-document-update (candidate)
  (handler-case
      (star.documents:ensure-v09-document candidate)
    (error (condition)
      (error 'document-update-validation-error
             :reason (princ-to-string condition)))))

(defun document-update-wire-equal-p (left right)
  (string= (jsown:to-json left) (jsown:to-json right)))

(defun upsert-document-update
    (load-fn save-fn document-id patch &key (max-attempts 8))
  "Create or update DOCUMENT-ID with immutable, bounded optimistic retries.

LOAD-FN receives DOCUMENT-ID and returns the latest document or NIL. SAVE-FN
receives the validated candidate and must signal DOCUMENT-UPDATE-STORE-CONFLICT
when its CouchDB revision loses a compare-and-swap race."
  (unless (and (integerp max-attempts) (plusp max-attempts))
    (error "MAX-ATTEMPTS must be a positive integer"))
  (let ((immutable-patch (clone-document-update-json patch)))
    (loop for attempt from 1 to max-attempts
          do
             (let ((existing (funcall load-fn document-id)))
               (handler-case
                   (let* ((candidate
                            (if existing
                                (merge-document-update
                                 document-id existing immutable-patch)
                                (prepare-document-insert
                                 document-id immutable-patch)))
                          (validated (validated-document-update candidate)))
                     (when (and existing
                                (document-update-wire-equal-p
                                 existing validated))
                       (return
                         (make-document-update-outcome
                          :duplicate
                          :document existing
                          :attempts attempt)))
                     (handler-case
                         (let ((saved (funcall save-fn validated)))
                           (return
                             (make-document-update-outcome
                              (if existing :updated :created)
                              :document saved
                              :attempts attempt)))
                       (document-update-store-conflict ()
                         (when (= attempt max-attempts)
                           (return
                             (make-document-update-outcome
                              :conflict-exhausted
                              :document existing
                              :attempts attempt
                              :reason
                              "optimistic concurrency retry budget exhausted")))))
                 (document-update-validation-error (condition)
                   (return
                     (make-document-update-outcome
                      :validation-failed
                      :document existing
                      :attempts attempt
                      :reason
                      (document-update-validation-reason condition)))))))))

(defun couchdb-upsert-document-update
    (client database document-id patch &key (max-attempts 8))
  "CouchDB adapter for UPSERT-DOCUMENT-UPDATE."
  (upsert-document-update
   (lambda (id)
     (couchdb-load-outbox-document client database id))
   (lambda (candidate)
     (handler-case
         (couchdb-save-outbox-document client database candidate)
       (outbox-store-conflict ()
         (error 'document-update-store-conflict))))
   document-id
   patch
   :max-attempts max-attempts))

(defun document-update-outcome-json (outcome)
  (let ((object (jsown:empty-object)))
    (setf (jsown:val object "status")
          (string-downcase
           (symbol-name (document-update-outcome-status outcome)))
          (jsown:val object "attempts")
          (document-update-outcome-attempts outcome)
          (jsown:val object "reason")
          (or (document-update-outcome-reason outcome) :null)
          (jsown:val object "document")
          (or (document-update-outcome-document outcome) :null))
    object))
