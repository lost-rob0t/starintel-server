(in-package :star.actors)

(defun target-delivery-context (consumer)
  (let ((stream
          (and consumer
               (star.consumers:consumer-stream consumer))))
    (if (typep stream 'star.consumers:retrying-rabbit-queue-stream)
        (values
         (star.consumers:delivery-attempt
          (star.consumers:retry-stream-current-properties stream))
         (star.consumers:delivery-trace-id
          (star.consumers:retry-stream-current-properties stream)))
        (values 0 nil))))

(defun target-dispatch-fingerprint-json (value)
  (handler-case
      (jsown:to-json value)
    (error ()
      (princ-to-string value))))

(defun legacy-target-dispatch-fingerprint (envelope)
  "Fingerprint all target semantics that may change dispatch behavior.

The schedule identity alone is an idempotency key, not proof that a retried
request is equivalent. Changed target content under one schedule identity must
conflict instead of being silently treated as a duplicate."
  (let ((record (target-dispatch-envelope-record envelope))
        (destination (target-dispatch-envelope-destination envelope)))
    (target-dispatch-digest
     (format nil "~a|~a|~a|~a|~a|~a|~a|~a|~a|~a|~a"
             (target-dispatch-envelope-schedule-id envelope)
             (target-record-id record)
             (or (target-record-revision record) "unrevisioned")
             (target-destination-handle-kind destination)
             (target-destination-handle-name destination)
             (target-record-actor record)
             (target-record-target record)
             (target-record-delay record)
             (if (target-record-recurring-p record) "true" "false")
             (target-dispatch-fingerprint-json
              (target-record-options record))
             (or (target-record-deadline record) "no-deadline")))))

(defun canonical-target-json (value)
  "Deterministic, injective JSON for canonical target semantics.

Object order is insignificant; key spelling, array order, JSON literals and
empty collections are retained. Unsupported values fail closed."
  (labels ((ordered (item ancestors)
             (cond
               ((or (stringp item) (integerp item) (floatp item) (starintel:json-number-p item)
                    (member item '(t :true :false :null) :test #'eq))
                item)
               ((or (listp item) (vectorp item))
                (when (member item ancestors :test #'eq)
                  (error 'invalid-target-dispatch
                         :reason "canonical target JSON contains a cycle"))
                (when (listp item)
                  (unless (integerp (list-length item))
                    (error 'invalid-target-dispatch
                           :reason "canonical target JSON contains a circular list")))
                (let ((parents (cons item ancestors)))
                  (if (and (consp item) (eq (car item) :obj))
                      (let ((seen nil) (pairs nil))
                        (dolist (pair (cdr item))
                          (unless (and (consp pair) (stringp (car pair)))
                            (error 'invalid-target-dispatch
                                   :reason "canonical target JSON object key is invalid"))
                          (when (member (car pair) seen :test #'string=)
                            (error 'invalid-target-dispatch
                                   :reason "canonical target JSON has duplicate object keys"))
                          (push (car pair) seen)
                          (push (cons (car pair) (ordered (cdr pair) parents)) pairs))
                        (cons :obj (sort pairs #'string< :key #'car)))
                      (map 'vector (lambda (entry) (ordered entry parents)) item))))
               (t
                (error 'invalid-target-dispatch
                       :reason "canonical target contains a non-JSON value")))))
    (handler-case
        (let ((json (jsown:to-json (ordered value nil))))
          ;; Reject non-finite or otherwise non-JSON number encodings, too.
          (starintel:parse-json json)
          json)
      (invalid-target-dispatch (condition) (error condition))
      (error ()
        (error 'invalid-target-dispatch
               :reason "canonical target cannot be serialized as JSON")))))

(defun target-fingerprint-canonical-document-p (document)
  "Canonical intent must not fall back to legacy after a field is removed."
  (or (star.documents:object-has-key-p document "id")
      (star.documents:object-has-key-p document "schemaVersion")))

(defun require-canonical-target-fingerprint (predicate reason)
  (unless predicate
    (error 'invalid-target-dispatch :reason reason)))

(defun canonical-target-fingerprint-record (document)
  "Read strict canonical Target semantics without mutating stored state."
  (require-canonical-target-fingerprint
   (and (target-fingerprint-canonical-document-p document)
        (member (star.documents:object-value document "dtype")
                '("target" "investigation-target") :test #'equal))
   "acceptance requires a canonical Target document")
  (canonical-target-json document)
  (let ((id (star.documents:object-value document "id")))
    (when (star.documents:object-has-key-p document "_id")
      (require-canonical-target-fingerprint
       (equal id (star.documents:object-value document "_id"))
       "canonical target id disagrees with storage identity")))
  (dolist (key '("rev" "_rev"))
    (when (star.documents:object-has-key-p document key)
      (require-canonical-target-fingerprint
       (target-nonempty-string-p (star.documents:object-value document key))
       "canonical target revision must be a non-empty string")))
  (when (and (star.documents:object-has-key-p document "rev")
             (star.documents:object-has-key-p document "_rev"))
    (require-canonical-target-fingerprint
     (equal (star.documents:object-value document "rev")
            (star.documents:object-value document "_rev"))
     "canonical target rev disagrees with storage revision"))
  (star.documents:validate-stored-document document)
  (parse-target-record document))

(defun canonical-target-record-values (record)
  (vector (target-record-id record)
          (or (target-record-revision record) :null)
          (target-record-actor record) (target-record-target record)
          (target-record-delay record)
          (if (target-record-recurring-p record) :true :false)
          (target-record-options record)))

(defun canonical-target-semantic-projection
    (record schedule-id destination-kind destination-name routing-key)
  "Shared pure semantics for a new envelope and a stored acceptance.

Do not check wall-clock expiry here: equality must not depend on retry time."
  (let* ((document (target-record-document record))
         (parsed (canonical-target-fingerprint-record document))
         (tenant-present (star.documents:object-has-key-p document "tenant_id"))
         (tenant (star.documents:object-value document "tenant_id")))
    (require-canonical-target-fingerprint
     (string= (canonical-target-json (canonical-target-record-values record))
              (canonical-target-json (canonical-target-record-values parsed)))
     "typed target record disagrees with its canonical document")
    (require-canonical-target-fingerprint
     (and (valid-target-actor-name-p (target-record-actor parsed))
          (target-nonempty-string-p (target-record-target parsed))
          (integerp (target-record-delay parsed))
          (<= 1 (target-record-delay parsed) *target-max-delay-seconds*))
     "canonical target dispatch fields are invalid")
    (require-canonical-target-fingerprint
     (and (target-nonempty-string-p schedule-id)
          (equal schedule-id (target-record-schedule-id parsed)))
     "canonical target schedule identity disagrees with its document")
    ;; The existing acceptance protocol stores actor as destination name.
    (require-canonical-target-fingerprint
     (equal destination-name (target-record-actor parsed))
     "canonical target destination name disagrees with actor")
    (require-canonical-target-fingerprint
     (case destination-kind
       (:rabbit (target-nonempty-string-p routing-key))
       (:local (eq routing-key :null))
       (otherwise nil))
     "canonical target destination or routing key is invalid")
    (require-canonical-target-fingerprint
     (or (not tenant-present) (target-nonempty-string-p tenant))
     "canonical target tenant scope is invalid")
    (jsown:new-js
      ("schemaVersion" (star.documents:object-value document "schemaVersion"))
      ("dtype" (star.documents:object-value document "dtype"))
      ("id" (target-record-id parsed))
      ("revision" (or (target-record-revision parsed) :null))
      ("scheduleId" schedule-id)
      ("dataset" (star.documents:document-dataset document))
      ("actor" (target-record-actor parsed)) ("target" (target-record-target parsed))
      ("delay" (target-record-delay parsed))
      ("recurring" (if (target-record-recurring-p parsed) :true :false))
      ("options" (target-record-options parsed))
      ("deadline" (or (target-record-deadline parsed) :null))
      ("destination"
       (jsown:new-js
         ("kind" (ecase destination-kind (:rabbit "rabbit") (:local "local")))
         ("name" destination-name) ("routingKey" routing-key)))
      ("tenantScope"
       (jsown:new-js ("present" (if tenant-present :true :false))
                     ("value" (if tenant-present tenant :null)))))))

(defun target-dispatch-fingerprint (envelope)
  (let* ((record (target-dispatch-envelope-record envelope))
         (destination (target-dispatch-envelope-destination envelope)))
    (if (target-fingerprint-canonical-document-p (target-record-document record))
        (progn
          (require-canonical-target-fingerprint
           (equal (target-dispatch-envelope-deadline envelope)
                  (target-record-deadline record))
           "canonical target envelope deadline disagrees with its document")
          (target-dispatch-digest
           (canonical-target-json
            (canonical-target-semantic-projection
             record (target-dispatch-envelope-schedule-id envelope)
             (target-destination-handle-kind destination)
             (target-destination-handle-name destination)
             (or (target-destination-handle-routing-key destination) :null)))))
        (legacy-target-dispatch-fingerprint envelope))))

(defun canonical-target-acceptance-projection (acceptance)
  "Validate stored metadata and reconstruct semantics, never trusting its digest."
  (canonical-target-json acceptance)
  (dolist (key '("_id" "type" "status" "fingerprint" "target_document"
                 "target_id" "target_revision" "actor" "schedule_id"
                 "execution_id" "attempt" "trace_id" "lease_id" "fencing_token"
                 "destination_kind" "routing_key" "recurring" "delay" "deadline"))
    (require-canonical-target-fingerprint
     (star.documents:object-has-key-p acceptance key)
     "canonical acceptance is missing required metadata"))
  (let* ((document (jsown:val acceptance "target_document"))
         (record (canonical-target-fingerprint-record document))
         (schedule-id (jsown:val acceptance "schedule_id"))
         (kind (jsown:val acceptance "destination_kind"))
         (projection
           (canonical-target-semantic-projection
            record schedule-id
            (cond ((equal kind "rabbit") :rabbit) ((equal kind "local") :local))
            (jsown:val acceptance "actor") (jsown:val acceptance "routing_key"))))
    (require-canonical-target-fingerprint
     (and (equal "_server_target_acceptance" (jsown:val acceptance "type"))
          (equal (target-acceptance-id schedule-id) (jsown:val acceptance "_id"))
          (equal (target-record-id record) (jsown:val acceptance "target_id"))
          (equal (or (target-record-revision record) :null)
                 (jsown:val acceptance "target_revision"))
          (equal (target-record-delay record) (jsown:val acceptance "delay"))
          (eq (if (target-record-recurring-p record) :true :false)
              (jsown:val acceptance "recurring"))
          (equal (or (target-record-deadline record) :null)
                 (jsown:val acceptance "deadline")))
     "canonical acceptance metadata disagrees with its target document")
    (dolist (key '("fingerprint" "execution_id" "trace_id" "lease_id"))
      (require-canonical-target-fingerprint
       (target-nonempty-string-p (jsown:val acceptance key))
       "canonical acceptance has invalid durable metadata"))
    (require-canonical-target-fingerprint
     (and (member (jsown:val acceptance "status")
                  '("pending" "accepted" "scheduled" "dispatched") :test #'equal)
          (integerp (jsown:val acceptance "attempt"))
          (not (minusp (jsown:val acceptance "attempt")))
          (integerp (jsown:val acceptance "fencing_token"))
          (plusp (jsown:val acceptance "fencing_token")))
     "canonical acceptance has invalid durable state")
    (let ((extensions (star.documents:object-value document "extensions")))
      (dolist (mapping '(("target_schedule_id" . "schedule_id")
                         ("target_execution_id" . "execution_id")
                         ("target_attempt" . "attempt") ("target_trace_id" . "trace_id")
                         ("target_lease_id" . "lease_id")
                         ("target_fencing_token" . "fencing_token")))
        (when (star.documents:object-has-key-p extensions (car mapping))
          (require-canonical-target-fingerprint
           (equal (star.documents:object-value extensions (car mapping))
                  (jsown:val acceptance (cdr mapping)))
           "canonical target transport metadata disagrees with its acceptance"))))
    projection))

(defun target-acceptance-equivalent-p (left right)
  "Compare canonical semantics, retaining historical legacy equality verbatim."
  (if (or (target-fingerprint-canonical-document-p
           (star.documents:object-value left "target_document"))
          (target-fingerprint-canonical-document-p
           (star.documents:object-value right "target_document")))
      (handler-case
          (string= (canonical-target-json (canonical-target-acceptance-projection left))
                   (canonical-target-json (canonical-target-acceptance-projection right)))
        (error () nil))
      (and (string= (jsown:val left "schedule_id") (jsown:val right "schedule_id"))
           (string= (jsown:val left "fingerprint") (jsown:val right "fingerprint")))))
