(in-package :star.actors)

(defstruct (target-record
             (:constructor %make-target-record
                 (id actor target delay recurring-p options document
                  revision lease-owner lease-expires-at)))
  "A scheduled target: what to run, where, and when."
  id
  actor
  target
  delay
  recurring-p
  options
  document
  revision
  lease-owner
  lease-expires-at)

;; Accessor documentation for target-record
(setf (documentation 'TARGET-RECORD-ACTOR 'function)
"The =actor= slot of =target-record=.")
(setf (documentation 'TARGET-RECORD-DELAY 'function)
"The =delay= slot of =target-record=.")
(setf (documentation 'TARGET-RECORD-DOCUMENT 'function)
"The =document= slot of =target-record=.")
(setf (documentation 'TARGET-RECORD-ID 'function)
"The =id= slot of =target-record=.")
(setf (documentation 'TARGET-RECORD-LEASE-EXPIRES-AT 'function)
"Expiration of the lease guarding this target record.")
(setf (documentation 'TARGET-RECORD-LEASE-OWNER 'function)
"The =lease-owner= slot of =target-record=.")
(setf (documentation 'TARGET-RECORD-OPTIONS 'function)
"The =options= slot of =target-record=.")
(setf (documentation 'TARGET-RECORD-RECURRING-P 'function)
"The =recurring-p= slot of =target-record=.")
(setf (documentation 'TARGET-RECORD-REVISION 'function)
"The =revision= slot of =target-record=.")
(setf (documentation 'TARGET-RECORD-TARGET 'function)
"The =target= slot of =target-record=.")



(defstruct (target-command
             (:constructor make-target-command
                 (record &key (first-time-p t) (recovered-p nil))))
  "Command message consumed by target actors."
  record
  (first-time-p t)
  (recovered-p nil))

;; Accessor documentation for target-command
(setf (documentation 'TARGET-COMMAND-FIRST-TIME-P 'function)
"The =first-time-p= slot of =target-command=.")
(setf (documentation 'TARGET-COMMAND-RECORD 'function)
"The =record= slot of =target-command=.")
(setf (documentation 'TARGET-COMMAND-RECOVERED-P 'function)
"The =recovered-p= slot of =target-command=.")



(define-condition invalid-persisted-target (error)
  ((document-id
    :initarg :document-id
    :reader invalid-target-document-id)
   (reason
    :initarg :reason
    :reader invalid-target-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid persisted target ~a: ~a"
             (invalid-target-document-id condition)
             (invalid-target-reason condition))))
  (:documentation "Signalled when a persisted target document cannot be reloaded safely."))

;; Accessor documentation for invalid-persisted-target
(setf (documentation 'INVALID-TARGET-DOCUMENT-ID 'function)
"The =document-id= slot of =invalid-persisted-target=.")
(setf (documentation 'INVALID-TARGET-REASON 'function)
"The =reason= slot of =invalid-persisted-target=.")



(defparameter *recovered-target-fingerprints*
  (make-hash-table :test #'equal)
  "Process-local ledger preventing duplicate timer/execution recovery passes.")

(defun target-value (document key &optional default)
  (star.documents:document-value document key default))

(defun target-boolean-p (value)
  (or (eq value t) (eq value :true)))

(defun target-nonempty-string-p (value)
  (and (stringp value) (plusp (length value))))

(defun target-record-fingerprint (record)
  (format nil "~a|~a"
          (target-record-id record)
          (or (target-record-revision record) "unrevisioned")))

(defun target-active-lease-p (record &optional (now (star.documents:utc-now)))
  "Return true when RECORD has a non-expired persisted execution lease."
  (let ((owner (target-record-lease-owner record))
        (expires (target-record-lease-expires-at record)))
    (and (target-nonempty-string-p owner)
         (target-nonempty-string-p expires)
         (string> expires now))))

(defun parse-target-record (document)
  "Validate one v0.9 target document and return a typed recovery record."
  (let* ((object (star.documents:parse-document-object document))
         (dtype (star.documents:document-dtype object))
         (id (star.documents:object-value object "_id"))
         (actor (target-value object "actor"))
         (target (target-value object "target"))
         (delay (target-value object "delay" 0))
         (recurring (target-boolean-p (target-value object "recurring" nil)))
         (options (target-value object "options" #()))
         (revision (star.documents:object-value object "_rev" nil))
         (lease-owner (target-value object "lease_owner" nil))
         (lease-expires-at (target-value object "lease_expires_at" nil)))
    (unless (member dtype '("target" "investigation-target") :test #'string=)
      (error 'invalid-persisted-target
             :document-id id
             :reason (format nil "unsupported dtype ~a" dtype)))
    (unless (target-nonempty-string-p id)
      (error 'invalid-persisted-target
             :document-id (or id "<missing>")
             :reason "_id must be a non-empty string"))
    (unless (target-nonempty-string-p actor)
      (error 'invalid-persisted-target
             :document-id id
             :reason "actor must be a non-empty string"))
    (unless (target-nonempty-string-p target)
      (error 'invalid-persisted-target
             :document-id id
             :reason "target must be a non-empty string"))
    (unless (and (integerp delay) (not (minusp delay)))
      (error 'invalid-persisted-target
             :document-id id
             :reason "delay must be a non-negative integer"))
    (when (and recurring (zerop delay))
      (error 'invalid-persisted-target
             :document-id id
             :reason "recurring targets require a positive delay"))
    (%make-target-record
     id actor target delay recurring options object revision
     lease-owner lease-expires-at)))

(defun target-recovery-digest (text)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (babel:string-to-octets text :encoding :utf-8))))

(defun invalid-target-quarantine-record (database document condition)
  (let* ((document-id
           (or (ignore-errors (star.documents:object-value document "_id"))
               "unknown"))
         (revision
           (or (ignore-errors (star.documents:object-value document "_rev"))
               "unrevisioned"))
         (digest
           (target-recovery-digest
            (format nil "~a|~a|~a" database document-id revision)))
         (record (jsown:empty-object)))
    (setf (jsown:val record "_id")
          (format nil "quarantine:target-recovery:~a" digest)
          (jsown:val record "type") "_server_quarantine"
          (jsown:val record "status") "quarantined"
          (jsown:val record "failure_class") "invalid-persisted-target"
          (jsown:val record "failure_reason") (princ-to-string condition)
          (jsown:val record "condition_type") "INVALID-PERSISTED-TARGET"
          (jsown:val record "original_exchange") "couchdb"
          (jsown:val record "original_routing_key")
          (format nil "database.~a.target.~a" database document-id)
          (jsown:val record "message_id") document-id
          (jsown:val record "trace_id") digest
          (jsown:val record "attempt_count") 0
          (jsown:val record "first_seen_at") (star.documents:utc-now)
          (jsown:val record "received_at") (star.documents:utc-now)
          (jsown:val record "failed_at") (star.documents:utc-now)
          (jsown:val record "replayed_at") :null
          (jsown:val record "replay_count") 0
          (jsown:val record "original_body")
          (handler-case (jsown:to-json document)
            (error () (princ-to-string document)))
          (jsown:val record "original_properties") (jsown:empty-object))
    record))

(defun quarantine-invalid-persisted-target
    (client database document condition)
  "Persist a deterministic quarantine record; repeated startup is idempotent."
  (let ((record
          (invalid-target-quarantine-record database document condition)))
    (handler-case
        (star.databases.couchdb:couchdb-save-quarantine-record
         client database record)
      (dexador:http-request-conflict () record))))

(defun load-persisted-target-records
    (client database &key actors
                      (query-fn #'star.databases.couchdb:query-view)
                      (quarantine-fn #'quarantine-invalid-persisted-target))
  "Return valid typed records and quarantine invalid persisted documents."
  (let ((records nil)
        (invalid-count 0))
    (dolist (document
             (query-persisted-target-documents
              client database :actors actors :query-fn query-fn))
      (handler-case
          (push (parse-target-record document) records)
        (error (condition)
          (incf invalid-count)
          (funcall quarantine-fn client database document condition))))
    (values (nreverse records) invalid-count)))

(defun recover-target-record (record)
  "Recover one record once per persisted revision in this process."
  (let ((fingerprint (target-record-fingerprint record)))
    (cond
      ((target-active-lease-p record) :leased)
      ((gethash fingerprint *recovered-target-fingerprints*) :duplicate)
      (t
       (setf (gethash fingerprint *recovered-target-fingerprints*) t)
       (handler-case
           (progn
             (submit-target
              record
              :first-time-p (target-record-recurring-p record)
              :recovered-p t)
             :recovered)
         (error (condition)
           (remhash fingerprint *recovered-target-fingerprints*)
           (error condition)))))))

(defun recover-persisted-targets
    (&key (database star:*couchdb-default-database*) actors)
  "Recover valid persisted targets after actor startup and before Rabbit ingest."
  (anypool:with-connection
      (client star.databases.couchdb:*couchdb-pool*)
    (multiple-value-bind (records invalid-count)
        (load-persisted-target-records client database :actors actors)
      (let ((recovered 0)
            (duplicates 0)
            (leased 0))
        (dolist (record records)
          (ecase (recover-target-record record)
            (:recovered (incf recovered))
            (:duplicate (incf duplicates))
            (:leased (incf leased))))
        (list :loaded (length records)
              :recovered recovered
              :duplicates duplicates
              :leased leased
              :invalid invalid-count)))))

(defun start-target-loader ()
  "Compatibility entry point for explicit recovery."
  (recover-persisted-targets))

(defun start-actors
    (&key rabbit-user rabbit-host rabbit-password rabbit-vhost rabbit-port)
  "Start persistence and registry actors, then recover targets before ingest."
  (start-actor-system)
  (setf *producer-agent*
        (make-producer-agent
         (star.producers:make-producer
          :name "actor-producer"
          :exchange-name "documents"
          :host rabbit-host
          :port rabbit-port
          :user rabbit-user
          :password rabbit-password
          :vhost rabbit-vhost)
         *sys*))
  (let ((*gc-timer* (wt:make-wheel-timer)))
    (wt:schedule-recurring
     *gc-timer* 1 3600 (lambda () (sb-ext:gc :full t))))
  (start-couchdb-agent *sys*)
  (start-actor-index *sys*)
  (start-couchdb-gets *sys*)
  (start-couchdb-inserts *sys*)
  (start-target-timer)
  (start-target-actor *sys*)
  ;; Actor hooks complete registry population before persisted work is replayed.
  (nhooks:run-hook star:*actors-start-hook*)
  (recover-persisted-targets))
