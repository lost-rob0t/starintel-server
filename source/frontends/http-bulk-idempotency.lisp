(in-package :star.frontends.http-api)

(defparameter +bulk-idempotency-header+ "Idempotency-Key")
(defparameter +bulk-idempotency-key-max-length+ 255)
(defparameter *bulk-idempotency-database*
  (or (uiop:getenv "STAR_BULK_IDEMPOTENCY_DATABASE")
      "starintel-gserver-idempotency"))
(defparameter *bulk-idempotency-ttl-seconds*
  (star::environment-integer "STAR_BULK_IDEMPOTENCY_TTL_SECONDS" 86400))

(defvar *bulk-idempotency-db-ready-p* nil)
(defvar *bulk-idempotency-db-lock*
  (bt:make-lock "bulk-idempotency-db"))
(defvar *bulk-idempotency-now-fn* #'get-universal-time)

(define-condition bulk-idempotency-store-conflict (error) ())

(defun bulk-idempotency-now ()
  (funcall *bulk-idempotency-now-fn*))

(defun sha256-hex (string)
  (let ((digest
          (ironclad:digest-sequence
           :sha256
           (babel:string-to-octets string :encoding :utf-8))))
    (with-output-to-string (stream)
      (map nil
           (lambda (octet)
             (format stream "~2,'0x" octet))
           digest))))

(defun clone-json-value (value)
  (jsown:with-injective-reader
    (jsown:parse (jsown:to-json value))))

(defun canonicalize-json-value (value)
  "Return VALUE with every JSON object key ordered lexically.

Array order is intentionally preserved: a bulk request is an ordered sequence,
while object member order is not part of JSON semantics."
  (cond
    ((json-object-p value)
     (let ((keys nil)
           (copy (jsown:empty-object)))
       (jsown:do-json-keys (key ignored) value
         (declare (ignore ignored))
         (push key keys))
       (dolist (key (sort keys #'string<))
         (setf (jsown:val copy key)
               (canonicalize-json-value (jsown:val value key))))
       copy))
    ((vectorp value)
     (map 'vector #'canonicalize-json-value value))
    ((listp value)
     (mapcar #'canonicalize-json-value value))
    (t value)))

(defun bulk-request-fingerprint (documents)
  (sha256-hex
   (jsown:to-json
    (mapcar #'canonicalize-json-value documents))))

(defun bulk-document-tenant-id (document)
  (or (star.documents:document-value document "tenant_id" nil)
      (star.documents:document-value document "tenant" nil)
      "default"))

(defun bulk-request-tenants (documents)
  (sort
   (remove-duplicates
    (mapcar #'bulk-document-tenant-id documents)
    :test #'string=)
   #'string<))

(defun bulk-idempotency-scope-string (principal-id tenants)
  (format nil "~a~%~{~a~^,~}" principal-id tenants))

(defun valid-idempotency-key-p (key)
  (and (stringp key)
       (plusp (length key))
       (<= (length key) +bulk-idempotency-key-max-length+)
       (every (lambda (character)
                (let ((code (char-code character)))
                  (<= 33 code 126)))
              key)))

(defun request-bulk-idempotency-key
    (&optional (request (ningle:context :request)))
  (let* ((headers (ignore-errors (lack.request:request-headers request)))
         (key (request-header-value headers +bulk-idempotency-header+)))
    (when key
      (unless (valid-idempotency-key-p key)
        (signal-http-input-error
         400
         "invalid_idempotency_key"
         "Idempotency-Key must contain 1-255 visible ASCII characters"))
      key)))

(defun idempotency-job-id (principal-id tenants key)
  (format nil "idem-~a"
          (sha256-hex
           (format nil "~a~%~a"
                   (bulk-idempotency-scope-string principal-id tenants)
                   key))))

(defun idempotency-job-id-p (job-id)
  (and (stringp job-id)
       (= (length job-id) 69)
       (string= "idem-" job-id :end2 5)
       (every (lambda (character)
                (or (digit-char-p character)
                    (find (char-downcase character) "abcdef")))
              (subseq job-id 5))))

(defun bulk-idempotency-record-id-from-job-id (job-id)
  (format nil "bulk-idempotency:~a" job-id))

(defun ensure-bulk-idempotency-database ()
  "Create the dedicated idempotency database lazily and exactly once per process."
  (unless *bulk-idempotency-db-ready-p*
    (bt:with-lock-held (*bulk-idempotency-db-lock*)
      (unless *bulk-idempotency-db-ready-p*
        (anypool:with-connection (client *couchdb-pool*)
          (handler-case
              (cl-couch:get-database client *bulk-idempotency-database*)
            (dexador:http-request-not-found ()
              ;; Another server may win the create race. Verify existence after
              ;; the best-effort create instead of treating that race as fatal.
              (ignore-errors
                (cl-couch:create-database
                 client *bulk-idempotency-database*))
              (cl-couch:get-database client *bulk-idempotency-database*)))
          (setf *bulk-idempotency-db-ready-p* t)))))
  t)

(defun couch-load-bulk-idempotency-record (record-id)
  (ensure-bulk-idempotency-database)
  (anypool:with-connection (client *couchdb-pool*)
    (handler-case
        (jsown:with-injective-reader
          (jsown:parse
           (cl-couch:get-document
            client *bulk-idempotency-database* record-id)))
      (dexador:http-request-not-found () nil))))

(defun couch-save-bulk-idempotency-record (record)
  (ensure-bulk-idempotency-database)
  (anypool:with-connection (client *couchdb-pool*)
    (handler-case
        (let* ((response
                 (jsown:parse
                  (cl-couch:create-document
                   client
                   *bulk-idempotency-database*
                   (jsown:to-json record))))
               (saved (clone-json-value record)))
          (when (jsown:keyp response "rev")
            (setf (jsown:val saved "_rev")
                  (jsown:val response "rev")))
          saved)
      (dexador:http-request-conflict ()
        (error 'bulk-idempotency-store-conflict)))))

(defvar *bulk-idempotency-load-fn*
  #'couch-load-bulk-idempotency-record)
(defvar *bulk-idempotency-save-fn*
  #'couch-save-bulk-idempotency-record)

(defun load-bulk-idempotency-record (record-id)
  (funcall *bulk-idempotency-load-fn* record-id))

(defun save-bulk-idempotency-record (record)
  (funcall *bulk-idempotency-save-fn* record))

(defun bulk-idempotency-record-expired-p (record now)
  (let ((expires-at (jsown:val-safe record "expires_at")))
    (and (integerp expires-at)
         (<= expires-at now))))

(defun make-bulk-idempotency-record
    (record-id job-id principal-id tenants key fingerprint mode total now)
  (jsown:new-js
    ("_id" record-id)
    ("kind" "bulk-idempotency")
    ("job_id" job-id)
    ("principal_id" principal-id)
    ("tenants" (coerce tenants 'vector))
    ("key_hash" (sha256-hex key))
    ("fingerprint" fingerprint)
    ("mode" (string-downcase (symbol-name mode)))
    ("status" "reserved")
    ("total" total)
    ("succeeded" 0)
    ("failed" 0)
    ("correlation_id" (current-correlation-id))
    ("created_at" now)
    ("expires_at" (+ now *bulk-idempotency-ttl-seconds*))))

(defun reserve-bulk-idempotency
    (documents key principal-id mode &key (max-attempts 8))
  "Reserve KEY durably before any publish side effect.

Returns RECORD and :OWNER when the caller won the reservation, or RECORD and
:REPLAY when an unexpired equivalent request already owns it."
  (let* ((tenants (bulk-request-tenants documents))
         (fingerprint (bulk-request-fingerprint documents))
         (job-id (idempotency-job-id principal-id tenants key))
         (record-id (bulk-idempotency-record-id-from-job-id job-id)))
    (handler-case
        (loop for attempt from 1 to max-attempts
              for now = (bulk-idempotency-now)
              for existing = (load-bulk-idempotency-record record-id)
              do
                 (cond
                   ((or (null existing)
                        (bulk-idempotency-record-expired-p existing now))
                    (let ((candidate
                            (make-bulk-idempotency-record
                             record-id job-id principal-id tenants key fingerprint
                             mode (length documents) now)))
                      (when (and existing (jsown:keyp existing "_rev"))
                        (setf (jsown:val candidate "_rev")
                              (jsown:val existing "_rev")))
                      (handler-case
                          (return
                            (values
                             (save-bulk-idempotency-record candidate)
                             :owner))
                        (bulk-idempotency-store-conflict ()
                          (when (= attempt max-attempts)
                            (error
                             "Bulk idempotency reservation conflict budget exhausted")))))
                   ((string=
                     fingerprint
                     (or (jsown:val-safe existing "fingerprint") ""))
                    (return (values existing :replay)))
                   (t
                    (signal-http-input-error
                     409
                     "idempotency_key_reused"
                     "Idempotency-Key was already used for a different bulk request"))))
      (http-input-error (condition)
        (error condition))
      (error (condition)
        (log:error "Bulk idempotency reservation failed correlation=~a: ~a"
                   (current-correlation-id) condition)
        (signal-http-input-error
         503
         "bulk_idempotency_unavailable"
         "Bulk idempotency state is temporarily unavailable")))))

(defun update-bulk-idempotency-record
    (record-id updater &key (max-attempts 8))
  (loop for attempt from 1 to max-attempts
        for current = (load-bulk-idempotency-record record-id)
        do
           (unless current
             (error "Bulk idempotency record ~a disappeared" record-id))
           (let ((updated (funcall updater (clone-json-value current))))
             (handler-case
                 (return (save-bulk-idempotency-record updated))
               (bulk-idempotency-store-conflict ()
                 (when (= attempt max-attempts)
                   (error
                    "Bulk idempotency update conflict budget exhausted for ~a"
                    record-id)))))))

(defun bulk-idempotency-terminal-status-p (status)
  (member status
          '("completed" "completed-with-errors" "failed" "indeterminate")
          :test #'string=))

(defun mark-bulk-idempotency-status
    (job-id status &key succeeded failed error-code)
  (let ((record-id (bulk-idempotency-record-id-from-job-id job-id)))
    (update-bulk-idempotency-record
     record-id
     (lambda (record)
       (let ((current (or (jsown:val-safe record "status") "reserved")))
         (unless (and (bulk-idempotency-terminal-status-p current)
                      (not (string= current status)))
           (setf (jsown:val record "status") status)
           (when succeeded
             (setf (jsown:val record "succeeded") succeeded))
           (when failed
             (setf (jsown:val record "failed") failed))
           (when error-code
             (setf (jsown:val record "error_code") error-code))))
       record))))

(defun store-bulk-idempotency-initial-response
    (job-id status-code body)
  (let ((record-id (bulk-idempotency-record-id-from-job-id job-id)))
    (update-bulk-idempotency-record
     record-id
     (lambda (record)
       (unless (jsown:keyp record "initial_response_body")
         (setf (jsown:val record "initial_response_status") status-code
               (jsown:val record "initial_response_body") body))
       record))))

(defun store-bulk-idempotency-inline-result (job-id body)
  (let* ((result (jsown:parse body))
         (succeeded (or (jsown:val-safe result "succeeded") 0))
         (failed (or (jsown:val-safe result "failed") 0))
         (status (if (zerop failed)
                     "completed"
                     "completed-with-errors")))
    (mark-bulk-idempotency-status
     job-id status :succeeded succeeded :failed failed)
    (store-bulk-idempotency-initial-response job-id 200 body)
    body))

(defun add-idempotency-replayed-header ()
  (setf (lack.response:response-headers *response*)
        (append (lack.response:response-headers *response*)
                (list :idempotency-replayed "true"))))

(defun bulk-idempotency-status-url (job-id)
  (format nil "/documents/bulk/~a" job-id))

(defun bulk-idempotency-status-json (record)
  (let ((json
          (jsown:new-js
            ("job_id" (jsown:val record "job_id"))
            ("status" (or (jsown:val-safe record "status") "reserved"))
            ("total" (or (jsown:val-safe record "total") 0))
            ("succeeded" (or (jsown:val-safe record "succeeded") 0))
            ("failed" (or (jsown:val-safe record "failed") 0))
            ("correlation_id" (jsown:val-safe record "correlation_id")))))
    (when (jsown:keyp record "error_code")
      (setf (jsown:val json "error_code")
            (jsown:val record "error_code")))
    json))

(defun replay-bulk-idempotency-record (record)
  "Replay the first response, or a safe 202 while the original request is unresolved."
  (add-idempotency-replayed-header)
  (let ((body (jsown:val-safe record "initial_response_body"))
        (status (jsown:val-safe record "initial_response_status")))
    (if (and (stringp body) (integerp status))
        (progn
          (setf (lack.response:response-status *response*) status)
          body)
        (progn
          (setf (lack.response:response-status *response*) 202)
          (jsown:to-json
           (jsown:new-js
             ("status" (or (jsown:val-safe record "status") "reserved"))
             ("job_id" (jsown:val record "job_id"))
             ("total" (or (jsown:val-safe record "total") 0))
             ("status_url"
              (bulk-idempotency-status-url
               (jsown:val record "job_id")))
             ("correlation_id" (jsown:val-safe record "correlation_id"))
             ("idempotent_replay" :true)))))))

(defun load-bulk-idempotency-by-job-id (job-id)
  (when (idempotency-job-id-p job-id)
    (load-bulk-idempotency-record
     (bulk-idempotency-record-id-from-job-id job-id))))

;; The authenticated CORS middleware reads this setting dynamically.
(unless (search "Idempotency-Key"
                star:*http-cors-allowed-headers*
                :test #'char-equal)
  (setf star:*http-cors-allowed-headers*
        (format nil "~a, Idempotency-Key"
                star:*http-cors-allowed-headers*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(bulk-idempotency-store-conflict
            request-bulk-idempotency-key
            reserve-bulk-idempotency
            replay-bulk-idempotency-record
            idempotency-job-id-p
            mark-bulk-idempotency-status
            store-bulk-idempotency-initial-response
            store-bulk-idempotency-inline-result
            load-bulk-idempotency-by-job-id
            bulk-idempotency-status-json)
          :star.frontends.http-api))
