(in-package :star.frontends.http-api)

(defparameter +target-v1-path+ "/api/v1/targets")
(defparameter +target-v1-max-idempotency-key-bytes+ 256)

(defun target-v1-digest (text)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (babel:string-to-octets text :encoding :utf-8))))

(defun target-v1-required-string (request field code)
  (let ((value (jsown:val-safe request field)))
    (unless (non-empty-string-p value)
      (signal-http-input-error
       422 code (format nil "~a must be a non-empty string" field)))
    value))

(defun target-v1-idempotency-key (request)
  (let ((value (jsown:val-safe request "idempotency_key")))
    (unless (non-empty-string-p value)
      (signal-http-input-error
       400 "idempotency_key_required" "idempotency_key is required"))
    (when (> (length (babel:string-to-octets value :encoding :utf-8))
             +target-v1-max-idempotency-key-bytes+)
      (signal-http-input-error
       422 "idempotency_key_too_large"
       "idempotency_key exceeds the configured limit"))
    value))

(defun target-v1-delay (request)
  (let ((value (or (jsown:val-safe request "delay") 1)))
    (unless (and (integerp value)
                 (plusp value)
                 (<= value star.actors::*target-max-delay-seconds*))
      (signal-http-input-error
       422 "invalid_target_delay"
       "delay must be a positive integer within the target scheduling limit"))
    value))

(defun target-v1-recurring-p (request)
  (let ((value (jsown:val-safe request "recurring")))
    (cond
      ((or (null value) (eq value :false)) nil)
      ((or (eq value t) (eq value :true)) t)
      (t
       (signal-http-input-error
        422 "invalid_target_recurring" "recurring must be a boolean")))))

(defun target-v1-options (request)
  (let ((value (jsown:val-safe request "options")))
    (cond
      ((null value) #())
      ((vectorp value) value)
      ((and (listp value) (not (json-object-p value))) value)
      (t
       (signal-http-input-error
        422 "invalid_target_options" "options must be a JSON array")))))

(defun target-v1-options-json (value)
  (jsown:to-json (if (vectorp value) (coerce value 'list) value)))

(defun target-v1-request-identity (principal idempotency-key)
  (target-v1-digest (format nil "~a|~a" principal idempotency-key)))

(defun target-v1-document-from-request (request principal)
  (unless (json-object-p request)
    (signal-http-input-error
     400 "json_object_required" "Request body must be a JSON object"))
  (unless (non-empty-string-p principal)
    (signal-http-input-error
     401 "principal_required" "An authenticated principal is required"))
  (let* ((actor (target-v1-required-string request "actor" "invalid_target_actor"))
         (target (target-v1-required-string request "target" "invalid_target_subject"))
         (dataset (target-v1-required-string request "dataset" "invalid_target_dataset"))
         (idempotency-key (target-v1-idempotency-key request))
         (identity (target-v1-request-identity principal idempotency-key))
         (delay (target-v1-delay request))
         (recurring-p (target-v1-recurring-p request))
         (options (target-v1-options request))
         (extensions (jsown:empty-object))
         (document (jsown:empty-object))
         (now (star.documents:utc-now)))
    (unless (star.actors::valid-target-actor-name-p actor)
      (signal-http-input-error
       422 "invalid_target_actor" "actor contains invalid characters"))
    (setf (jsown:val extensions "idempotency_key") identity
          (jsown:val extensions "submitted_by") (target-v1-digest principal)
          (jsown:val document "_id") (format nil "target:~a" identity)
          (jsown:val document "dataset") dataset
          (jsown:val document "dtype") "target"
          (jsown:val document "schema_version") starintel:+starintel-doc-version+
          (jsown:val document "version") 1
          (jsown:val document "date_added") now
          (jsown:val document "date_updated") now
          (jsown:val document "actor") actor
          (jsown:val document "target") target
          (jsown:val document "delay") delay
          (jsown:val document "recurring") (if recurring-p :true :false)
          (jsown:val document "options") options
          (jsown:val document "schedule_id") (format nil "target-request:~a" identity)
          (jsown:val document "extensions") extensions)
    document))

(defun target-v1-request-fingerprint (request principal)
  (let ((actor (target-v1-required-string request "actor" "invalid_target_actor"))
        (target (target-v1-required-string request "target" "invalid_target_subject"))
        (dataset (target-v1-required-string request "dataset" "invalid_target_dataset"))
        (delay (target-v1-delay request))
        (recurring-p (target-v1-recurring-p request))
        (options (target-v1-options request)))
    (target-v1-digest
     (format nil "~a|~a|~a|~a|~a|~a|~a"
             principal actor target dataset delay
             (if recurring-p "true" "false")
             (target-v1-options-json options)))))

(defun target-v1-request-ledger (request document principal)
  (let* ((idempotency-key (target-v1-idempotency-key request))
         (identity (target-v1-request-identity principal idempotency-key))
         (ledger (jsown:empty-object)))
    (setf (jsown:val ledger "_id") (format nil "target-request:~a" identity)
          (jsown:val ledger "type") "_server_target_request"
          (jsown:val ledger "target_id") (jsown:val document "_id")
          (jsown:val ledger "fingerprint")
          (target-v1-request-fingerprint request principal)
          (jsown:val ledger "created_at") (star.documents:utc-now))
    ledger))

(defun target-v1-request-equivalent-p (left right)
  (and (string= (jsown:val left "_id") (jsown:val right "_id"))
       (string= (jsown:val left "fingerprint")
                (jsown:val right "fingerprint"))))

(defun target-v1-receipt (ledger disposition)
  (let ((receipt (jsown:empty-object)))
    (setf (jsown:val receipt "status")
          (if (member disposition '(:duplicate :resumed) :test #'eq)
              "duplicate"
              "accepted")
          (jsown:val receipt "target_id") (jsown:val ledger "target_id")
          (jsown:val receipt "request_id") (jsown:val ledger "_id")
          (jsown:val receipt "correlation_id") (current-correlation-id))
    receipt))