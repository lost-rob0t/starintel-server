(in-package :star.documents.v09)

(define-condition v09-document-error (error)
  ((message :initarg :message :reader v09-document-error-message))
  (:report (lambda (condition stream)
             (format stream "~a" (v09-document-error-message condition)))))

(defun object-has-key-p (object key)
  (handler-case
      (progn (jsown:val object key) t)
    (error () nil)))

(defun object-value (object key)
  (when (and object (object-has-key-p object key))
    (jsown:val object key)))

(defun document-data (document)
  (let ((data (object-value document "data")))
    (if data data (jsown:empty-object))))

(defun document-value (document key &optional default)
  "Read KEY from a canonical v0.9 document, with legacy top-level fallback."
  (let* ((data (document-data document))
         (value (or (object-value data key)
                    (object-value document key))))
    (if (null value) default value)))

(defun document-dtype (document)
  (spec:canonical-dtype (or (object-value document "dtype") "document")))

(defun document-date-added (document)
  (or (object-value document "date_added")
      (object-value document "dateAdded")
      0))

(defun document-transient-p (document)
  (let ((value (document-value document "transient" nil)))
    (or (eq value t) (eq value :true))))

(defun required-key-present-p (document key)
  (object-has-key-p document key))

(defun v09-document-p (document)
  (and document
       (every (lambda (key) (required-key-present-p document key))
              '("_id" "dataset" "dtype" "schema_version" "version"
                "date_added" "date_updated" "sources" "evidence" "data"))
       (string= (or (object-value document "schema_version") "") "0.9.0")
       (integerp (object-value document "version"))
       (stringp (object-value document "date_added"))
       (stringp (object-value document "date_updated"))))

(defun parse-document (document)
  (etypecase document
    (string (jsown:parse document))
    (list document)))

(defun ensure-v09-document (document &key route-dtype)
  "Parse and validate the canonical v0.9 envelope at an ingestion boundary."
  (let* ((object (parse-document document))
         (dtype (document-dtype object)))
    (unless (v09-document-p object)
      (error 'v09-document-error
             :message "payload is not a canonical StarIntel v0.9.0 document"))
    (when (and route-dtype
               (not (string= dtype (spec:canonical-dtype route-dtype))))
      (error 'v09-document-error
             :message (format nil "route dtype ~a does not match document dtype ~a"
                              route-dtype dtype)))
    (setf (jsown:val object "dtype") dtype)
    (unless (object-has-key-p object "schema_org")
      (setf (jsown:val object "schema_org")
            (spec:schema-org-metadata dtype (jsown:val object "_id"))))
    object))

(defun v09-document-json (document &key route-dtype)
  (jsown:to-json (ensure-v09-document document :route-dtype route-dtype)))
