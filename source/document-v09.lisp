(in-package :star.documents)

(define-condition v09-document-error (error)
  ((message :initarg :message :reader v09-document-error-message))
  (:report (lambda (condition stream)
             (format stream "~a" (v09-document-error-message condition)))))

(defun document-data (document)
  (let ((profile (schema-profile-for-document document)))
    (profile-document-data profile document)))

(defun document-value (document key &optional default)
  "Read KEY through the versioned schema profile selected for DOCUMENT."
  (let ((profile (schema-profile-for-document document)))
    (profile-document-value profile document key default)))

(defun document-dtype (document)
  (spec:canonical-dtype
   (or (object-value document "dtype") "document")))

(defun document-date-added (document)
  (let ((profile (schema-profile-for-document document)))
    (profile-document-date-added profile document)))

(defun document-transient-p (document)
  (let ((value (document-value document "transient" nil)))
    (or (eq value t) (eq value :true))))

(defun required-key-present-p (document key)
  (object-has-key-p document key))

(defun couchdb-revision-p (value)
  (and (stringp value)
       (> (length value) 2)
       (let ((dash (position #\- value)))
         (and dash
              (> dash 0)
              (< dash (1- (length value)))
              (every #'digit-char-p (subseq value 0 dash))
              (every #'alphanumericp (subseq value (1+ dash)))))))

(defun valid-optional-revision-p (document)
  (or (not (object-has-key-p document "_rev"))
      (couchdb-revision-p (object-value document "_rev"))))

(defun v09-document-p (document)
  (and document
       (handler-case
           (typep (schema-profile-for-document document)
                  'v09-schema-profile)
         (unsupported-document-schema () nil))
       (every (lambda (key)
                (required-key-present-p document key))
              '("_id" "dataset" "dtype" "schema_version" "version"
                "date_added" "date_updated" "sources" "evidence" "data"))
       (every (lambda (key)
                (profile-top-level-key-p
                 (find-schema-profile "0.9.0")
                 key))
              (object-keys document))
       (valid-optional-revision-p document)
       (string= (or (object-value document "schema_version") "")
                "0.9.0")
       (integerp (object-value document "version"))
       (stringp (object-value document "date_added"))
       (stringp (object-value document "date_updated"))))

(defun parse-document (document)
  (etypecase document
    (string
     (jsown:with-injective-reader
       (jsown:parse document)))
    (list document)))

(defun ensure-v09-document (document &key route-dtype)
  "Parse and validate the canonical v0.9 envelope at a write boundary."
  (let ((object (parse-document document)))
    (handler-case
        (writable-schema-profile-for-document object)
      (unsupported-document-schema (condition)
        (error 'v09-document-error
               :message (princ-to-string condition)))
      (read-only-document-schema (condition)
        (error 'v09-document-error
               :message (princ-to-string condition))))
    (let ((dtype (document-dtype object)))
      (unless (v09-document-p object)
        (error 'v09-document-error
               :message "payload is not a canonical StarIntel v0.9.0 document"))
      (when (and route-dtype
                 (not (string= dtype
                               (spec:canonical-dtype route-dtype))))
        (error 'v09-document-error
               :message
               (format nil
                       "route dtype ~a does not match document dtype ~a"
                       route-dtype
                       dtype)))
      (setf (jsown:val object "dtype") dtype)
      (unless (object-has-key-p object "schema_org")
        (setf (jsown:val object "schema_org")
              (spec:schema-org-metadata
               dtype
               (jsown:val object "_id"))))
      object)))

(defun v09-document-json (document &key route-dtype)
  (jsown:to-json
   (ensure-v09-document document :route-dtype route-dtype)))
