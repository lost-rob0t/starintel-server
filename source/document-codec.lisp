(in-package :star.documents)

(define-condition unknown-document-dtype (error)
  ((dtype
    :initarg :dtype
    :reader unknown-document-dtype-value)
   (schema-version
    :initarg :schema-version
    :reader unknown-document-dtype-schema-version))
  (:report
   (lambda (condition stream)
     (format stream
             "No registered StarIntel document class for dtype ~s under schema ~a"
             (unknown-document-dtype-value condition)
             (unknown-document-dtype-schema-version condition)))))

(define-condition document-class-mismatch (error)
  ((dtype
    :initarg :dtype
    :reader document-class-mismatch-dtype)
   (registered-class
    :initarg :registered-class
    :reader document-class-mismatch-registered-class)
   (expected-class
    :initarg :expected-class
    :reader document-class-mismatch-expected-class))
  (:report
   (lambda (condition stream)
     (format stream
             "Document dtype ~a resolves to ~s, not requested class ~s"
             (document-class-mismatch-dtype condition)
             (class-name
              (document-class-mismatch-registered-class condition))
             (class-name
              (document-class-mismatch-expected-class condition))))))

(defclass document-class-registry ()
  ((profile
    :initarg :profile
    :reader class-registry-profile)
   (classes
    :initform (make-hash-table :test #'equal)
    :reader class-registry-classes)))

(defparameter *document-class-registries*
  (make-hash-table :test #'equal))

(defun profile-designator-profile (profile-designator)
  (etypecase profile-designator
    (document-schema-profile profile-designator)
    (string
     (or (find-schema-profile profile-designator)
         (error 'unsupported-document-schema
                :version profile-designator)))))

(defun class-registry-for-profile (profile-designator)
  (let* ((profile (profile-designator-profile profile-designator))
         (version (schema-profile-version profile)))
    (or (gethash version *document-class-registries*)
        (setf (gethash version *document-class-registries*)
              (make-instance 'document-class-registry
                             :profile profile)))))

(defun document-class-object (class-designator)
  (cond
    ((symbolp class-designator)
     (or (find-class class-designator nil)
         (error "Unknown class designator: ~s" class-designator)))
    ((ignore-errors (class-name class-designator))
     class-designator)
    (t
     (error "Unknown class designator: ~s" class-designator))))

(defun starintel-document-class-p (class)
  (let ((name (class-name class)))
    (and name
         (ignore-errors
           (nth-value 0 (subtypep name 'spec:document))))))

(defun register-document-class (profile-designator dtype class-designator)
  (let* ((registry (class-registry-for-profile profile-designator))
         (class (document-class-object class-designator))
         (canonical (spec:canonical-dtype dtype)))
    (unless (starintel-document-class-p class)
      (error "Class ~s is not a StarIntel document class"
             (class-name class)))
    (setf (gethash canonical (class-registry-classes registry)) class)
    class))

(defun registered-document-class (profile-designator dtype &key (errorp t))
  (let* ((profile (profile-designator-profile profile-designator))
         (canonical (spec:canonical-dtype dtype))
         (registry (class-registry-for-profile profile))
         (class (gethash canonical (class-registry-classes registry))))
    (cond
      (class class)
      (errorp
       (error 'unknown-document-dtype
              :dtype dtype
              :schema-version (schema-profile-version profile)))
      (t nil))))

(defun registered-document-dtypes (profile-designator)
  (let ((registry (class-registry-for-profile profile-designator)))
    (sort
     (loop for dtype being the hash-keys of (class-registry-classes registry)
           collect dtype)
     #'string<)))

(defun register-runtime-document-classes (profile-designator)
  "Build the dtype registry from exported CLOS document classes once at load time."
  (let ((package (find-package :starintel)))
    (loop for symbol being the external-symbols of package
          for class = (find-class symbol nil)
          when (and class (starintel-document-class-p class))
            do (register-document-class
                profile-designator
                (spec:canonical-dtype
                 (string-downcase (symbol-name symbol)))
                class)))
  (class-registry-for-profile profile-designator))

(defun parse-document-object (document)
  (etypecase document
    (string
     (jsown:with-injective-reader
       (jsown:parse document)))
    (list document)))

(defun document-field-value (document key &optional default)
  "Read an envelope or dtype-specific field without conflating absence and false."
  (let ((object (parse-document-object document)))
    (multiple-value-bind (value presentp) (object-get object key)
      (if presentp
          value
          (profile-document-value
           (schema-profile-for-document object)
           object
           key
           default)))))

(defun document-dataset (document)
  (document-field-value document "dataset" ""))

(defun document-date-updated (document)
  (let ((object (parse-document-object document)))
    (or (object-value object "date_updated")
        (object-value object "dateUpdated")
        (document-date-added object))))

(defun decode-payload-for-profile (profile object)
  (etypecase profile
    (v09-schema-profile
     (unless (v09-document-p object)
       (error 'v09-document-error
              :message "payload is not a canonical StarIntel v0.9.0 document"))
     object)
    (v08-schema-profile
     (profile-normalize-for-index profile object))))

(defun expected-document-class (expected-class)
  (when expected-class
    (let ((class (document-class-object expected-class)))
      (unless (starintel-document-class-p class)
        (error "Expected class ~s is not a StarIntel document class"
               (class-name class)))
      class)))

(defun decode-document (document &key expected-class)
  "Decode through the schema profile and registered dtype class.

No request-controlled symbol is interned. Unknown dtypes fail before SPEC:DECODE."
  (let* ((object (parse-document-object document))
         (profile (schema-profile-for-document object))
         (dtype (document-dtype object))
         (registered-class (registered-document-class profile dtype))
         (expected (expected-document-class expected-class)))
    (when (and expected (not (eq expected registered-class)))
      (error 'document-class-mismatch
             :dtype dtype
             :registered-class registered-class
             :expected-class expected))
    (spec:decode
     (decode-payload-for-profile profile object)
     (class-name registered-class))))

(eval-when (:load-toplevel :execute)
  (clrhash *document-class-registries*)
  (register-runtime-document-classes "0.9.0")
  (register-runtime-document-classes "0.8.0"))
