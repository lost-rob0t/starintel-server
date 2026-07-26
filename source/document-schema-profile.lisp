(in-package :star.documents)

(define-condition unsupported-document-schema (error)
  ((version :initarg :version :reader unsupported-document-schema-version))
  (:report (lambda (condition stream)
             (format stream "Unsupported StarIntel document schema: ~s"
                     (unsupported-document-schema-version condition)))))

(define-condition read-only-document-schema (error)
  ((version :initarg :version :reader read-only-document-schema-version))
  (:report (lambda (condition stream)
             (format stream
                     "StarIntel schema ~a is read-only; canonical writes require 0.9.0"
                     (read-only-document-schema-version condition)))))

(defclass document-schema-profile ()
  ((version
    :initarg :version
    :reader schema-profile-version)
   (writable-p
    :initarg :writable-p
    :initform nil
    :reader schema-profile-writable-p)))

(defclass v09-schema-profile (document-schema-profile) ())
(defclass v08-schema-profile (document-schema-profile) ())

(defparameter *schema-profile-registry* (make-hash-table :test #'equal))
(defparameter *schema-profile-order* nil)

(defun object-has-key-p (object key)
  (handler-case
      (progn
        (jsown:val object key)
        t)
    (error () nil)))

(defun object-get (object key)
  (if (and object (object-has-key-p object key))
      (values (jsown:val object key) t)
      (values nil nil)))

(defun object-value (object key &optional default)
  (multiple-value-bind (value presentp) (object-get object key)
    (if presentp value default)))

(defun object-keys (object)
  (let ((keys nil))
    (jsown:do-json-keys (key value) object
      (declare (ignore value))
      (push key keys))
    (nreverse keys)))

(defun put-json-value (object key value)
  (setf (jsown:val object key) value)
  object)

(defun register-schema-profile (profile)
  (check-type profile document-schema-profile)
  (setf (gethash (schema-profile-version profile) *schema-profile-registry*)
        profile)
  (setf *schema-profile-order*
        (cons profile
              (remove (schema-profile-version profile)
                      *schema-profile-order*
                      :key #'schema-profile-version
                      :test #'string=)))
  profile)

(defun find-schema-profile (version)
  (gethash version *schema-profile-registry*))

(defgeneric profile-matches-document-p (profile document))
(defgeneric profile-wire-key (profile slot-name))
(defgeneric profile-envelope-slot-p (profile slot-name))
(defgeneric profile-top-level-key-p (profile key))
(defgeneric profile-document-data (profile document))
(defgeneric profile-document-value (profile document key default))
(defgeneric profile-document-date-added (profile document))
(defgeneric profile-normalize-for-index (profile document))

(defmethod profile-wire-key ((profile v09-schema-profile) slot-name)
  (declare (ignore profile))
  (substitute #\_ #\-
              (string-downcase (string slot-name))))

(defmethod profile-wire-key ((profile v08-schema-profile) slot-name)
  (declare (ignore profile))
  (let ((name (string slot-name)))
    (if (and (> (length name) 0)
             (char= (char name 0) #\_))
        (string-downcase name)
        (str:camel-case name))))

(defparameter +v08-envelope-slot-names+
  '(_id _rev dataset dtype sources version date-added date-updated))

(defmethod profile-envelope-slot-p ((profile v09-schema-profile) slot-name)
  (declare (ignore profile))
  (spec:document-envelope-slot-p slot-name))

(defmethod profile-envelope-slot-p ((profile v08-schema-profile) slot-name)
  (declare (ignore profile))
  (member slot-name +v08-envelope-slot-names+ :test #'eq))

(defun schema-version-token (document)
  (or (object-value document "schema_version")
      (object-value document "schemaVersion")
      (let ((version (object-value document "version")))
        (and (stringp version) version))))

(defun version-prefix-p (prefix value)
  (and (stringp value)
       (<= (length prefix) (length value))
       (string= prefix value :end2 (length prefix))))

(defmethod profile-matches-document-p ((profile v09-schema-profile) document)
  (string= (or (schema-version-token document) "")
           (schema-profile-version profile)))

(defmethod profile-matches-document-p ((profile v08-schema-profile) document)
  (let ((token (schema-version-token document)))
    (or (version-prefix-p "0.8" token)
        (and (null token)
             (object-has-key-p document "dtype")
             (or (integerp (object-value document "dateAdded"))
                 (integerp (object-value document "dateUpdated")))))))

(defun schema-profile-for-document (document)
  (or (find-if (lambda (profile)
                 (profile-matches-document-p profile document))
               *schema-profile-order*)
      (error 'unsupported-document-schema
             :version (schema-version-token document))))

(defun writable-schema-profile-for-document (document)
  (let ((profile (schema-profile-for-document document)))
    (unless (schema-profile-writable-p profile)
      (error 'read-only-document-schema
             :version (schema-profile-version profile)))
    profile))

(defun document-class-for-dtype (dtype)
  (let ((canonical (spec:canonical-dtype dtype))
        (package (find-package :starintel)))
    (loop for symbol being the external-symbols of package
          for class = (find-class symbol nil)
          when (and class
                    (nth-value 0 (subtypep symbol 'spec:document))
                    (string= canonical
                             (spec:canonical-dtype
                              (string-downcase (symbol-name symbol)))))
            return class)))

(defun profile-data-slot-map (from-profile to-profile dtype)
  "Return legacy-wire-key -> canonical-wire-key pairs derived from CLOS slots."
  (let ((class (document-class-for-dtype dtype)))
    (when class
      (closer-mop:finalize-inheritance class)
      (loop for slot in (closer-mop:class-slots class)
            for slot-name = (closer-mop:slot-definition-name slot)
            unless (profile-envelope-slot-p to-profile slot-name)
              collect (cons (profile-wire-key from-profile slot-name)
                            (profile-wire-key to-profile slot-name))))))

(defun profile-envelope-wire-keys (profile)
  (etypecase profile
    (v08-schema-profile
     (mapcar (lambda (slot-name)
               (profile-wire-key profile slot-name))
             +v08-envelope-slot-names+))
    (v09-schema-profile
     (let ((class (find-class 'spec:document)))
       (closer-mop:finalize-inheritance class)
       (loop for slot in (closer-mop:class-slots class)
             for slot-name = (closer-mop:slot-definition-name slot)
             when (profile-envelope-slot-p profile slot-name)
               collect (profile-wire-key profile slot-name))))))

(defmethod profile-top-level-key-p ((profile document-schema-profile) key)
  (member key (profile-envelope-wire-keys profile) :test #'string=))

(defun copy-profile-data-slots (document from-profile to-profile)
  (let* ((dtype (spec:canonical-dtype
                 (or (object-value document "dtype") "document")))
         (mappings (profile-data-slot-map from-profile to-profile dtype))
         (mapped-legacy-keys (mapcar #'car mappings))
         (envelope-keys (profile-envelope-wire-keys from-profile))
         (data (jsown:empty-object)))
    (dolist (mapping mappings)
      (multiple-value-bind (value presentp)
          (object-get document (car mapping))
        (when presentp
          (put-json-value data (cdr mapping) value))))
    ;; Preserve custom legacy fields for indexing without treating them as
    ;; canonical envelope fields. Known CLOS slots use their v0.9 wire names;
    ;; unknown legacy fields retain their original key.
    (jsown:do-json-keys (key value) document
      (unless (or (member key envelope-keys :test #'string=)
                  (member key mapped-legacy-keys :test #'string=))
        (put-json-value data key value)))
    data))

(defmethod profile-document-data ((profile v09-schema-profile) document)
  (declare (ignore profile))
  (object-value document "data" (jsown:empty-object)))

(defmethod profile-document-data ((profile v08-schema-profile) document)
  (copy-profile-data-slots
   document
   profile
   (or (find-schema-profile "0.9.0")
       (error 'unsupported-document-schema :version "0.9.0"))))

(defmethod profile-document-value ((profile v09-schema-profile)
                                   document key default)
  (multiple-value-bind (value presentp)
      (object-get (profile-document-data profile document) key)
    (if presentp value default)))

(defmethod profile-document-value ((profile v08-schema-profile)
                                   document key default)
  (let* ((target-profile
           (or (find-schema-profile "0.9.0")
               (error 'unsupported-document-schema :version "0.9.0")))
         (dtype (spec:canonical-dtype
                 (or (object-value document "dtype") "document")))
         (mapping (find key
                        (profile-data-slot-map profile target-profile dtype)
                        :key #'cdr
                        :test #'string=))
         (legacy-key (and mapping (car mapping))))
    (multiple-value-bind (value presentp)
        (if legacy-key
            (object-get document legacy-key)
            (object-get document key))
      (if presentp value default))))

(defmethod profile-document-date-added ((profile v09-schema-profile) document)
  (declare (ignore profile))
  (object-value document "date_added" ""))

(defmethod profile-document-date-added ((profile v08-schema-profile) document)
  (declare (ignore profile))
  (or (object-value document "dateAdded")
      (object-value document "date_added")
      0))

(defun unix-seconds-to-iso8601 (seconds)
  (if (integerp seconds)
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time
           (+ seconds (encode-universal-time 0 0 0 1 1 1970 0))
           0)
        (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
                year month day hour minute second))
      seconds))

(defun normalize-source-for-index (source)
  (if (stringp source)
      (let ((object (jsown:empty-object)))
        (put-json-value object "kind" "web")
        (put-json-value object "name" source)
        (put-json-value object "uri" source)
        (put-json-value object "url" source)
        object)
      source))

(defun normalize-sources-for-index (sources)
  (mapcar #'normalize-source-for-index (or sources nil)))

(defmethod profile-normalize-for-index ((profile v09-schema-profile) document)
  (declare (ignore profile))
  document)

(defmethod profile-normalize-for-index ((profile v08-schema-profile) document)
  (let* ((dtype (spec:canonical-dtype
                 (or (object-value document "dtype") "document")))
         (object (jsown:empty-object))
         (extensions (jsown:empty-object))
         (date-added (or (object-value document "dateAdded")
                         (object-value document "date_added")
                         0))
         (date-updated (or (object-value document "dateUpdated")
                           (object-value document "date_updated")
                           date-added)))
    (put-json-value object "_id" (or (object-value document "_id") ""))
    (when (object-has-key-p document "_rev")
      (put-json-value object "_rev" (object-value document "_rev")))
    (put-json-value object "dataset" (or (object-value document "dataset") ""))
    (put-json-value object "dtype" dtype)
    ;; Preserve source truth: this is a v0.8 record in a v0.9-shaped index
    ;; projection, not an in-place migration.
    (put-json-value object "schema_version" (schema-profile-version profile))
    (put-json-value object "version" 1)
    (put-json-value object "date_added"
                    (unix-seconds-to-iso8601 date-added))
    (put-json-value object "date_updated"
                    (unix-seconds-to-iso8601 date-updated))
    (put-json-value object "sources"
                    (normalize-sources-for-index
                     (object-value document "sources" nil)))
    (put-json-value object "evidence" #())
    (put-json-value object "data" (profile-document-data profile document))
    (put-json-value object "schema_org"
                    (spec:schema-org-metadata
                     dtype
                     (object-value document "_id" "")))
    (put-json-value extensions "index_schema_version" "0.9.0")
    (put-json-value extensions "adapter" "starintel-v08-index-adapter")
    (put-json-value object "extensions" extensions)
    object))

(defun normalize-document-for-index (document)
  (let ((object (etypecase document
                  (string
                   (jsown:with-injective-reader
                     (jsown:parse document)))
                  (list document))))
    (profile-normalize-for-index
     (schema-profile-for-document object)
     object)))

(eval-when (:load-toplevel :execute)
  (register-schema-profile
   (make-instance 'v09-schema-profile
                  :version "0.9.0"
                  :writable-p t))
  (register-schema-profile
   (make-instance 'v08-schema-profile
                  :version "0.8.0"
                  :writable-p nil)))
