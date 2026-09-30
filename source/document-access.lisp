(in-package :star.documents)

(define-condition document-schema-validation-error (error)
  ((category
    :initarg :category
    :reader document-schema-validation-category)
   (reason
    :initarg :reason
    :reader document-schema-validation-reason))
  (:report
   (lambda (condition stream)
     (format stream "StarIntel document validation failed (~a): ~a"
             (document-schema-validation-category condition)
             (document-schema-validation-reason condition))))
  (:documentation "Signalled when a document fails v0.9 schema validation."))

;; Accessor documentation for document-schema-validation-error
(setf (documentation 'DOCUMENT-SCHEMA-VALIDATION-CATEGORY 'function)
"The =category= slot of =document-schema-validation-error=.")
(setf (documentation 'DOCUMENT-SCHEMA-VALIDATION-REASON 'function)
"The =reason= slot of =document-schema-validation-error=.")



(defvar *v09-schema* nil)
(defvar *v09-schema-lock* (bt:make-lock "starintel-v09-schema"))

(defun object-has-key-p (object key)
  "True when a parsed JSON object contains KEY."
  (and object
       (handler-case
           (progn
             (jsown:val object key)
             t)
         (error () nil))))

(defun object-value (object key &optional default)
  "Fetch the value at KEY of a parsed JSON object."
  (if (object-has-key-p object key)
      (jsown:val object key)
      default))

(defun object-keys (object)
  "Keywords of a parsed JSON object."
  (let ((keys nil))
    (when object
      (jsown:do-json-keys (key value) object
        (push key keys)))
    (nreverse keys)))

(defun parse-document-object (document)
  "Parse a document payload into the internal JSOWN form."
  (etypecase document
    (string
     (jsown:with-injective-reader
       (jsown:parse document)))
    (list document)))

(defun clone-document-object (document)
  "Deep copy a parsed document object."
  (jsown:with-injective-reader
    (jsown:parse (jsown:to-json (parse-document-object document)))))

(defun canonical-dtype (dtype)
  "Canonical string form of a document dtype."
  (let ((token
          (substitute #\- #\_
                      (string-downcase (string dtype)))))
    (cond
      ((member token '("organization" "organisation") :test #'string=)
       "org")
      ((string= token "investigation-target") "target")
      ((string= token "social-media-posts") "social-media-post")
      (t token))))

(defun document-id (document)
  "The canonical =id= of a document object, with legacy =_id= fallback."
  (let ((object (parse-document-object document)))
    (or (object-value object "id" nil)
        (object-value object "_id" nil))))

(defun document-data (document)
  "The =data= payload of a document object."
  (let* ((object (parse-document-object document))
         (data (object-value object "data" nil)))
    (if (and (consp data) (eq (first data) :obj))
        data
        object)))

(defun document-value (document key &optional default)
  "Generic accessor into a document object cell."
  (let* ((object (parse-document-object document))
         (data (document-data object)))
    (cond
      ((object-has-key-p data key) (object-value data key))
      ((object-has-key-p object key) (object-value object key))
      (t default))))

(defun document-dtype (document)
  "The =dtype= field of a document object."
  (let ((dtype (object-value (parse-document-object document) "dtype" nil)))
    (and dtype (canonical-dtype dtype))))

(defun document-dataset (document)
  "The =dataset= field of a document object."
  (object-value (parse-document-object document) "dataset" nil))

(defun document-date-added (document)
  "The =dateAdded= field of a document object."
  (let ((object (parse-document-object document)))
    (or (object-value object "dateAdded" nil)
        (object-value object "date_added" nil))))

(defun document-date-updated (document)
  "The =dateUpdated= field of a document object."
  (let ((object (parse-document-object document)))
    (or (object-value object "dateUpdated" nil)
        (object-value object "date_updated" nil))))

(defun jzon-to-jsown (value)
  (jsown:with-injective-reader
    (jsown:parse (com.inuoe.jzon:stringify value))))

(defun jsown-to-jzon (value)
  (com.inuoe.jzon:parse (jsown:to-json value)))

(defun validate-v0101-document (document)
  "Validate one canonical 0.10.1 document through star-cl."
  (handler-case
      (progn
        (starintel-v0101:validate-document (jsown-to-jzon document))
        document)
    (starintel-v0101:starintel-validation-error (condition)
      (error 'document-schema-validation-error
             :category (starintel-v0101:validation-category condition)
             :reason (starintel-v0101:validation-message condition)))))

(defun migrate-v0101-document (document)
  "Migrate one legacy or canonical document and return canonical documents.

The returned vector can contain derived documents, such as person identifiers
or transcripts. Snake-case aliases are accepted only inside star-cl's explicit
legacy migration boundary; returned documents are canonical lowerCamelCase."
  (handler-case
      (map 'vector #'jzon-to-jsown
           (starintel-v0101:migrate-document (jsown-to-jzon document)))
    (starintel-v0101:migration-error (condition)
      (error 'document-schema-validation-error
             :category (starintel-v0101:migration-reason-code condition)
             :reason (princ-to-string condition)))
    (starintel-v0101:starintel-validation-error (condition)
      (error 'document-schema-validation-error
             :category (starintel-v0101:validation-category condition)
             :reason (starintel-v0101:validation-message condition)))
    (error (condition)
      (error 'document-schema-validation-error
             :category "migrationFailed"
             :reason (princ-to-string condition)))))

(defun document-transient-p (document)
  "True when the document is marked transient."
  (let ((value (document-value document "transient" nil)))
    (or (eq value t) (eq value :true))))

(defun v09-schema-path ()
  "Return the schema path shipped with the loaded STARINTEL ASDF system."
  (merge-pathnames
   "../schemas/starintel-doc-v0.9.0.schema.json"
   (asdf:system-source-directory :starintel)))

(defun load-v09-schema ()
  (let ((path (v09-schema-path)))
    (unless (probe-file path)
      (error "StarIntel v0.9 schema not found: ~a" path))
    (com.inuoe.jzon:parse (pathname path))))

(defun v09-schema ()
  (or *v09-schema*
      (bt:with-lock-held (*v09-schema-lock*)
        (or *v09-schema*
            (setf *v09-schema* (load-v09-schema))))))

(defun validate-v09-document (document)
  "Validate DOCUMENT with star-cl's canonical StarIntel v0.9 validator.

The server owns only schema discovery, JSOWN-to-Jzon conversion, and a stable
condition. The schema rules and validator remain owned by star-cl."
  (let* ((object (parse-document-object document))
         (jzon-object
           (com.inuoe.jzon:parse (jsown:to-json object))))
    (handler-case
        (progn
          (starintel::validate-v090-document jzon-object (v09-schema))
          object)
      (starintel::starintel-validation-error (condition)
        (error 'document-schema-validation-error
               :category (starintel::validation-category condition)
               :reason (starintel::validation-message condition))))))

(defun ensure-document (document &key route-dtype)
  "Parse DOCUMENT and enforce transport-level identity invariants.

Strict canonical validation is an explicit boundary operation so legacy target
compatibility adapters can opt out without weakening canonical ingest."
  (let* ((object (parse-document-object document))
         (dtype (document-dtype object))
         (route (and route-dtype (canonical-dtype route-dtype))))
    (unless (and (consp object) (eq (first object) :obj))
      (error "Document payload must be a JSON object"))
    (unless dtype
      (if route
          (progn
            (setf (jsown:val object "dtype") route)
            (setf dtype route))
          (error "Document payload is missing dtype")))
    (when (and route (not (string= dtype route)))
      (error "Route dtype ~a does not match document dtype ~a" route dtype))
    ;; Canonical 0.10.1 documents carry =id=.  Do not leak CouchDB's private
    ;; =_id= field back onto the wire.  Legacy documents that have neither
    ;; form still receive their historical transport identifier.
    (unless (or (let ((id (object-value object "id" nil)))
                  (and (stringp id) (plusp (length id))))
                (let ((id (object-value object "_id" nil)))
                  (and (stringp id) (plusp (length id)))))
      (setf (jsown:val object "_id") (star.ids:ulid)))
    object))

(defun document-json (document &key route-dtype)
  "Serialize DOCUMENT after enforcing wire identity and dtype invariants.

Canonical 0.10.1 documents retain =id= and never gain CouchDB's private
=_id= field. Legacy documents retain the historical normalization path."
  (jsown:to-json (ensure-document document :route-dtype route-dtype)))

(defun utc-now ()
  "Current UTC timestamp as an ISO-8601 string."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))
