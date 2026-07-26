(in-package :star.documents)

(defun object-has-key-p (object key)
  (and object
       (handler-case
           (progn
             (jsown:val object key)
             t)
         (error () nil))))

(defun object-value (object key &optional default)
  (if (object-has-key-p object key)
      (jsown:val object key)
      default))

(defun object-keys (object)
  (let ((keys nil))
    (when object
      (jsown:do-json-keys (key value) object
        (push key keys)))
    (nreverse keys)))

(defun parse-document-object (document)
  (etypecase document
    (string
     (jsown:with-injective-reader
       (jsown:parse document)))
    (list document)))

(defun clone-document-object (document)
  (jsown:with-injective-reader
    (jsown:parse (jsown:to-json (parse-document-object document)))))

(defun canonical-dtype (dtype)
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
  (object-value (parse-document-object document) "_id" nil))

(defun document-data (document)
  (let* ((object (parse-document-object document))
         (data (object-value object "data" nil)))
    (if (and (consp data) (eq (first data) :obj))
        data
        object)))

(defun document-value (document key &optional default)
  (let* ((object (parse-document-object document))
         (data (document-data object)))
    (cond
      ((object-has-key-p data key) (object-value data key))
      ((object-has-key-p object key) (object-value object key))
      (t default))))

(defun document-dtype (document)
  (let ((dtype (object-value (parse-document-object document) "dtype" nil)))
    (and dtype (canonical-dtype dtype))))

(defun document-dataset (document)
  (object-value (parse-document-object document) "dataset" nil))

(defun document-date-added (document)
  (let ((object (parse-document-object document)))
    (or (object-value object "date_added" nil)
        (object-value object "dateAdded" nil))))

(defun document-date-updated (document)
  (let ((object (parse-document-object document)))
    (or (object-value object "date_updated" nil)
        (object-value object "dateUpdated" nil))))

(defun document-transient-p (document)
  (let ((value (document-value document "transient" nil)))
    (or (eq value t) (eq value :true))))

(defun ensure-document (document &key route-dtype)
  "Parse DOCUMENT and enforce the transport-level identity invariants.

Schema validation remains owned by star-cl. This accessor only guarantees a
JSON object, a stable _id, and a route-compatible dtype."
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
    (unless (let ((id (object-value object "_id" nil)))
              (and (stringp id) (plusp (length id))))
      (setf (jsown:val object "_id") (cms-ulid:ulid)))
    object))

(defun document-json (document &key route-dtype)
  (jsown:to-json (ensure-document document :route-dtype route-dtype)))

(defun utc-now ()
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))
