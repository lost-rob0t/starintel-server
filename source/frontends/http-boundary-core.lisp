(in-package :star.frontends.http-api)

(defparameter +http-max-body-bytes+ (* 1024 1024))
(defparameter +http-max-query-limit+ 100)

(defvar *http-correlation-id* nil)

(define-condition http-input-error (error)
  ((status
    :initarg :status
    :reader http-input-error-status)
   (code
    :initarg :code
    :reader http-input-error-code)
   (message
    :initarg :message
    :reader http-input-error-message)
   (info
    :initarg :info
    :initform nil
    :reader http-input-error-info))
  (:report
   (lambda (condition stream)
     (format stream "~a" (http-input-error-message condition)))))

(defun new-correlation-id ()
  (cms-ulid:ulid))

(defun current-correlation-id ()
  (or *http-correlation-id*
      (new-correlation-id)))

(defun set-correlation-id-header ()
  (setf (lack.response:response-headers *response*)
        (append (lack.response:response-headers *response*)
                (list :x-correlation-id (current-correlation-id)))))

(defun status-msg (msg status &key info traceback code)
  "Return a client-safe status envelope. TRACEBACK is intentionally ignored."
  (declare (ignore traceback))
  (let ((json (jsown:new-js
                ("msg" msg)
                ("status" (string-downcase (symbol-name status)))
                ("correlation_id" (current-correlation-id)))))
    (when code
      (setf (jsown:val json "code") code))
    (when info
      (setf (jsown:val json "info") info))
    (jsown:to-json json)))

(defun signal-http-input-error (status code message &optional info)
  (error 'http-input-error
         :status status
         :code code
         :message message
         :info info))

(defun respond-http-input-error (condition)
  (setf (lack.response:response-status *response*)
        (http-input-error-status condition))
  (status-msg (http-input-error-message condition)
              'error
              :code (http-input-error-code condition)
              :info (http-input-error-info condition)))

(defmacro with-http-boundary (() &body body)
  `(let ((*http-correlation-id* (new-correlation-id)))
     (set-default-headers)
     (set-correlation-id-header)
     (handler-case
         (progn ,@body)
       (http-input-error (condition)
         (log:warn "HTTP input rejected correlation=~a code=~a: ~a"
                   (current-correlation-id)
                   (http-input-error-code condition)
                   condition)
         (respond-http-input-error condition))
       (bt:timeout (condition)
         (log:error "HTTP operation timed out correlation=~a: ~a"
                    (current-correlation-id)
                    condition)
         (setf (lack.response:response-status *response*) 504)
         (status-msg "Request deadline exceeded"
                     'error
                     :code "request_timeout"))
       (error (condition)
         (log:error "HTTP internal error correlation=~a: ~a"
                    (current-correlation-id)
                    condition)
         (setf (lack.response:response-status *response*) 500)
         (status-msg "Internal Server Error"
                     'error
                     :code "internal_error")))))

(defun json-object-p (value)
  (and (consp value)
       (eq (car value) :obj)))

(defun json-array-p (value)
  (or (null value)
      (and (listp value)
           (not (json-object-p value)))))

(defun request-content-type (&optional (request (ningle:context :request)))
  (or (ignore-errors (lack.request:request-content-type request))
      (getf (lack.request:request-env request) :content-type)))

(defun json-content-type-p (content-type)
  (when (stringp content-type)
    (let ((normalized (string-downcase content-type)))
      (or (search "application/json" normalized)
          (search "+json" normalized)))))

(defun request-body-octets (&optional (request (ningle:context :request)))
  (let ((content (lack.request:request-content request)))
    (etypecase content
      ((simple-array (unsigned-byte 8) (*)) content)
      (string (babel:string-to-octets content :encoding :utf-8))
      (vector content)
      (null #()))))

(defun parse-json-octets (octets content-type
                          &key (max-bytes +http-max-body-bytes+))
  (unless (json-content-type-p content-type)
    (signal-http-input-error
     415
     "unsupported_media_type"
     "Content-Type must be application/json"))
  (when (> (length octets) max-bytes)
    (signal-http-input-error
     413
     "request_body_too_large"
     "Request body exceeds the configured limit"
     (jsown:new-js ("maximum_bytes" max-bytes))))
  (handler-case
      (let ((text (babel:octets-to-string octets :encoding :utf-8)))
        ;; Validate with the packaged standards-oriented parser, then retain
        ;; JSOWN as the service's existing internal document representation.
        (yason:parse text)
        (jsown:parse text))
    (error ()
      (signal-http-input-error
       400
       "malformed_json"
       "Request body contains malformed JSON"))))

(defun parse-json-request (&key (max-bytes +http-max-body-bytes+))
  (let* ((request (ningle:context :request))
         (content-type (request-content-type request))
         (octets (request-body-octets request)))
    (parse-json-octets octets content-type :max-bytes max-bytes)))

(defun require-json-object (value)
  (unless (json-object-p value)
    (signal-http-input-error
     400
     "json_object_required"
     "Request body must be a JSON object"))
  value)

(defun require-json-array (value)
  (unless (json-array-p value)
    (signal-http-input-error
     400
     "json_array_required"
     "Request body must be a JSON array"))
  value)

(defun non-empty-string-p (value)
  (and (stringp value)
       (plusp (length value))))

(defun require-document-string (document field &key index)
  (let ((value (jsown:val-safe document field)))
    (unless (non-empty-string-p value)
      (signal-http-input-error
       422
       "invalid_document"
       (if index
           (format nil "Document at index ~d requires a non-empty ~a field"
                   index field)
           (format nil "Document requires a non-empty ~a field" field))
       (jsown:new-js ("field" field)
                     ("index" (or index :null)))))
    value))

(defun validate-schema-version (document &key index)
  (let ((version (jsown:val-safe document "version"))
        (expected starintel:+starintel-doc-version+))
    (unless version
      (signal-http-input-error
       422
       "schema_version_required"
       (if index
           (format nil "Document at index ~d requires a version field" index)
           "Document requires a version field")))
    (unless (string= (princ-to-string version)
                     (princ-to-string expected))
      (signal-http-input-error
       422
       "unsupported_schema_version"
       "Document schema version is not supported"
       (jsown:new-js ("expected" (princ-to-string expected))
                     ("received" (princ-to-string version))
                     ("index" (or index :null)))))))

(defun validate-document-input (document &key path-dtype index)
  (unless (json-object-p document)
    (signal-http-input-error
     422
     "invalid_document"
     (if index
         (format nil "Document at index ~d must be a JSON object" index)
         "Document must be a JSON object")))
  (require-document-string document "_id" :index index)
  (require-document-string document "dataset" :index index)
  (let ((dtype (require-document-string document "dtype" :index index)))
    (when (and path-dtype
               (not (string-equal dtype path-dtype)))
      (signal-http-input-error
       422
       "dtype_mismatch"
       "Document dtype does not match the route dtype"
       (jsown:new-js ("path_dtype" path-dtype)
                     ("document_dtype" dtype))))
    (validate-schema-version document :index index)
    document))

(defun query-value (params name)
  (or (cdr (assoc name params :test #'string=))
      (cdr (assoc (intern (string-upcase name) :keyword)
                  params
                  :test #'eq))))

(defun require-query-string (params name)
  (let ((value (query-value params name)))
    (unless (non-empty-string-p value)
      (signal-http-input-error
       400
       "missing_query_parameter"
       (format nil "Query parameter ~a is required" name)))
    value))

(defun bounded-query-integer (params name &key default (minimum 0)
                                            (maximum +http-max-query-limit+))
  (let ((raw (query-value params name)))
    (when (and (null raw) default)
      (return-from bounded-query-integer default))
    (unless raw
      (signal-http-input-error
       400
       "missing_query_parameter"
       (format nil "Query parameter ~a is required" name)))
    (let ((value
            (handler-case
                (parse-integer raw :junk-allowed nil)
              (error ()
                (signal-http-input-error
                 400
                 "invalid_query_parameter"
                 (format nil "Query parameter ~a must be an integer" name))))))
      (unless (<= minimum value maximum)
        (signal-http-input-error
         400
         "query_parameter_out_of_range"
         (format nil "Query parameter ~a must be between ~d and ~d"
                 name minimum maximum)))
      value)))

(defun request-header-value (headers name)
  (cond
    ((hash-table-p headers)
     (or (gethash (string-downcase name) headers)
         (gethash name headers)))
    ((and (listp headers) (consp (car headers)))
     (cdr (assoc name headers :test #'string-equal)))
    (t nil)))

(defun request-principal (&optional (request (ningle:context :request)))
  (let* ((headers (ignore-errors (lack.request:request-headers request)))
         (authorization (request-header-value headers "authorization"))
         (remote-address
           (getf (lack.request:request-env request) :remote-addr)))
    (cond
      (authorization
       (format nil "auth-~x" (sxhash authorization)))
      ((non-empty-string-p remote-address)
       (format nil "remote-~a" remote-address))
      (t "anonymous"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(http-input-error
            http-input-error-status
            http-input-error-code
            json-object-p
            json-array-p
            parse-json-octets
            validate-document-input
            bounded-query-integer)
          :star.frontends.http-api))
