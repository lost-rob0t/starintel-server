(in-package :star.databases.couchdb)

(defstruct (couchdb-view-request
             (:constructor make-couchdb-view-request (method uri body)))
  method
  uri
  body)

(define-condition view-query-error (error)
  ((reason :initarg :reason :reader view-query-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid CouchDB view query: ~a"
             (view-query-error-reason condition)))))

(defun reject-view-query (control &rest arguments)
  (error 'view-query-error :reason (apply #'format nil control arguments)))

(defun validate-view-boolean (name value)
  (unless (or (eq value t) (null value))
    (reject-view-query "~a must be T or NIL, got ~s" name value))
  value)

(defun validate-nonnegative-integer (name value)
  (unless (and (integerp value) (not (minusp value)))
    (reject-view-query "~a must be a non-negative integer, got ~s"
                       name value))
  value)

(defun normalize-view-update (value)
  (cond
    ((eq value t) "true")
    ((null value) "false")
    ((eq value :true) "true")
    ((eq value :false) "false")
    ((eq value :lazy) "lazy")
    ((and (stringp value) (string= value "true")) "true")
    ((and (stringp value) (string= value "false")) "false")
    ((and (stringp value) (string= value "lazy")) "lazy")
    (t
     (reject-view-query
      "update must be true, false, or lazy, got ~s" value))))

(defun validate-view-query-options
    (limit skip descending include-docs reduce group group-level
     key keys start-key end-key)
  (validate-nonnegative-integer "limit" limit)
  (validate-nonnegative-integer "skip" skip)
  (validate-view-boolean "descending" descending)
  (validate-view-boolean "include-docs" include-docs)
  (validate-view-boolean "reduce" reduce)
  (validate-view-boolean "group" group)
  (when group-level
    (validate-nonnegative-integer "group-level" group-level))
  (when (and reduce include-docs)
    (reject-view-query "include-docs cannot be used with reduced output"))
  (when (and (or group group-level) (not reduce))
    (reject-view-query "group and group-level require reduce=true"))
  (when (and key keys)
    (reject-view-query "key and keys are mutually exclusive"))
  (when (and (or key keys) (or start-key end-key))
    (reject-view-query "exact keys cannot be combined with key ranges"))
  t)

(defun couchdb-json-query-value (value)
  (jsown:to-json value))

(defun couchdb-view-query-parameters
    (limit skip descending include-docs reduce update
     key start-key end-key group group-level)
  (append
   `(("limit" . ,limit)
     ("skip" . ,skip)
     ("descending" . ,(if descending "true" "false"))
     ("include_docs" . ,(if include-docs "true" "false"))
     ("reduce" . ,(if reduce "true" "false"))
     ("update" . ,update))
   (when key
     `(("key" . ,(couchdb-json-query-value key))))
   (when start-key
     `(("startkey" . ,(couchdb-json-query-value start-key))))
   (when end-key
     `(("endkey" . ,(couchdb-json-query-value end-key))))
   (when group
     '(("group" . "true")))
   (when group-level
     `(("group_level" . ,group-level)))))

(defun couchdb-view-uri
    (client database design-document view-name parameters)
  (quri:render-uri
   (quri:merge-uris
    (quri:make-uri
     :path (format nil "/~a/_design/~a/_view/~a"
                   database design-document view-name)
     :query (quri:url-encode-params parameters))
    (cl-couch:couchdb-url client))))

(defun build-couchdb-view-request
    (client database design-document view-name
     &key
       (limit 50)
       (skip 0)
       (descending nil)
       (include-docs nil)
       (reduce nil)
       (update t)
       key
       keys
       start-key
       end-key
       (group nil)
       group-level)
  "Build one validated CouchDB view request without performing network I/O."
  (validate-view-query-options
   limit skip descending include-docs reduce group group-level
   key keys start-key end-key)
  (let* ((normalized-update (normalize-view-update update))
         (parameters
           (couchdb-view-query-parameters
            limit skip descending include-docs reduce normalized-update
            key start-key end-key group group-level))
         (uri
           (couchdb-view-uri
            client database design-document view-name parameters)))
    (if keys
        (make-couchdb-view-request
         :post uri (jsown:to-json (jsown:new-js ("keys" keys))))
        (make-couchdb-view-request :get uri nil))))

(defun perform-couchdb-view-request (client request)
  (dexador:request
   (couchdb-view-request-uri request)
   :method (couchdb-view-request-method request)
   :headers (cl-couch:couchdb-headers client)
   :content (couchdb-view-request-body request)
   :cookie-jar (cl-couch:couchdb-cookie client)
   :keep-alive t))

(defparameter *couchdb-view-transport* #'perform-couchdb-view-request)

(defun query-view
    (client database design-document view-name
     &rest arguments
     &key
       (limit 50)
       (skip 0)
       (descending nil)
       (include-docs nil)
       (reduce nil)
       (update t)
       key
       keys
       start-key
       end-key
       (group nil)
       group-level
     &allow-other-keys)
  "Execute a validated view request and return its decoded CouchDB response."
  (declare (ignore limit skip descending include-docs reduce update
                   key keys start-key end-key group group-level))
  (let* ((request
           (apply #'build-couchdb-view-request
                  client database design-document view-name arguments))
         (response (funcall *couchdb-view-transport* client request)))
    (jsown:parse response)))

(defun map-view-results
    (function client database design-document view-name
     &rest arguments
     &key (include-docs nil) &allow-other-keys)
  "Compatibility mapper for callers that consume key/value callback results."
  (validate-view-boolean "include-docs" include-docs)
  (let ((rows
          (jsown:val
           (apply #'query-view
                  client database design-document view-name arguments)
           "rows")))
    (mapcar
     (lambda (row)
       (let ((key (jsown:val row "key"))
             (value (jsown:val row "value")))
         (if include-docs
             (funcall function key value (jsown:val row "doc"))
             (funcall function key value))))
     rows)))
