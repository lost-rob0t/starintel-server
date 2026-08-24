(in-package :star.api.client)

;;; Conditions

(define-condition star-client-error (error) ())

(define-condition client-configuration-error (star-client-error)
  ((message :initarg :message :reader client-error-message))
  (:report (lambda (condition stream)
             (format stream "~a" (client-error-message condition)))))

(define-condition client-request-error (star-client-error)
  ((message :initarg :message :reader client-error-message))
  (:report (lambda (condition stream)
             (format stream "~a" (client-error-message condition)))))

(define-condition client-transport-error (star-client-error)
  ((message :initarg :message :reader client-error-message)
   (cause :initarg :cause :initform nil :reader client-error-cause))
  (:report (lambda (condition stream)
             (format stream "~a" (client-error-message condition)))))

(define-condition client-timeout-error (client-transport-error) ())
(define-condition client-connection-error (client-transport-error) ())

(define-condition client-protocol-error (star-client-error)
  ((message :initarg :message :reader client-error-message)
   (operation-id :initarg :operation-id :initform nil
                 :reader client-protocol-error-operation-id))
  (:report (lambda (condition stream)
             (format stream "~a" (client-error-message condition)))))

(define-condition malformed-server-response (client-protocol-error) ())
(define-condition incompatible-server-response (client-protocol-error) ())

(define-condition client-http-error (star-client-error)
  ((status :initarg :status :reader client-http-error-status)
   (code :initarg :code :initform nil :reader client-http-error-code)
   (message :initarg :message :reader client-http-error-message)
   (correlation-id :initarg :correlation-id :initform nil
                   :reader client-http-error-correlation-id)
   (operation-id :initarg :operation-id :initform nil
                 :reader client-http-error-operation-id)
   (content-type :initarg :content-type :initform nil
                 :reader client-http-error-content-type)
   (headers :initarg :headers :initform nil :reader client-http-error-headers)
   (body :initarg :body :initform nil :reader client-http-error-body))
  (:report
   (lambda (condition stream)
     (format stream "StarIntel request failed with HTTP ~d~@[ (~a)~]: ~a~@[ [correlation ~a]~]"
             (client-http-error-status condition)
             (client-http-error-code condition)
             (client-http-error-message condition)
             (client-http-error-correlation-id condition)))))

(define-condition client-authentication-error (client-http-error) ())
(define-condition client-authorization-error (client-http-error) ())
(define-condition client-not-found-error (client-http-error) ())
(define-condition client-conflict-error (client-http-error) ())
(define-condition client-validation-error (client-http-error) ())
(define-condition client-rate-limit-error (client-http-error) ())
(define-condition client-server-unavailable-error (client-http-error) ())

;;; Request/response values

(defstruct client-request
  method
  uri
  headers
  body
  timeout-ms
  operation-id)

(defstruct client-response
  status
  headers
  body
  uri
  correlation-id
  content-type)

(defstruct request-options
  timeout-ms
  correlation-id
  idempotency-key
  headers)

;;; Authentication

(defclass authentication-provider () ())
(defclass anonymous-authentication (authentication-provider) ())
(defclass bearer-authentication (authentication-provider)
  ((api-key :initarg :api-key :reader bearer-api-key)))

(defmethod print-object ((provider bearer-authentication) stream)
  (declare (ignore provider))
  (print-unreadable-object (provider stream :type t :identity t)
    (write-string "REDACTED" stream)))

(defun merge-headers (&rest header-lists)
  (let ((result nil))
    (dolist (headers header-lists (nreverse result))
      (dolist (header headers)
        (let ((name (car header)))
          (setf result
                (delete name result :key #'car :test #'string-equal))
          (push (cons name (cdr header)) result))))))

(defgeneric authentication-headers (provider))

(defmethod authentication-headers ((provider anonymous-authentication))
  (declare (ignore provider))
  nil)

(defmethod authentication-headers ((provider bearer-authentication))
  (list (cons "Authorization"
              (format nil "Bearer ~a" (bearer-api-key provider)))))

;;; Client configuration

(defclass star-client ()
  ((base-url
    :initarg :base-url
    :reader base-url)
   (transport
    :initarg :transport
    :reader star-client-transport)
   (authentication
    :initarg :authentication
    :reader star-client-authentication)
   (default-timeout-ms
    :initarg :default-timeout-ms
    :reader star-client-default-timeout-ms)
   (user-agent
    :initarg :user-agent
    :reader star-client-user-agent)
   (headers
    :initarg :headers
    :reader star-client-headers)))

(defmethod print-object ((client star-client) stream)
  (print-unreadable-object (client stream :type t :identity t)
    (format stream "~a auth=~a"
            (base-url client)
            (type-of (star-client-authentication client)))))

(defclass client-transport () ())
(defclass dexador-transport (client-transport) ())
(defclass function-transport (client-transport)
  ((function :initarg :function :reader function-transport-function)))

(defun make-function-transport (function)
  (make-instance 'function-transport :function function))

(defun normalize-base-url (base-url)
  (unless (and (stringp base-url) (plusp (length base-url)))
    (error 'client-configuration-error
           :message "StarIntel client base URL must be a non-empty string"))
  (string-right-trim "/" base-url))

(defun make-star-client (&key
                           (base-url "http://127.0.0.1:5000")
                           (transport (make-instance 'dexador-transport))
                           (authentication (make-instance 'anonymous-authentication))
                           (default-timeout-ms 30000)
                           (user-agent "starintel-gserver-client/0.2")
                           (headers '(("Accept" . "application/json"))))
  (unless (typep transport 'client-transport)
    (error 'client-configuration-error
           :message "TRANSPORT must implement the StarIntel client transport protocol"))
  (unless (typep authentication 'authentication-provider)
    (error 'client-configuration-error
           :message "AUTHENTICATION must be a StarIntel authentication provider"))
  (make-instance 'star-client
                 :base-url (normalize-base-url base-url)
                 :transport transport
                 :authentication authentication
                 :default-timeout-ms default-timeout-ms
                 :user-agent user-agent
                 :headers (copy-tree headers)))

(defun client-with-api-key (client api-key)
  (unless (and (stringp api-key) (plusp (length api-key)))
    (error 'client-configuration-error
           :message "API key must be a non-empty string"))
  (make-star-client
   :base-url (base-url client)
   :transport (star-client-transport client)
   :authentication (make-instance 'bearer-authentication :api-key api-key)
   :default-timeout-ms (star-client-default-timeout-ms client)
   :user-agent (star-client-user-agent client)
   :headers (star-client-headers client)))

;;; Transport

(defun header-value (headers name)
  (cond
    ((hash-table-p headers)
     (or (gethash name headers)
         (gethash (string-downcase name) headers)
         (gethash (string-upcase name) headers)))
    ((and (listp headers) (consp (first headers)))
     (cdr (assoc name headers :test #'string-equal)))
    ((listp headers)
     (loop for (key value) on headers by #'cddr
           when (string-equal (string key) name)
             return value))
    (t nil)))

(defun response-content-type (headers)
  (header-value headers "content-type"))

(defun response-correlation-id (headers)
  (header-value headers "x-correlation-id"))

(defgeneric perform-client-request (transport request))

(defmethod perform-client-request ((transport function-transport) request)
  (funcall (function-transport-function transport) request))

(defun timeout-seconds (timeout-ms)
  (and timeout-ms
       (max 0.001d0 (/ timeout-ms 1000.0d0))))

(defmethod perform-client-request ((transport dexador-transport) request)
  (declare (ignore transport))
  (let ((arguments
          (list :method (client-request-method request)
                :headers (client-request-headers request)
                :content (client-request-body request)
                :force-string t
                :keep-alive t)))
    (when (client-request-timeout-ms request)
      (let ((seconds (timeout-seconds (client-request-timeout-ms request))))
        (setf arguments
              (append arguments
                      (list :connect-timeout seconds
                            :read-timeout seconds)))))
    (handler-case
        (multiple-value-bind (body status headers uri)
            (apply #'dexador:request (client-request-uri request) arguments)
          (make-client-response
           :status status
           :headers headers
           :body body
           :uri uri
           :correlation-id (response-correlation-id headers)
           :content-type (response-content-type headers)))
      (dex:http-request-failed (condition)
        (let ((headers (dex:response-headers condition)))
          (make-client-response
           :status (dex:response-status condition)
           :headers headers
           :body (dex:response-body condition)
           :uri (dex:request-uri condition)
           :correlation-id (response-correlation-id headers)
           :content-type (response-content-type headers))))
      (error (condition)
        (error 'client-connection-error
               :message (format nil "StarIntel transport failed: ~a" condition)
               :cause condition)))))

;;; Request construction

(defun make-url (client api-url &key query)
  (let ((uri (quri:merge-uris
              (quri:make-uri
               :path api-url
               :query (and query (quri:url-encode-params query)))
              (base-url client))))
    (quri:render-uri uri)))

(defun replace-all (string old new)
  (with-output-to-string (stream)
    (loop with start = 0
          for position = (search old string :start2 start :test #'char-equal)
          do (write-string string stream :start start :end position)
          if position
            do (write-string new stream)
               (setf start (+ position (length old)))
          else
            do (return))))

(defun path-parameter-value (path-parameters name)
  (let ((entry (assoc name path-parameters :test #'string-equal)))
    (unless entry
      (error 'client-request-error
             :message (format nil "Missing path parameter ~a" name)))
    (cdr entry)))

(defun expand-operation-path (operation path-parameters)
  (let ((path (star.http.contract:http-operation-path operation)))
    (dolist (name (star.http.contract:http-operation-path-parameters operation) path)
      (let ((value (path-parameter-value path-parameters name)))
        (setf path
              (replace-all path
                           (format nil ":~a" name)
                           (quri:url-encode (princ-to-string value))))))))

(defun request-body-string (body)
  (cond
    ((null body) nil)
    ((stringp body) body)
    (t (jsown:to-json body))))

(defun effective-request-headers (client body options)
  (merge-headers
   (star-client-headers client)
   (list (cons "User-Agent" (star-client-user-agent client)))
   (authentication-headers (star-client-authentication client))
   (when body (list (cons "Content-Type" "application/json")))
   (when (request-options-correlation-id options)
     (list (cons "X-Correlation-ID"
                 (request-options-correlation-id options))))
   (when (request-options-timeout-ms options)
     (list (cons "X-Request-Timeout-Ms"
                 (princ-to-string (request-options-timeout-ms options)))))
   (when (request-options-idempotency-key options)
     (list (cons "Idempotency-Key"
                 (request-options-idempotency-key options))))
   (request-options-headers options)))

(defun json-content-type-p (content-type)
  (and (stringp content-type)
       (let ((normalized (string-downcase content-type)))
         (or (search "application/json" normalized)
             (search "+json" normalized)))))

(defun probable-json-body-p (body)
  (and (stringp body)
       (let ((trimmed (string-left-trim '(#\Space #\Tab #\Newline #\Return) body)))
         (and (plusp (length trimmed))
              (member (char trimmed 0) '(#\{ #\[))))))

(defun decode-json-body (body operation-id)
  (handler-case
      (jsown:parse body)
    (error (condition)
      (error 'malformed-server-response
             :message (format nil "Operation ~a returned malformed JSON: ~a"
                              operation-id condition)
             :operation-id operation-id))))

(defun decoded-response-value (response operation-id)
  (let ((body (client-response-body response)))
    (cond
      ((or (null body)
           (and (stringp body) (zerop (length body))))
       nil)
      ((or (json-content-type-p (client-response-content-type response))
           (probable-json-body-p body))
       (decode-json-body body operation-id))
      (t body))))

(defun error-envelope-fields (body)
  (when (and (stringp body) (probable-json-body-p body))
    (let ((parsed (ignore-errors (jsown:parse body))))
      (when parsed
        (values (jsown:val-safe parsed "code")
                (or (jsown:val-safe parsed "detail")
                    (jsown:val-safe parsed "msg")
                    (jsown:val-safe parsed "message"))
                (jsown:val-safe parsed "correlation_id"))))))

(defun http-error-class (status)
  (cond
    ((= status 401) 'client-authentication-error)
    ((= status 403) 'client-authorization-error)
    ((= status 404) 'client-not-found-error)
    ((= status 409) 'client-conflict-error)
    ((member status '(400 413 415 422)) 'client-validation-error)
    ((= status 429) 'client-rate-limit-error)
    ((member status '(502 503 504)) 'client-server-unavailable-error)
    (t 'client-http-error)))

(defun signal-response-error (response operation-id)
  (multiple-value-bind (code message body-correlation-id)
      (error-envelope-fields (client-response-body response))
    (error (http-error-class (client-response-status response))
           :status (client-response-status response)
           :code code
           :message (or message
                        (format nil "HTTP ~d"
                                (client-response-status response)))
           :correlation-id (or (client-response-correlation-id response)
                               body-correlation-id)
           :operation-id operation-id
           :content-type (client-response-content-type response)
           :headers (client-response-headers response)
           :body (client-response-body response))))

(defun successful-status-p (status)
  (<= 200 status 299))

(defun ensure-request-options (options client)
  (let ((result (or options (make-request-options))))
    (unless (request-options-timeout-ms result)
      (setf (request-options-timeout-ms result)
            (star-client-default-timeout-ms client)))
    result))

(defun call-operation (client operation-id
                       &key path-parameters query-parameters headers body
                         request-options)
  (let* ((operation (star.http.contract:find-http-operation operation-id))
         (options (ensure-request-options request-options client)))
    (when headers
      (setf (request-options-headers options)
            (merge-headers (request-options-headers options) headers)))
    (let* ((path (expand-operation-path operation path-parameters))
           (body-string (request-body-string body))
           (request
             (make-client-request
              :method (star.http.contract:http-operation-method operation)
              :uri (make-url client path :query query-parameters)
              :headers (effective-request-headers client body-string options)
              :body body-string
              :timeout-ms (request-options-timeout-ms options)
              :operation-id operation-id))
           (response
             (perform-client-request (star-client-transport client) request)))
      (unless (typep response 'client-response)
        (error 'client-protocol-error
               :message "Client transport returned a non-response value"
               :operation-id operation-id))
      (unless (successful-status-p (client-response-status response))
        (signal-response-error response operation-id))
      (let ((value (decoded-response-value response operation-id)))
        (when (and (null (client-response-correlation-id response))
                   (consp value)
                   (eq (first value) :obj))
          (setf (client-response-correlation-id response)
                (jsown:val-safe value "correlation_id")))
        (values value response)))))

(defun api-request (client path &key stream query content (method :get)
                                  force-binary (keep-alive t))
  "Compatibility raw request helper. New code should use named operations."
  (declare (ignore stream force-binary keep-alive))
  (let* ((options (ensure-request-options nil client))
         (body (request-body-string content))
         (request
           (make-client-request
            :method method
            :uri (make-url client path :query query)
            :headers (effective-request-headers client body options)
            :body body
            :timeout-ms (request-options-timeout-ms options)
            :operation-id (format nil "raw:~a" path)))
         (response (perform-client-request (star-client-transport client) request)))
    (unless (successful-status-p (client-response-status response))
      (signal-response-error response (client-request-operation-id request)))
    (client-response-body response)))

;;; Secret-bearing structured results

(defstruct login-result
  api-key
  credential
  user
  correlation-id)

(defmethod print-object ((result login-result) stream)
  (print-unreadable-object (result stream :type t :identity t)
    (format stream "api-key=REDACTED user=~a"
            (let ((user (login-result-user result)))
              (and user (jsown:val-safe user "username"))))))

(defstruct credential-secret-result
  api-key
  credential
  correlation-id)

(defmethod print-object ((result credential-secret-result) stream)
  (declare (ignore result))
  (print-unreadable-object (result stream :type t :identity t)
    (write-string "api-key=REDACTED" stream)))
