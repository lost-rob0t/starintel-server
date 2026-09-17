(in-package :star.storage)

(defclass s3-storage-backend (storage-backend)
  ((endpoint :initarg :endpoint :reader s3-backend-endpoint)
   (region :initarg :region :reader s3-backend-region)
   (bucket :initarg :bucket :reader s3-backend-bucket)
   (access-key-id :initarg :access-key-id :reader s3-backend-access-key-id)
   (secret-access-key :initarg :secret-access-key :reader s3-backend-secret-access-key)
   (session-token :initarg :session-token :initform nil
                  :reader s3-backend-session-token)
   (connect-timeout :initarg :connect-timeout :initform 5
                    :reader s3-backend-connect-timeout)
   (read-timeout :initarg :read-timeout :initform 30
                 :reader s3-backend-read-timeout))
  (:documentation "AWS Signature Version 4 S3-compatible object backend."))

(defun non-empty-string-p (value)
  (and (stringp value) (plusp (length value))))

(defun normalize-s3-endpoint (value)
  (let* ((trimmed (string-right-trim "/" value))
         (uri (ignore-errors (quri:uri trimmed))))
    (unless (and uri
                 (member (string-downcase (or (quri:uri-scheme uri) ""))
                         '("http" "https")
                         :test #'string=)
                 (quri:uri-host uri)
                 (null (quri:uri-query uri))
                 (null (quri:uri-fragment uri)))
      (error "STAR_S3_ENDPOINT must be an absolute HTTP(S) URL without query or fragment"))
    trimmed))

(defun make-s3-storage-backend-from-settings (&key (errorp t))
  "Build the configured S3 backend, or NIL when configuration is incomplete.

No secret is logged or copied into document metadata."
  (let ((bucket star::*s3-bucket*)
        (access-key-id star::*s3-access-key-id*)
        (secret-access-key star::*s3-secret-access-key*))
    (cond
      ((and (non-empty-string-p bucket)
            (non-empty-string-p access-key-id)
            (non-empty-string-p secret-access-key))
       (make-instance
        's3-storage-backend
        :name "s3"
        :endpoint (normalize-s3-endpoint star::*s3-endpoint*)
        :region star::*s3-region*
        :bucket bucket
        :access-key-id access-key-id
        :secret-access-key secret-access-key
        :session-token star::*s3-session-token*
        :connect-timeout star::*s3-connect-timeout-seconds*
        :read-timeout star::*s3-read-timeout-seconds*))
      (errorp
       (error 'storage-backend-error
              :backend "s3"
              :operation :configure
              :key ""
              :reason
              "STAR_S3_BUCKET and S3 access/secret credentials are required"))
      (t nil))))

(defun utf8-octets (value)
  (babel:string-to-octets value :encoding :utf-8))

(defun sha256-octets (octets)
  (ironclad:digest-sequence :sha256 octets))

(defun sha256-string-hex (value)
  (ironclad:byte-array-to-hex-string
   (sha256-octets (utf8-octets value))))

(defun hmac-sha256 (key data)
  (let ((hmac (ironclad:make-hmac key :sha256)))
    (ironclad:update-hmac hmac data)
    (ironclad:hmac-digest hmac)))

(defun hmac-sha256-string (key value)
  (hmac-sha256 key (utf8-octets value)))

(defun aws-signing-key (secret date region)
  (let* ((k-secret (utf8-octets (concatenate 'string "AWS4" secret)))
         (k-date (hmac-sha256-string k-secret date))
         (k-region (hmac-sha256-string k-date region))
         (k-service (hmac-sha256-string k-region "s3")))
    (hmac-sha256-string k-service "aws4_request")))

(defun aws-timestamp (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (values
     (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ"
             year month day hour minute second)
     (format nil "~4,'0d~2,'0d~2,'0d" year month day))))

(defun unreserved-uri-character-p (character)
  (or (alphanumericp character)
      (member character '(#\- #\_ #\. #\~))))

(defun aws-uri-encode-path (path)
  "AWS SigV4 path encoding for the ASCII storage keys emitted by this server."
  (with-output-to-string (stream)
    (loop for character across path
          do (cond
               ((or (char= character #\/)
                    (unreserved-uri-character-p character))
                (write-char character stream))
               (t
                (let ((code (char-code character)))
                  (if (< code 128)
                      (format stream "%~2,'0X" code)
                      (loop for byte across (utf8-octets (string character))
                            do (format stream "%~2,'0X" byte)))))))))

(defun endpoint-authority (uri)
  (let* ((scheme (string-downcase (quri:uri-scheme uri)))
         (host (quri:uri-host uri))
         (port (quri:uri-port uri))
         (default-port (if (string= scheme "https") 443 80)))
    (if (and port (/= port default-port))
        (format nil "~a:~d" host port)
        host)))

(defun s3-request-path (backend key)
  (let* ((uri (quri:uri (s3-backend-endpoint backend)))
         (prefix (string-right-trim "/" (or (quri:uri-path uri) ""))))
    (aws-uri-encode-path
     (format nil "~a/~a/~a"
             prefix
             (s3-backend-bucket backend)
             key))))

(defun s3-request-url (backend canonical-path)
  (let* ((uri (quri:uri (s3-backend-endpoint backend)))
         (scheme (quri:uri-scheme uri))
         (authority (endpoint-authority uri)))
    (format nil "~a://~a~a" scheme authority canonical-path)))

(defun s3-canonical-header-values (backend payload-hash amz-date)
  (let* ((uri (quri:uri (s3-backend-endpoint backend)))
         (host (endpoint-authority uri))
         (token (s3-backend-session-token backend))
         (headers
           (list (cons "host" host)
                 (cons "x-amz-content-sha256" payload-hash)
                 (cons "x-amz-date" amz-date))))
    (when (non-empty-string-p token)
      (setf headers
            (append headers (list (cons "x-amz-security-token" token)))))
    headers))

(defun canonical-headers-string (headers)
  (with-output-to-string (stream)
    (dolist (header headers)
      (format stream "~a:~a~%" (car header) (cdr header)))))

(defun signed-header-names (headers)
  (format nil "~{~a~^;~}" (mapcar #'car headers)))

(defun s3-authorization-header
    (backend method canonical-path payload-hash amz-date date-stamp headers)
  (let* ((signed-headers (signed-header-names headers))
         (canonical-request
           (format nil "~a~%~a~%~%~a~%~a~%~a"
                   (string-upcase (symbol-name method))
                   canonical-path
                   (canonical-headers-string headers)
                   signed-headers
                   payload-hash))
         (scope
           (format nil "~a/~a/s3/aws4_request"
                   date-stamp
                   (s3-backend-region backend)))
         (string-to-sign
           (format nil "AWS4-HMAC-SHA256~%~a~%~a~%~a"
                   amz-date
                   scope
                   (sha256-string-hex canonical-request)))
         (signature
           (ironclad:byte-array-to-hex-string
            (hmac-sha256-string
             (aws-signing-key
              (s3-backend-secret-access-key backend)
              date-stamp
              (s3-backend-region backend))
             string-to-sign))))
    (format nil
            "AWS4-HMAC-SHA256 Credential=~a/~a, SignedHeaders=~a, Signature=~a"
            (s3-backend-access-key-id backend)
            scope
            signed-headers
            signature)))

(defun s3-request-headers
    (backend method canonical-path payload-hash &key content-type)
  (multiple-value-bind (amz-date date-stamp)
      (aws-timestamp)
    (let* ((canonical
             (s3-canonical-header-values backend payload-hash amz-date))
           (authorization
             (s3-authorization-header
              backend method canonical-path payload-hash
              amz-date date-stamp canonical))
           (headers
             (loop for (name . value) in canonical
                   unless (string= name "host")
                     collect (cons name value))))
      (push (cons "Authorization" authorization) headers)
      (when content-type
        (push (cons "Content-Type" content-type) headers))
      headers)))

(defun s3-request (backend method key &key (content "") content-type)
  (let* ((body (or content ""))
         (payload-hash (sha256-string-hex body))
         (canonical-path (s3-request-path backend key))
         (url (s3-request-url backend canonical-path))
         (headers
           (s3-request-headers
            backend method canonical-path payload-hash
            :content-type content-type)))
    (handler-case
        (multiple-value-bind (response status response-headers)
            (dex:request
             url
             :method method
             :headers headers
             :content (unless (member method '(:get :head :delete)) body)
             :connect-timeout (s3-backend-connect-timeout backend)
             :read-timeout (s3-backend-read-timeout backend)
             :use-connection-pool t)
          (values response status response-headers))
      (error (condition)
        (error 'storage-backend-error
               :backend (storage-backend-name backend)
               :operation method
               :key key
               :reason (princ-to-string condition))))))

(defmethod storage-put ((backend s3-storage-backend) key content
                        &key (content-type "application/json") metadata)
  (declare (ignore metadata))
  (multiple-value-bind (body status headers)
      (s3-request backend :put key
                  :content content
                  :content-type content-type)
    (declare (ignore body))
    (list :key key :status status :headers headers)))

(defmethod storage-get ((backend s3-storage-backend) key)
  (multiple-value-bind (body status headers)
      (s3-request backend :get key)
    (declare (ignore status headers))
    body))

(defmethod storage-delete ((backend s3-storage-backend) key)
  (multiple-value-bind (body status headers)
      (s3-request backend :delete key)
    (declare (ignore body headers))
    (or (= status 200) (= status 204))))

(defmethod storage-head ((backend s3-storage-backend) key)
  (multiple-value-bind (body status headers)
      (s3-request backend :head key)
    (declare (ignore body))
    (list :key key :status status :headers headers)))
