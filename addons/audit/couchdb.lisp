(defpackage #:star.audit
  (:use #:cl)
  (:export #:authorization-event #:make-couchdb-writer
           #:install-authorization-sink))
(in-package #:star.audit)

(defun safe-id (value &optional (fallback "redacted"))
  "Project identifiers only. Never serialize a raw request or arbitrary metadata."
  (if (and (stringp value)
           (cl-ppcre:scan "^[A-Za-z0-9][A-Za-z0-9._:/-]{0,159}\\z" value))
      value fallback))

(defun value (object key)
  (and (consp object) (jsown:val-safe object key)))

(defun decision-time (id)
  "Derive a stable timestamp from the existing ULID decision ID for replay."
  (unless (and (stringp id) (= 26 (length id)))
    (error "Invalid audit decision identity"))
  (let ((milliseconds 0)
        (alphabet "0123456789ABCDEFGHJKMNPQRSTVWXYZ"))
    (loop for character across (subseq id 0 10)
          for digit = (position (char-upcase character) alphabet)
          do (unless digit (error "Invalid audit decision identity"))
             (setf milliseconds (+ (* milliseconds 32) digit)))
    (unless (< milliseconds (expt 2 48))
      (error "Invalid audit decision timestamp"))
    (multiple-value-bind (seconds remainder) (floor milliseconds 1000)
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time (+ seconds 2208988800) 0)
        (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.~6,'0dZ"
                year month day hour minute second (* remainder 1000))))))

(defun authorization-event (raw allowed-p tenant)
  "Project the existing authorization audit hook to starintel.audit.v1."
  (let* ((id (value raw "decision_id"))
         (source "starintel-server")
         (tenant-id (safe-id tenant nil))
         (resource (value raw "resource")))
    (unless (and tenant-id (safe-id id nil))
      (error "Invalid audit identity"))
    (let* ((identity (format nil "~a~%~a~%~a" tenant-id source id))
           (digest (ironclad:byte-array-to-hex-string
                    (ironclad:digest-sequence
                     :sha256 (babel:string-to-octets identity :encoding :utf-8)))))
      (jsown:new-js
        ("_id" (concatenate 'string "audit-" digest))
        ("schema" "starintel.audit.v1")
        ("event_id" id)
        ("event_type" (if allowed-p "security.authorization.allowed" "security.authorization.denied"))
        ("tenant_id" tenant-id)
        ("source" source)
        ("action" (safe-id (value raw "action") "unknown"))
        ("occurred_at" (decision-time id))
        ("principal_id" (safe-id (value raw "principal_id") "unknown"))
        ("resource_id" (safe-id (value resource "resource_id") "none"))
        ("correlation_id" (safe-id (value raw "correlation_id") "none"))
        ("outcome" (if allowed-p "allow" "deny"))
        ("details" (jsown:new-js
                     ("status" (safe-id (value raw "reason") "unknown"))))))))

(defun json-equal (left right)
  (if (and (consp left) (eq :obj (car left)))
      (and (consp right) (eq :obj (car right))
           (= (length (cdr left)) (length (cdr right)))
           (every (lambda (pair)
                    (let ((other (assoc (car pair) (cdr right) :test #'string=)))
                      (and other (json-equal (cdr pair) (cdr other)))))
                  (cdr left)))
      (equalp left right)))

(defun bounded-request (url username password method &optional content)
  "Loopback/tunnel transport only. SBCL deadline bounds the complete request."
  #-sbcl (error "CouchDB audit writer currently requires SBCL timeout support")
  #+sbcl
  (sb-ext:with-timeout 5
    (let ((drakma:*header-stream* nil))
      (multiple-value-bind (stream status)
          (drakma:http-request url :method method :content content
                               :basic-authorization (list username password)
                               :content-type "application/json"
                               :redirect nil :connection-timeout 2
                               :want-stream t :force-binary t :close t)
        (unwind-protect
             (let* ((buffer (make-array 32769 :element-type '(unsigned-byte 8)))
                    (count (read-sequence buffer stream)))
               (when (> count 32768) (error "Audit response too large"))
               (values (babel:octets-to-string buffer :end count :encoding :utf-8) status))
          (when (streamp stream) (close stream)))))))

(defun make-couchdb-writer (&key database-url tenant username password)
  "Return an explicit append callback. Credentials must come from secret files.

No database creation, worker, request or credential resolution occurs on load.
The URL must use a local CouchDB port or an authenticated local tunnel."
  (unless (and (stringp database-url)
               (cl-ppcre:scan "^http://127\\.0\\.0\\.1:[0-9]{1,5}/[a-z][a-z0-9_$()+-]*\\z" database-url)
               (safe-id tenant nil) (stringp username) (plusp (length username))
               (not (find #\: username)) (stringp password) (plusp (length password)))
    (error "Invalid audit writer configuration"))
  (lambda (document)
    (unless (equal tenant (value document "tenant_id"))
      (error "Audit tenant mismatch"))
    (let* ((id (value document "_id"))
           (body (jsown:to-json document)))
      (unless (and (stringp id) (cl-ppcre:scan "^audit-[0-9a-f]{64}\\z" id)
                   (<= (length (babel:string-to-octets body :encoding :utf-8)) 16384))
        (error "Invalid audit document"))
      (let ((url (concatenate 'string database-url "/" id)))
        (handler-case
            (multiple-value-bind (reply status)
                (bounded-request url username password :put body)
              (declare (ignore reply))
              (cond
                ((= status 201) id)
                ((= status 409)
                 (multiple-value-bind (old get-status)
                     (bounded-request url username password :get)
                   (unless (= get-status 200) (error "Audit conflict lookup failed"))
                   (let ((parsed (jsown:parse old)))
                     (setf (cdr parsed) (remove "_rev" (cdr parsed) :key #'car :test #'string=))
                     (unless (json-equal document parsed) (error "Audit identity collision"))
                     id)))
                (t (error "Audit append not committed"))))
          (error () (error "Audit append failed; no durable acknowledgement")))))))

(defun install-authorization-sink (writers &key (on-failure (lambda () (warn "Audit append failed"))))
  "WRITERS maps exact tenant IDs to append callbacks. Return an uninstall closure.

No cross-tenant fallback. Tenant-less global operations use the explicit system
entry. Normal authorization results do not fail closed on audit outage: the
existing logger remains active and ON-FAILURE is called without secret details.
This is best-effort authorization export, not a transactional security journal."
  (let* ((package (find-package "STAR.AUTHORIZATION"))
         (symbol (and package (find-symbol "*AUTHORIZATION-AUDIT-SINK*" package))))
    (unless (and symbol (boundp symbol)) (error "Load StarIntel authorization first"))
    (dolist (entry writers)
      (unless (and (safe-id (car entry) nil) (functionp (cdr entry)))
        (error "Invalid audit tenant writer")))
    (let* ((previous (symbol-value symbol))
           (installed
             (lambda (raw allowed-p)
               (funcall previous raw allowed-p)
               (handler-case
                   (let* ((raw-tenant (value (value raw "resource") "tenant_id"))
                          (tenant (if (or (null raw-tenant) (eq raw-tenant :null))
                                      "system" (safe-id raw-tenant nil)))
                          (writer (and tenant (cdr (assoc tenant writers :test #'string=)))))
                     (unless tenant (error "Invalid audit tenant"))
                     (when writer (funcall writer (authorization-event raw allowed-p tenant))))
                 (error () (handler-case (funcall on-failure) (error () nil)))))))
      (setf (symbol-value symbol) installed)
      (lambda ()
        ;; Do not clobber a newer hook installed by another runtime component.
        (when (eq installed (symbol-value symbol))
          (setf (symbol-value symbol) previous))))))
