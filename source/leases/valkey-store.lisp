(in-package :star.leases)

(define-condition valkey-pool-timeout (error) ())
(define-condition valkey-store-closed (error) ())
(define-condition valkey-server-error (error) ())
(define-condition valkey-command-failure (error)
  ((submitted-p :initarg :submitted-p
                :reader valkey-command-failure-submitted-p)))

(defstruct valkey-connection
  socket
  stream)

(defclass valkey-lease-store (lease-store)
  ((host :initarg :host :reader valkey-store-host)
   (port :initarg :port :reader valkey-store-port)
   (password :initarg :password :reader valkey-store-password)
   (tls-p :initarg :tls-p :reader valkey-store-tls-p)
   (tls-context :initarg :tls-context :reader valkey-store-tls-context)
   (pool-size :initarg :pool-size :reader valkey-store-pool-size)
   (pool-wait-timeout-ms
    :initarg :pool-wait-timeout-ms :reader valkey-store-pool-wait-timeout-ms)
   (operation-timeout-ms
    :initarg :operation-timeout-ms :reader valkey-store-operation-timeout-ms)
   (reconnect-attempts
    :initarg :reconnect-attempts :reader valkey-store-reconnect-attempts)
   (reconnect-backoff-ms
    :initarg :reconnect-backoff-ms :reader valkey-store-reconnect-backoff-ms)
   (idempotency-ttl-ms
    :initarg :idempotency-ttl-ms :reader valkey-store-idempotency-ttl-ms)
   (key-prefix :initarg :key-prefix :reader valkey-store-key-prefix)
   (audit-hook :initarg :audit-hook :initform nil :reader valkey-store-audit-hook)
   (metrics-hook
    :initarg :metrics-hook :initform nil :reader valkey-store-metrics-hook)
   (after-submit-hook
    :initarg :after-submit-hook :initform nil
    :reader valkey-store-after-submit-hook)
   (pool-lock :initform (bt:make-lock "valkey-lease-pool")
              :reader valkey-store-pool-lock)
   (pool-condition :initform (bt:make-condition-variable)
                   :reader valkey-store-pool-condition)
   (idle-connections :initform nil :accessor valkey-store-idle-connections)
   (all-connections :initform nil :accessor valkey-store-all-connections)
   (open-count :initform 0 :accessor valkey-store-open-count)
   (closed-p :initform nil :accessor valkey-store-closed-p)))

(defun valkey-unix-milliseconds ()
  (multiple-value-bind (seconds microseconds)
      (sb-ext:get-time-of-day)
    (+ (* seconds 1000) (floor microseconds 1000))))

(defun positive-integer-p (value)
  (and (integerp value) (plusp value)))

(defun non-empty-string-p (value)
  (and (stringp value) (plusp (length value))))

(defun read-password-file (path)
  (unless (and path (probe-file path))
    (error "Valkey password file does not exist"))
  (let ((password
          (string-right-trim
           '(#\Newline #\Return)
           (uiop:read-file-string path))))
    (unless (plusp (length password))
      (error "Valkey password file is empty"))
    password))

(defun validate-key-prefix (prefix)
  (unless (and (non-empty-string-p prefix)
               (<= (length prefix) 128)
               (notany (lambda (character)
                         (or (find character "{}*?[]" :test #'char=)
                             (find character
                                   '(#\Space #\Tab #\Newline #\Return)
                                   :test #'char=)))
                       prefix))
    (error "Valkey key prefix must be a bounded literal namespace"))
  prefix)

(defun make-valkey-lease-store
    (&key (host "127.0.0.1") (port 6379) password-file
       (tls-p nil) (tls-verify-p t) tls-ca-file
       (pool-size 8) (pool-wait-timeout-ms 500)
       (operation-timeout-ms 1000) (reconnect-attempts 2)
       (reconnect-backoff-ms 25) (idempotency-ttl-ms 86400000)
       (key-prefix "starintel:target-lease:v1") audit-hook metrics-hook
       after-submit-hook)
  (unless (and (non-empty-string-p host)
               (integerp port) (<= 1 port 65535)
               (positive-integer-p pool-size)
               (positive-integer-p pool-wait-timeout-ms)
               (positive-integer-p operation-timeout-ms)
               (integerp reconnect-attempts) (not (minusp reconnect-attempts))
               (integerp reconnect-backoff-ms)
               (not (minusp reconnect-backoff-ms))
               (positive-integer-p idempotency-ttl-ms))
    (error "Invalid bounded Valkey connection configuration"))
  (when (and tls-p (not tls-verify-p))
    (error "Valkey TLS certificate verification cannot be disabled"))
  (when (and tls-ca-file (not (probe-file tls-ca-file)))
    (error "Valkey TLS CA file does not exist"))
  (let ((context
          (when tls-p
            (cl+ssl:make-context
             :verify-location (or tls-ca-file :default)
             :verify-mode cl+ssl:+ssl-verify-peer+))))
    (make-instance
     'valkey-lease-store
     :host host
     :port port
     :password (read-password-file password-file)
     :tls-p tls-p
     :tls-context context
     :pool-size pool-size
     :pool-wait-timeout-ms pool-wait-timeout-ms
     :operation-timeout-ms operation-timeout-ms
     :reconnect-attempts reconnect-attempts
     :reconnect-backoff-ms reconnect-backoff-ms
     :idempotency-ttl-ms idempotency-ttl-ms
     :key-prefix (validate-key-prefix key-prefix)
     :audit-hook audit-hook
     :metrics-hook metrics-hook
     :after-submit-hook after-submit-hook)))

(defun close-valkey-connection (connection)
  (when connection
    (ignore-errors
      (when (valkey-connection-stream connection)
        (close (valkey-connection-stream connection))))
    (ignore-errors
      (when (valkey-connection-socket connection)
        (usocket:socket-close (valkey-connection-socket connection))))
    (setf (valkey-connection-stream connection) nil
          (valkey-connection-socket connection) nil))
  nil)

(defun valkey-connection-open-p (connection)
  (and connection
       (valkey-connection-stream connection)
       (open-stream-p (valkey-connection-stream connection))))

(defun write-resp-fragment (stream text)
  (write-sequence (babel:string-to-octets text :encoding :utf-8) stream))

(defun command-argument-string (argument)
  (etypecase argument
    (string argument)
    (integer (write-to-string argument))))

(defun write-valkey-command (stream arguments)
  (write-resp-fragment stream (format nil "*~d~c~c" (length arguments)
                                      #\Return #\Newline))
  (dolist (argument arguments)
    (let* ((text (command-argument-string argument))
           (octets (babel:string-to-octets text :encoding :utf-8)))
      (write-resp-fragment stream (format nil "$~d~c~c" (length octets)
                                          #\Return #\Newline))
      (write-sequence octets stream)
      (write-byte (char-code #\Return) stream)
      (write-byte (char-code #\Newline) stream))))

(defun read-resp-line (stream)
  (let ((octets
          (loop with result = (make-array 32 :element-type '(unsigned-byte 8)
                                          :adjustable t :fill-pointer 0)
                for byte = (read-byte stream)
                do (if (= byte (char-code #\Return))
                       (progn
                         (unless (= (read-byte stream) (char-code #\Newline))
                           (error 'valkey-server-error))
                         (return result))
                       (vector-push-extend byte result)))))
    (babel:octets-to-string octets :encoding :utf-8)))

(defun read-exact-octets (stream count)
  (let ((octets (make-array count :element-type '(unsigned-byte 8))))
    (read-sequence octets stream)
    (unless (and (= (read-byte stream) (char-code #\Return))
                 (= (read-byte stream) (char-code #\Newline)))
      (error 'valkey-server-error))
    octets))

(defun read-valkey-response (stream)
  (let ((type (code-char (read-byte stream))))
    (case type
      (#\+ (read-resp-line stream))
      (#\- (progn (read-resp-line stream) (error 'valkey-server-error)))
      (#\: (parse-integer (read-resp-line stream)))
      (#\$
       (let ((count (parse-integer (read-resp-line stream))))
         (if (minusp count)
             nil
             (babel:octets-to-string
              (read-exact-octets stream count) :encoding :utf-8))))
      (#\*
       (let ((count (parse-integer (read-resp-line stream))))
         (if (minusp count)
             nil
             (loop repeat count collect (read-valkey-response stream)))))
      (otherwise (error 'valkey-server-error)))))

(defun remaining-operation-seconds (store deadline)
  (/ (max 1
          (min (valkey-store-operation-timeout-ms store)
               (- deadline (valkey-unix-milliseconds))))
     1000.0))

(defun send-valkey-command
    (store connection deadline invoke-after-submit-hook-p arguments)
  (let ((submitted-p nil))
    (handler-case
        (sb-sys:with-deadline
            (:seconds (remaining-operation-seconds store deadline))
          (write-valkey-command (valkey-connection-stream connection) arguments)
          (force-output (valkey-connection-stream connection))
          (setf submitted-p t)
          (when (and invoke-after-submit-hook-p
                     (valkey-store-after-submit-hook store))
            (funcall (valkey-store-after-submit-hook store) connection))
          (unless (or (valkey-store-tls-p store)
                      (usocket:wait-for-input
                       (valkey-connection-socket connection)
                       :timeout (remaining-operation-seconds store deadline)
                       :ready-only t))
            (error 'valkey-command-failure :submitted-p submitted-p))
          (read-valkey-response (valkey-connection-stream connection)))
      (valkey-server-error (condition) (error condition))
      (valkey-command-failure (condition) (error condition))
      (sb-sys:deadline-timeout ()
        (close-valkey-connection connection)
        (error 'valkey-command-failure :submitted-p submitted-p))
      (error ()
        (close-valkey-connection connection)
        (error 'valkey-command-failure :submitted-p submitted-p)))))

(defun open-valkey-connection (store deadline)
  (let ((socket nil)
        (stream nil))
    (handler-case
        (progn
          (sb-sys:with-deadline
              (:seconds (remaining-operation-seconds store deadline))
            (setf socket
                  (usocket:socket-connect
                   (valkey-store-host store) (valkey-store-port store)
                   :element-type '(unsigned-byte 8)
                   :timeout (remaining-operation-seconds store deadline))
                  stream (usocket:socket-stream socket))
            (when (valkey-store-tls-p store)
              (setf stream
                    (cl+ssl:with-global-context
                        ((valkey-store-tls-context store))
                      (cl+ssl:make-ssl-client-stream
                       stream
                       :hostname (valkey-store-host store)
                       :verify :required))))
            (let ((connection
                    (make-valkey-connection :socket socket :stream stream)))
              (send-valkey-command
               store connection deadline nil
               (list "AUTH" (valkey-store-password store)))
              connection)))
      (serious-condition ()
        (when stream (ignore-errors (close stream)))
        (when socket (ignore-errors (usocket:socket-close socket)))
        (error 'valkey-command-failure :submitted-p nil)))))

(defun remove-pool-connection (store connection)
  (setf (valkey-store-idle-connections store)
        (delete connection (valkey-store-idle-connections store))
        (valkey-store-all-connections store)
        (delete connection (valkey-store-all-connections store)))
  (when (plusp (valkey-store-open-count store))
    (decf (valkey-store-open-count store))))

(defun call-with-valkey-connection (store deadline function)
  (let ((connection nil)
        (reserved-p nil))
    (bt:with-lock-held ((valkey-store-pool-lock store))
      (loop
        (when (valkey-store-closed-p store)
          (error 'valkey-store-closed))
        (when (valkey-store-idle-connections store)
          (setf connection (pop (valkey-store-idle-connections store)))
          (return))
        (when (< (valkey-store-open-count store)
                 (valkey-store-pool-size store))
          (incf (valkey-store-open-count store))
          (setf reserved-p t)
          (return))
        (let ((remaining
                (min (valkey-store-pool-wait-timeout-ms store)
                     (- deadline (valkey-unix-milliseconds)))))
          (when (<= remaining 0)
            (error 'valkey-pool-timeout))
          (unless (bt:condition-wait
                   (valkey-store-pool-condition store)
                   (valkey-store-pool-lock store)
                   :timeout (/ remaining 1000.0))
            (error 'valkey-pool-timeout)))))
    (when reserved-p
      (handler-case
          (progn
            (setf connection (open-valkey-connection store deadline))
            (bt:with-lock-held ((valkey-store-pool-lock store))
              (push connection (valkey-store-all-connections store))))
        (error (condition)
          (bt:with-lock-held ((valkey-store-pool-lock store))
            (decf (valkey-store-open-count store))
            (bt:condition-notify (valkey-store-pool-condition store)))
          (error condition))))
    (unwind-protect
         (funcall function connection)
      (bt:with-lock-held ((valkey-store-pool-lock store))
        (if (and (not (valkey-store-closed-p store))
                 (valkey-connection-open-p connection))
            (push connection (valkey-store-idle-connections store))
            (progn
              (close-valkey-connection connection)
              (remove-pool-connection store connection)))
        (bt:condition-notify (valkey-store-pool-condition store))))))

(defun valkey-pool-open-count (store)
  (bt:with-lock-held ((valkey-store-pool-lock store))
    (valkey-store-open-count store)))

(defun call-valkey-request (store deadline mutating-p arguments)
  (when (or (not (integerp deadline))
            (<= deadline (valkey-unix-milliseconds)))
    (return-from call-valkey-request (values nil :timeout)))
  (loop for attempt from 0 to (valkey-store-reconnect-attempts store)
        do (handler-case
               (return
                 (values
                  (call-with-valkey-connection
                   store deadline
                   (lambda (connection)
                     (send-valkey-command
                      store connection deadline mutating-p arguments)))
                  nil))
             (valkey-store-closed ()
               (return (values nil :closed)))
             (valkey-pool-timeout ()
               (return (values nil :timeout)))
             (valkey-server-error ()
               (return (values nil :backend-unavailable)))
             (valkey-command-failure (condition)
               (when (and mutating-p
                          (valkey-command-failure-submitted-p condition))
                 (return (values nil :outcome-unknown))))
             (sb-sys:deadline-timeout () nil)
             (error () nil))
           (when (or (= attempt (valkey-store-reconnect-attempts store))
                     (<= deadline (valkey-unix-milliseconds)))
             (return (values nil :backend-unavailable)))
           (sleep (/ (min (valkey-store-reconnect-backoff-ms store)
                          (max 0 (- deadline
                                    (valkey-unix-milliseconds))))
                     1000.0))))

(defun valkey-outcome (code &key lease leases)
  (make-lease-outcome
   :code code :lease lease :leases leases
   :retryable-p (retryable-lease-outcome-code-p code)
   :detail (case code
              (:timeout "Valkey operation deadline exceeded")
              (:backend-unavailable "Valkey backend unavailable")
              (:outcome-unknown "Valkey mutation outcome requires request-id retry")
              (otherwise nil))))

(defun emit-valkey-hooks (store operation request-id result)
  (let ((event (list :operation operation
                     :request-id request-id
                     :code (lease-outcome-code result)
                     :retryable-p (lease-outcome-retryable-p result))))
    (when (valkey-store-audit-hook store)
      (funcall (valkey-store-audit-hook store) event))
    (when (valkey-store-metrics-hook store)
      (funcall (valkey-store-metrics-hook store) event)))
  result)

(defun digest-string (value)
  (let ((octets
          (ironclad:digest-sequence
           :sha256
           (babel:string-to-octets value :encoding :utf-8))))
    (with-output-to-string (stream)
      (loop for octet across octets do (format stream "~2,'0x" octet)))))

(defun valkey-lock-digest (identity)
  (let* ((key (canonical-target-lock-key identity))
         (separator (position #\: key :from-end t)))
    (subseq key (1+ separator))))

(defun valkey-key (store identity suffix)
  (format nil "~a:{~a}:~a"
          (valkey-store-key-prefix store)
          (valkey-lock-digest identity)
          suffix))

(defun valkey-active-key (store identity)
  (valkey-key store identity "lease"))

(defun valkey-fence-key (store identity)
  (valkey-key store identity "fence"))

(defun valkey-fenced-value-key (store identity)
  (valkey-key store identity "commit"))

(defun valkey-idempotency-key (store identity operation owner request-id)
  (valkey-key
   store identity
   (format nil "idem:~a"
           (digest-string
            (format nil "~a~c~a~c~a" operation #\Null owner #\Null request-id)))))

(defun valkey-key-family (store identity)
  (list (valkey-active-key store identity)
        (valkey-fence-key store identity)
        (valkey-idempotency-key
         store identity "sample" "sample-owner" "sample-request")
        (valkey-fenced-value-key store identity)))

(defun behavior-digest (&rest values)
  (digest-string (jsown:to-json values)))

(defun valkey-code (name)
  (cdr (assoc name
               '(("acquired" . :acquired) ("renewed" . :renewed)
                 ("released" . :released) ("found" . :found)
                 ("revoked" . :revoked) ("not-found" . :not-found)
                 ("conflict" . :conflict) ("stale-token" . :stale-token)
                 ("not-owner" . :not-owner) ("expired" . :expired)
                 ("committed" . :committed)
                 ("idempotency-conflict" . :idempotency-conflict)
                 ("backend-unavailable" . :backend-unavailable))
               :test #'string=)))

(defun valkey-script-outcome (response)
  (let* ((code (and response (valkey-code (first response))))
         (encoded (and (second response) (second response)))
         (has-encoded (and (stringp encoded) (plusp (length encoded))))
         (record
           (when has-encoded
             (handler-case
                 (deserialize-lease-record encoded)
               (error () nil)))))
    (cond
      ((not code) (valkey-outcome :backend-unavailable))
      ((and has-encoded (not record)) (valkey-outcome :backend-unavailable))
      (t (valkey-outcome code :lease record)))))

(defun valkey-eval (store deadline script keys arguments)
  (call-valkey-request
   store deadline t
   (append (list "EVAL" script (length keys)) keys arguments)))

(defun valid-valkey-operation-p (deadline request-id strings)
  ;; Validate request shape only. A deadline that is an integer but already in
  ;; the past is an expired deadline, not a malformed request; it is handled by
  ;; call-valkey-request as :timeout so the outcome matches the memory backend
  ;; and the normative deadline-exceeded contract.
  (and (integerp deadline)
       (valid-lease-identifier-p request-id)
       (every #'valid-lease-identifier-p strings)))

(defun finish-valkey-operation (store operation request-id response failure)
  (emit-valkey-hooks
   store operation request-id
   (if failure
       (valkey-outcome failure)
       (valkey-script-outcome response))))

(defmethod acquire-lease
    ((store valkey-lease-store) identity
     &key owner-principal-id owner-client-id owner-credential-id
       service-instance-id ttl-ms maximum-lifetime-ms execution-id job-id
       trace-id metadata deadline request-id)
  (let ((strings
          (list owner-principal-id owner-client-id owner-credential-id
                service-instance-id execution-id job-id trace-id)))
    (unless (and (typep identity 'lease-identity)
                 (valid-valkey-operation-p deadline request-id strings)
                 (positive-integer-p ttl-ms)
                 (positive-integer-p maximum-lifetime-ms)
                 (<= ttl-ms maximum-lifetime-ms)
                 (> (valkey-store-idempotency-ttl-ms store)
                    maximum-lifetime-ms)
                 (valid-lease-metadata-p metadata))
      (return-from acquire-lease
        (emit-valkey-hooks store :acquire request-id
                           (valkey-outcome :invalid-request))))
    (let* ((identity-json (jsown:to-json (identity-to-json identity)))
           (metadata-json (jsown:to-json (or metadata (jsown:new-js))))
           (digest
             (behavior-digest
              owner-principal-id owner-client-id owner-credential-id
              service-instance-id ttl-ms maximum-lifetime-ms execution-id
              job-id trace-id metadata-json))
           (keys
             (list (valkey-active-key store identity)
                   (valkey-fence-key store identity)
                   (valkey-idempotency-key
                    store identity "acquire" owner-principal-id request-id))))
      (multiple-value-bind (response failure)
          (valkey-eval
           store deadline +valkey-acquire-script+ keys
           (list digest (valkey-store-idempotency-ttl-ms store)
                 (canonical-target-lock-key identity) identity-json
                 (cms-ulid:ulid) owner-principal-id owner-client-id
                 ttl-ms maximum-lifetime-ms owner-credential-id
                 service-instance-id execution-id job-id trace-id request-id
                 metadata-json))
        (finish-valkey-operation
         store :acquire request-id response failure)))))

(defmethod renew-lease
    ((store valkey-lease-store) identity
     &key lease-id owner-principal-id service-instance-id fencing-token ttl-ms
       deadline request-id)
  (unless (and (typep identity 'lease-identity)
               (valid-valkey-operation-p
                deadline request-id
                (list lease-id owner-principal-id service-instance-id))
               (positive-integer-p fencing-token)
               (positive-integer-p ttl-ms))
    (return-from renew-lease (valkey-outcome :invalid-request)))
  (let* ((digest
           (behavior-digest lease-id owner-principal-id service-instance-id
                            fencing-token ttl-ms))
         (keys
           (list (valkey-active-key store identity)
                 (valkey-idempotency-key
                  store identity "renew" owner-principal-id request-id))))
    (multiple-value-bind (response failure)
        (valkey-eval
         store deadline +valkey-renew-script+ keys
         (list digest (valkey-store-idempotency-ttl-ms store)
               (canonical-target-lock-key identity) lease-id owner-principal-id
               service-instance-id fencing-token ttl-ms request-id))
      (finish-valkey-operation store :renew request-id response failure))))

(defmethod release-lease
    ((store valkey-lease-store) identity
     &key lease-id owner-principal-id service-instance-id fencing-token
       deadline request-id)
  (unless (and (typep identity 'lease-identity)
               (valid-valkey-operation-p
                deadline request-id
                (list lease-id owner-principal-id service-instance-id))
               (positive-integer-p fencing-token))
    (return-from release-lease (valkey-outcome :invalid-request)))
  (let* ((digest
           (behavior-digest lease-id owner-principal-id service-instance-id
                            fencing-token))
         (keys
           (list (valkey-active-key store identity)
                 (valkey-idempotency-key
                  store identity "release" owner-principal-id request-id))))
    (multiple-value-bind (response failure)
        (valkey-eval
         store deadline +valkey-release-script+ keys
         (list digest (valkey-store-idempotency-ttl-ms store)
               (canonical-target-lock-key identity) lease-id owner-principal-id
               service-instance-id fencing-token request-id))
      (finish-valkey-operation store :release request-id response failure))))

(defmethod get-lease
    ((store valkey-lease-store) identity &key deadline request-id)
  (unless (and (typep identity 'lease-identity)
               (valid-valkey-operation-p deadline request-id nil))
    (return-from get-lease (valkey-outcome :invalid-request)))
  (multiple-value-bind (response failure)
      (call-valkey-request
       store deadline nil
       (list "EVAL" +valkey-get-script+ 1
             (valkey-active-key store identity)
             (canonical-target-lock-key identity)))
    (finish-valkey-operation store :get request-id response failure)))

(defun valkey-record-matches-p (record owner-principal-id target-id program-id)
  "Filters are already normalized (or nil for omitted). Compare directly."
  (and (or (null owner-principal-id)
           (string= owner-principal-id
                    (lease-record-owner-principal-id record)))
       (or (null target-id)
           (string= target-id
                    (lease-identity-target-id
                     (lease-record-identity record))))
       (or (null program-id)
           (string= program-id
                    (lease-identity-program-id
                     (lease-record-identity record))))))

(defun record-currently-active-p (record server-time)
  "True when SERVER-TIME (a TIME response: list of (seconds microseconds)
strings, or integer ms) is before the record's logical expires_at. Used by
list-leases to exclude logically expired surviving keys, consistent with
get-lease."
  (let* ((now-ms
           (cond
             ((and (consp server-time) (rest server-time))
              (+ (* (parse-integer (first server-time)) 1000)
                 (floor (parse-integer (second server-time)) 1000)))
             ((integerp server-time) server-time)
             (t 0))))
    (< now-ms (lease-record-expires-at record))))

(defun valkey-test-command (store deadline &rest arguments)
  (multiple-value-bind (response failure)
      (call-valkey-request store deadline nil arguments)
    (when failure (error "Valkey test command failed: ~a" failure))
    response))

(defmethod list-leases
    ((store valkey-lease-store)
     &key owner-principal-id target-id program-id deadline request-id)
  (unless (and (valid-valkey-operation-p deadline request-id nil)
               (valid-lease-filter-p owner-principal-id)
               (valid-lease-component-filter-p target-id)
               (valid-lease-component-filter-p program-id))
    (return-from list-leases (valkey-outcome :invalid-request)))
  (let ((normalized-owner owner-principal-id)
        (normalized-target
          (if target-id
              (normalize-identity-component "target-id" target-id)
              nil))
        (normalized-program
          (if program-id
              (normalize-identity-component "program-id" program-id)
              nil)))
    (handler-case
        (let ((cursor "0")
              (records nil)
              (pattern (format nil "~a:*:lease" (valkey-store-key-prefix store))))
          (loop
            (let ((page
                    (valkey-test-command
                     store deadline "SCAN" cursor "MATCH" pattern "COUNT" 100)))
              (setf cursor (first page))
              (dolist (key (second page))
                (let ((encoded (valkey-test-command store deadline "GET" key)))
                  (when (and (stringp encoded) (plusp (length encoded)))
                    ;; Corrupt-JSON defense: a malformed record is skipped,
                    ;; not signaled.
                    (let ((record
                            (handler-case
                                (deserialize-lease-record encoded)
                              (error () nil))))
                      (when record
                        ;; Corrupt-state guards, cluster-safe: each command
                        ;; targets one key. A no-TTL key (corrupt) or a
                        ;; logically expired surviving key (now >= expires_at)
                        ;; is excluded from the active list, consistent with
                        ;; get-lease.
                        (let ((ttl
                                (valkey-test-command
                                 store deadline "PTTL" key))
                              (server-time
                                (valkey-test-command
                                 store deadline "TIME")))
                          (when (and (integerp ttl)
                                     (/= ttl -1)
                                     (record-currently-active-p
                                      record server-time))
                            (when (valkey-record-matches-p
                                   record normalized-owner
                                   normalized-target normalized-program)
                              (push record records))))))))))
            (when (string= cursor "0") (return)))
          (emit-valkey-hooks
           store :list request-id
           (valkey-outcome
            :listed :leases (sort records #'string< :key #'lease-record-lock-key))))
      (error ()
        (emit-valkey-hooks store :list request-id
                           (valkey-outcome :backend-unavailable))))))

(defmethod revoke-lease
    ((store valkey-lease-store) identity
     &key lease-id fencing-token reason deadline request-id)
  (unless (and (typep identity 'lease-identity)
               (valid-valkey-operation-p deadline request-id (list lease-id))
               (positive-integer-p fencing-token)
               (valid-lease-reason-p reason))
    (return-from revoke-lease (valkey-outcome :invalid-request)))
  (let* ((digest (behavior-digest lease-id fencing-token reason))
         (keys
           (list (valkey-active-key store identity)
                 (valkey-idempotency-key
                  store identity "revoke" "administrator" request-id))))
    (multiple-value-bind (response failure)
        (valkey-eval
         store deadline +valkey-revoke-script+ keys
         (list digest (valkey-store-idempotency-ttl-ms store)
               (canonical-target-lock-key identity) lease-id fencing-token
               reason request-id))
      (finish-valkey-operation store :revoke request-id response failure))))

(defmethod backend-health
    ((store valkey-lease-store) &key deadline request-id)
  (unless (valid-valkey-operation-p deadline request-id nil)
    (return-from backend-health (valkey-outcome :invalid-request)))
  (multiple-value-bind (response failure)
      (call-valkey-request store deadline nil (list "PING"))
    (emit-valkey-hooks
     store :health request-id
     (cond
       (failure (valkey-outcome failure))
       ((string= response "PONG") (valkey-outcome :healthy))
       (t (valkey-outcome :backend-unavailable))))))

(defmethod close-lease-store
    ((store valkey-lease-store) &key deadline request-id)
  (unless (and (integerp deadline) (valid-lease-identifier-p request-id))
    (return-from close-lease-store (valkey-outcome :invalid-request)))
  (when (<= deadline (valkey-unix-milliseconds))
    (return-from close-lease-store (valkey-outcome :timeout)))
  (let ((connections nil)
        (context nil))
    (bt:with-lock-held ((valkey-store-pool-lock store))
      (unless (valkey-store-closed-p store)
        (setf (valkey-store-closed-p store) t
              connections (valkey-store-all-connections store)
              context (valkey-store-tls-context store)
              (valkey-store-all-connections store) nil
              (valkey-store-idle-connections store) nil
              (valkey-store-open-count store) 0)
        (bt:condition-notify (valkey-store-pool-condition store))))
    (mapc #'close-valkey-connection connections)
    (when context
      (cl+ssl:ssl-ctx-free context))
    (emit-valkey-hooks store :close request-id (valkey-outcome :closed))))

(defun valkey-fenced-set
    (store identity record key value &key deadline request-id)
  (unless (and (typep store 'valkey-lease-store)
               (typep identity 'lease-identity)
               (typep record 'lease-record)
               (string= key (valkey-fenced-value-key store identity))
               (non-empty-string-p value)
               (valid-valkey-operation-p deadline request-id nil))
    (return-from valkey-fenced-set :invalid-request))
  (multiple-value-bind (response failure)
      (valkey-eval
       store deadline +valkey-fenced-set-script+
       (list (valkey-active-key store identity) key)
       (list (lease-record-lease-id record)
             (lease-record-owner-principal-id record)
             (lease-record-service-instance-id record)
             (lease-record-fencing-token record)
             value
             (canonical-target-lock-key identity)))
    (if failure failure (or (valkey-code response) :backend-unavailable))))
