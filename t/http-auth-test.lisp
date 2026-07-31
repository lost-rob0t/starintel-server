(in-package :star-server-tests)

(def-suite http-auth-tests
  :description "API-key authentication, lifecycle, redaction, and concurrency")

(in-suite http-auth-tests)

(defun captured-authentication-code (thunk)
  (handler-case
      (progn
        (funcall thunk)
        nil)
    (star.auth:authentication-error (condition)
      (star.auth:authentication-error-code condition))))

(defun captured-lifecycle-code (thunk)
  (handler-case
      (progn
        (funcall thunk)
        nil)
    (star.auth:credential-lifecycle-error (condition)
      (star.auth:credential-lifecycle-error-code condition))))

(defun altered-api-key (api-key)
  (let* ((last-index (1- (length api-key)))
         (last-character (char api-key last-index))
         (replacement (if (char= last-character #\0) #\1 #\0)))
    (concatenate 'string
                 (subseq api-key 0 last-index)
                 (string replacement))))

(defun authenticate-test-key (raw-key store &optional (correlation-id "corr-auth"))
  (star.auth:authenticate-authorization-header
   (format nil "Bearer ~a" raw-key)
   correlation-id
   (+ (star.auth:auth-now) 30)
   :store store))

(test missing-malformed-expired-disabled-revoked-and-incorrect-credentials-are-rejected
  (let* ((now 1000)
         (star.auth:*auth-clock* (lambda () now))
         (star:*auth-pepper* "unit-test-pepper")
         (store (star.auth:make-memory-credential-store)))
    (multiple-value-bind (valid-record valid-key)
        (star.auth:create-api-key
         "valid-client" "api_client" '("documents:read") :store store)
      (declare (ignore valid-record))
      (dolist (header
               (list nil
                     ""
                     "Basic abc"
                     "Bearer broken"
                     "Bearer star_sk_v2_bad_bad"
                     (format nil "Bearer ~a" (altered-api-key valid-key))))
        (is (string= "invalid_credential"
                     (captured-authentication-code
                      (lambda ()
                        (star.auth:authenticate-authorization-header
                         header "corr-reject" 1030 :store store)))))))
    (multiple-value-bind (expired-record expired-key)
        (star.auth:create-api-key
         "expired-client" "api_client" '("documents:read")
         :expires-in-seconds 1
         :store store)
      (declare (ignore expired-record))
      (incf now 2)
      (is (string= "invalid_credential"
                   (captured-authentication-code
                    (lambda ()
                      (authenticate-test-key expired-key store))))))
    (multiple-value-bind (disabled-record disabled-key)
        (star.auth:create-api-key
         "disabled-client" "api_client" '("documents:read") :store store)
      (star.auth:disable-api-key
       (star.auth:api-key-record-id disabled-record)
       :store store)
      (is (string= "invalid_credential"
                   (captured-authentication-code
                    (lambda ()
                      (authenticate-test-key disabled-key store))))))
    (multiple-value-bind (revoked-record revoked-key)
        (star.auth:create-api-key
         "revoked-client" "api_client" '("documents:read") :store store)
      (star.auth:revoke-api-key
       (star.auth:api-key-record-id revoked-record)
       :store store)
      (is (string= "invalid_credential"
                   (captured-authentication-code
                    (lambda ()
                      (authenticate-test-key revoked-key store))))))))

(test valid-credential-creates-immutable-request-principal
  (let* ((star:*auth-pepper* "unit-test-pepper")
         (store (star.auth:make-memory-credential-store)))
    (multiple-value-bind (record raw-key)
        (star.auth:create-api-key
         "quasar-client"
         "api_client"
         '("documents:read" "search:read")
         :store store)
      (let* ((context (authenticate-test-key raw-key store "corr-valid"))
             (principal
               (star.auth:request-security-context-principal context)))
        (is (string= "quasar-client"
                     (star.auth:request-principal-id principal)))
        (is (string= "api_client"
                     (star.auth:request-principal-type principal)))
        (is (string= (star.auth:api-key-record-id record)
                     (star.auth:request-principal-credential-id principal)))
        (is (equal '("documents:read" "search:read")
                   (star.auth:request-principal-scopes principal)))
        (is (string= "corr-valid"
                     (star.auth:request-security-context-correlation-id
                      context)))))))

(test verifier-boundary-uses-constant-time-comparison
  (let* ((star:*auth-pepper* "unit-test-pepper")
         (store (star.auth:make-memory-credential-store))
         (calls 0))
    (multiple-value-bind (record raw-key)
        (star.auth:create-api-key
         "constant-time-client" "api_client" '("documents:read")
         :store store)
      (declare (ignore record))
      (let ((star.auth:*verifier-compare-function*
              (lambda (left right)
                (incf calls)
                (star.auth:constant-time-octets= left right))))
        (authenticate-test-key raw-key store)
        (is (= 1 calls))))))

(test rotation-honors-overlap-and-then-invalidates-old-secret
  (let* ((now 2000)
         (star.auth:*auth-clock* (lambda () now))
         (star:*auth-pepper* "unit-test-pepper")
         (store (star.auth:make-memory-credential-store)))
    (multiple-value-bind (original original-key)
        (star.auth:create-api-key
         "rotation-client" "service_instance" '("documents:write")
         :store store)
      (multiple-value-bind (replacement replacement-key)
          (star.auth:rotate-api-key
           (star.auth:api-key-record-id original)
           10
           :store store)
        (is-true (authenticate-test-key original-key store))
        (is-true (authenticate-test-key replacement-key store))
        (is (string= (star.auth:api-key-record-id original)
                     (star.auth:api-key-record-rotation-parent-id replacement)))
        (setf now 2010)
        (is (string= "invalid_credential"
                     (captured-authentication-code
                      (lambda ()
                        (authenticate-test-key original-key store)))))
        (is-true (authenticate-test-key replacement-key store))))))

(test bootstrap-is-one-time-and-does-not-store-raw-secret
  (let* ((star:*auth-pepper* "unit-test-pepper")
         (star:*auth-bootstrap-secret* "bootstrap-secret")
         (store (star.auth:make-memory-credential-store)))
    (is (string= "bootstrap_denied"
                 (captured-lifecycle-code
                  (lambda ()
                    (star.auth:bootstrap-api-key
                     "wrong-secret" "admin" :store store)))))
    (multiple-value-bind (record raw-key)
        (star.auth:bootstrap-api-key
         "bootstrap-secret" "admin" :store store)
      (let* ((metadata
               (jsown:to-json
                (star.auth:api-key-metadata-json record)))
             (stored
               (star.auth:credential-store-get
                store
                (star.auth:api-key-record-id record))))
        (is (search "star_sk_v1_" raw-key))
        (is (null (search raw-key metadata :test #'char=)))
        (is (null (search "verifier" metadata :test #'char-equal)))
        (is (null (search "salt" metadata :test #'char-equal)))
        (is (not (string= raw-key
                          (star.auth:api-key-record-verifier stored))))))
    (is (string= "bootstrap_complete"
                 (captured-lifecycle-code
                  (lambda ()
                    (star.auth:bootstrap-api-key
                     "bootstrap-secret" "other-admin" :store store)))))))

(test revocation-has-zero-cache-bound-under-concurrency
  (let* ((star:*auth-pepper* "unit-test-pepper")
         (store (star.auth:make-memory-credential-store))
         (gate-lock (bt:make-lock "auth-revocation-gate"))
         (start nil)
         (results (make-array 16 :initial-element nil)))
    (multiple-value-bind (record raw-key)
        (star.auth:create-api-key
         "concurrent-client" "api_client" '("documents:read")
         :store store)
      (let ((threads
              (loop for index below (length results)
                    collect
                    (let ((slot index))
                      (bt:make-thread
                       (lambda ()
                         (loop until
                           (bt:with-lock-held (gate-lock) start)
                           do (sleep 0.001))
                         (setf (aref results slot)
                               (if (captured-authentication-code
                                    (lambda ()
                                      (authenticate-test-key raw-key store)))
                                   :rejected
                                   :accepted))))))))
        (star.auth:revoke-api-key
         (star.auth:api-key-record-id record)
         :store store)
        (bt:with-lock-held (gate-lock)
          (setf start t))
        (dolist (thread threads)
          (bt:join-thread thread))
        (is (every (lambda (result) (eq result :rejected)) results))))))

(test cors-is-allowlisted-and-never-wildcard
  (let ((star:*http-cors-allowed-origins*
          '("https://quasar.example")))
    (is-true
     (star.frontends.http-api::configured-origin-allowed-p
      "https://quasar.example"))
    (is-false
     (star.frontends.http-api::configured-origin-allowed-p
      "https://attacker.example"))
    (let ((headers
            (star.frontends.http-api::cors-headers-for-origin
             "https://quasar.example")))
      (is (string= "https://quasar.example"
                   (getf headers :access-control-allow-origin)))
      (is (null (member "*" headers :test #'equal))))))

(test authenticated-service-context-propagates-without-secret
  (let* ((principal
           (star.auth::%make-request-principal
            :id "actor-service"
            :type "actor_component"
            :scopes '("targets:lease")
            :credential-id "key-public-id"))
         (context
           (star.auth::%make-request-security-context
            :principal principal
            :correlation-id "corr-service"
            :deadline 9999
            :authenticated-at 9000))
         (star.auth:*request-security-context* context)
         (service-context (star.auth:current-service-call-context))
         (properties
           (star.frontends.http-api::service-context-properties
            "target" service-context))
         (headers (cdr (assoc :headers properties))))
    (is (string= "corr-service"
                 (cdr (assoc :correlation-id properties))))
    (is (string= "actor-service"
                 (cdr (assoc "x-star-principal-id" headers
                             :test #'string=))))
    (is (string= "key-public-id"
                 (cdr (assoc "x-star-credential-id" headers
                             :test #'string=))))
    (is (string= "9999"
                 (cdr (assoc "x-star-deadline" headers
                             :test #'string=))))
    (is (null (search "star_sk_" (prin1-to-string properties))))))

(test authentication-configuration-fails-closed
  (let ((star:*auth-mode* "api-key")
        (star:*auth-pepper* nil))
    (signals error (star.auth:validate-auth-configuration)))
  (let ((star:*auth-mode* "disabled")
        (star:*auth-dev-bypass* t)
        (star:*http-api-address* "0.0.0.0"))
    (signals error (star.auth:validate-auth-configuration)))
  (let ((star:*auth-mode* "disabled")
        (star:*auth-dev-bypass* t)
        (star:*http-api-address* "127.0.0.1"))
    (is-true (star.auth:validate-auth-configuration))))
