(in-package :star-server-tests)

(def-suite auth-users-tests
  :description "Persistent human users, default bootstrap login, and password lifecycle")

(in-suite auth-users-tests)

(defun captured-user-authentication-code (thunk)
  (handler-case
      (progn
        (funcall thunk)
        nil)
    (star.auth:authentication-error (condition)
      (star.auth:authentication-error-code condition))))

(defun captured-user-lifecycle-code (thunk)
  (handler-case
      (progn
        (funcall thunk)
        nil)
    (star.auth:credential-lifecycle-error (condition)
      (star.auth:credential-lifecycle-error-code condition))))

(test first-run-creates-star-intel-administrator-once
  (let* ((star:*auth-pepper* "unit-test-user-pepper")
         (star:*auth-password-iterations* 1000)
         (star:*auth-password-min-length* 12)
         (star:*auth-initial-username* "star")
         (star:*auth-initial-password* "intel")
         (store (star.auth:make-memory-credential-store)))
    (let ((record (star.auth:ensure-initial-user :store store)))
      (is (string= "star" (star.auth:user-record-username record)))
      (is (string= "administrator"
                   (star.auth:user-record-principal-type record)))
      (is (equal '("admin") (star.auth:user-record-scopes record)))
      (is-true (star.auth:user-record-must-change-password record))
      (is-true
       (star.auth:authenticate-user-password
        "STAR" "intel" :store store))
      (is (string= "invalid_credential"
                   (captured-user-authentication-code
                    (lambda ()
                      (star.auth:authenticate-user-password
                       "star" "wrong" :store store))))))
    (star.auth:ensure-initial-user :store store)
    (is (= 1 (star.auth:user-store-count store)))
    (let ((metadata
            (jsown:to-json
             (first (star.auth:list-user-metadata :store store)))))
      (is (search "\"username\":\"star\"" metadata))
      (is (null (search "password_hash" metadata :test #'char-equal)))
      (is (null (search "intel" metadata :test #'char-equal))))))

(test normal-users-reject-short-passwords
  (let* ((star:*auth-pepper* "unit-test-user-pepper")
         (star:*auth-password-iterations* 1000)
         (star:*auth-password-min-length* 12)
         (store (star.auth:make-memory-credential-store)))
    (is (string= "password_too_short"
                 (captured-user-lifecycle-code
                  (lambda ()
                    (star.auth:create-user
                     "alice"
                     "short"
                     "user"
                     '("documents:read")
                     :store store)))))
    (is (= 0 (star.auth:user-store-count store)))))

(test password-login-mints-existing-api-key-credentials
  (let* ((star:*auth-pepper* "unit-test-user-pepper")
         (star:*auth-password-iterations* 1000)
         (star:*auth-password-min-length* 12)
         (star:*auth-login-session-seconds* 3600)
         (store (star.auth:make-memory-credential-store)))
    (star.auth:create-user
     "alice"
     "correct-horse-battery-staple"
     "user"
     '("documents:read" "search:read")
     :must-change-password nil
     :store store)
    (multiple-value-bind (user credential raw-key)
        (star.auth:login-user
         "alice" "correct-horse-battery-staple" :store store)
      (is (string= "alice" (star.auth:user-record-username user)))
      (is (string= "alice" (star.auth:api-key-record-owner credential)))
      (is (search "star_sk_v1_" raw-key))
      (let* ((context
               (star.auth:authenticate-api-key
                raw-key "corr-user-login" (+ (star.auth:auth-now) 30)
                :store store))
             (principal
               (star.auth:request-security-context-principal context)))
        (is (string= "alice" (star.auth:request-principal-id principal)))
        (is (equal '("documents:read" "search:read")
                   (star.auth:request-principal-scopes principal)))))))

(test password-change-rejects-old-password-and-clears-bootstrap-flag
  (let* ((star:*auth-pepper* "unit-test-user-pepper")
         (star:*auth-password-iterations* 1000)
         (star:*auth-password-min-length* 12)
         (store (star.auth:make-memory-credential-store)))
    (star.auth:create-user
     "alice"
     "initial-password-123"
     "user"
     '("documents:read")
     :must-change-password t
     :store store)
    (let ((updated
            (star.auth:change-user-password
             "alice"
             "initial-password-123"
             "replacement-password-456"
             :store store)))
      (is-false (star.auth:user-record-must-change-password updated)))
    (is (string= "invalid_credential"
                 (captured-user-authentication-code
                  (lambda ()
                    (star.auth:authenticate-user-password
                     "alice" "initial-password-123" :store store)))))
    (is-true
     (star.auth:authenticate-user-password
      "alice" "replacement-password-456" :store store))))

(test administrator-can-reset-an-existing-user-password
  (let* ((star:*auth-pepper* "unit-test-user-pepper")
         (star:*auth-password-iterations* 1000)
         (star:*auth-password-min-length* 12)
         (store (star.auth:make-memory-credential-store)))
    (star.auth:create-user
     "analyst"
     "analyst-password-123"
     "user"
     '("documents:read")
     :must-change-password nil
     :store store)
    (let ((updated
            (star.auth:admin-set-user-password
             "analyst"
             "reset-password-456"
             :must-change-password t
             :store store)))
      (is-true (star.auth:user-record-must-change-password updated)))
    (is-true
     (star.auth:authenticate-user-password
      "analyst" "reset-password-456" :store store))))
