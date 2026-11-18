(in-package :star-server-tests)

(def-suite biz-auth-tests)
(in-suite biz-auth-tests)

(test biz-auth-usernames-are-validated-and-canonicalized
  (is (string= "alice" (star.biz-auth:normalize-biz-username "Alice")))
  (is (string= "a-1_b.c" (star.biz-auth:normalize-biz-username "a-1_b.c")))
  (signals error (star.biz-auth:normalize-biz-username ""))
  (signals error (star.biz-auth:normalize-biz-username "bad name"))
  (signals error (star.biz-auth:normalize-biz-username "café"))
  (signals error (star.biz-auth:normalize-biz-username
                  (make-string 65 :initial-element #\a))))

(test biz-auth-generated-passwords-satisfy-the-identity-policy
  (let ((seen '()))
    (loop repeat 8
          for password = (star.biz-auth:generate-initial-password)
          do (push password seen)
             (is (<= 20 (length password)))
             (is (some #'upper-case-p password))
             (is (some #'lower-case-p password))
             (is (some #'digit-char-p password))
             (is (some (lambda (character)
                         (find character "!@#$%^&*()-_=+/"))
                       password)))
    (is (= 8 (length (remove-duplicates seen :test #'string=))))))

(test biz-auth-import-payload-uses-the-verified-internal-email-recipe
  (let* ((password (star.biz-auth:generate-initial-password))
         (payload (star.biz-auth:import-payload
                   "alice" "Alice" "Example" password))
         (email (jsown:val payload "email")))
    (is (string= "alice" (jsown:val payload "userName")))
    (is (string= "alice@starintel.actor" (jsown:val email "email")))
    (is-true (jsown:val email "isEmailVerified"))
    (is (string= password (jsown:val payload "password")))
    (is-true (jsown:val payload "passwordChangeRequired"))))

(test biz-auth-user-state-parsing
  (is (eq :active (star.biz-auth:user-state-keyword "USER_STATE_ACTIVE")))
  (is (eq :inactive (star.biz-auth:user-state-keyword "USER_STATE_INACTIVE")))
  (is (eq :initial (star.biz-auth:user-state-keyword "USER_STATE_INITIAL")))
  (is (eq :unknown (star.biz-auth:user-state-keyword "USER_STATE_SUSPENDED")))
  (is (eq :unknown (star.biz-auth:user-state-keyword "garbage"))))

(test biz-auth-fails-closed-without-a-service-token
  (let ((star.biz-auth::*token-override* :unset))
    (is-false (star.biz-auth:configured-p))))

(test biz-auth-admin-capability-is-in-the-closed-vocabulary
  (is-true (find "biz-auth:admin"
                 star.authorization::+capabilities+
                 :test #'string=)))

(test biz-auth-admin-capability-gates-on-scopes
  (flet ((principal (scopes)
           (star.auth::%make-request-principal
            :id "worker" :type "service" :scopes scopes)))
    (signals star.authorization:authorization-error
      (star.authorization:authorize!
       "biz-auth:admin"
       :principal (principal '("other:read"))
       :resource (star.authorization:make-authorization-resource)))
    (is-true
     (star.authorization:authorize!
      "biz-auth:admin"
      :principal (principal '("biz-auth:admin"))
      :resource (star.authorization:make-authorization-resource)))))
