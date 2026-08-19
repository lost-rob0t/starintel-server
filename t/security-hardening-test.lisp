(in-package :star-server-tests)

(in-suite auth-users-tests)

(test first-run-without-configured-password-skips-human-administrator
  (let* ((star:*auth-pepper* "unit-test-security-pepper")
         (star:*auth-password-iterations* 1000)
         (star:*auth-initial-username* "star")
         (star:*auth-initial-password* nil)
         (store (star.auth:make-memory-credential-store)))
    (is (null (star.auth:ensure-initial-user :store store)))
    (is (= 0 (star.auth:user-store-count store)))))

(in-suite http-auth-tests)

(test every-http-response-has-browser-hardening-headers
  (let ((headers star.frontends.http-api::*security-response-headers*))
    (is (string= "nosniff" (getf headers :x-content-type-options)))
    (is (string= "DENY" (getf headers :x-frame-options)))
    (is (search "frame-ancestors 'none'"
                (getf headers :content-security-policy)))
    (is (string= "no-referrer" (getf headers :referrer-policy)))
    (is (search "camera=()" (getf headers :permissions-policy)))))
