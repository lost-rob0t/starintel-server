(in-package :star-server-tests)

(in-suite http-auth-tests)

(test unknown-credential-id-uses-the-verifier-comparison-path
  (let* ((star:*auth-pepper* "unit-test-pepper")
         (store (star.auth:make-memory-credential-store))
         (calls 0)
         (unknown-key
           (format nil "star_sk_v1_unknown_~a"
                   (make-string 64 :initial-element #\a))))
    (let ((star.auth:*verifier-compare-function*
            (lambda (left right)
              (incf calls)
              (star.auth:constant-time-octets= left right))))
      (is (string= "invalid_credential"
                   (captured-authentication-code
                    (lambda ()
                      (authenticate-test-key unknown-key store)))))
      (is (= 1 calls)))))

(test inactive-credential-still-uses-the-verifier-comparison-path
  (let* ((star:*auth-pepper* "unit-test-pepper")
         (store (star.auth:make-memory-credential-store))
         (calls 0))
    (multiple-value-bind (record raw-key)
        (star.auth:create-api-key
         "inactive-client" "api_client" '("documents:read")
         :store store)
      (star.auth:revoke-api-key
       (star.auth:api-key-record-id record)
       :store store)
      (let ((star.auth:*verifier-compare-function*
              (lambda (left right)
                (incf calls)
                (star.auth:constant-time-octets= left right))))
        (is (string= "invalid_credential"
                     (captured-authentication-code
                      (lambda ()
                        (authenticate-test-key raw-key store)))))
        (is (= 1 calls))))))
