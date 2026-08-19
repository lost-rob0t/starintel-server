(in-package :star-server-tests)

(in-suite gserver-client-tests)

;; Redefine the fixture with an actual Common Lisp newline. In CL, "\n" is
;; the character n with an escape, not a C-style newline sequence.
(test admin-password-source-is-explicit
  (multiple-value-bind (password generated-p)
      (star::resolve-admin-password :random-password t)
    (is (= 32 (length password)))
    (is-true generated-p))
  (with-input-from-string (*standard-input* (format nil "stdin-password-123~%"))
    (multiple-value-bind (password generated-p)
        (star::resolve-admin-password :password-stdin t)
      (is (string= "stdin-password-123" password))
      (is-false generated-p)))
  (signals error
    (star::resolve-admin-password :password "one" :random-password t)))

(test openapi-secret-directions-match-wire-semantics
  (let* ((document (jsown:parse (star.http.contract:openapi-json)))
         (paths (jsown:val document "paths"))
         (login (jsown:val (jsown:val paths "/auth/login") "post"))
         (request-schema
           (jsown:val
            (jsown:val
             (jsown:val
              (jsown:val login "requestBody") "content")
             "application/json")
            "schema"))
         (response-schema
           (jsown:val
            (jsown:val
             (jsown:val
              (jsown:val
               (jsown:val login "responses") "200")
              "content")
             "application/json")
            "schema"))
         (password
           (jsown:val (jsown:val request-schema "properties") "password"))
         (api-key
           (jsown:val (jsown:val response-schema "properties") "api_key")))
    (is-true (jsown:val password "writeOnly"))
    (is (null (jsown:val-safe password "readOnly")))
    (is-true (jsown:val api-key "readOnly"))
    (is (null (jsown:val-safe api-key "writeOnly")))))
