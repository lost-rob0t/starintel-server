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
