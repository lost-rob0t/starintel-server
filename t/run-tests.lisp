(in-package :star-server-tests)

;;;; Test Runner - Main entry point for running all tests

(defun run-all-gserver-tests ()
  "Run all test suites (init-loader, consumers, and HTTP API)"
  (format t "~%~%========================================~%")
  (format t "   StarIntel Gserver Test Suite~%")
  (format t "========================================~%~%")

  ;; Run init-loader tests
  (format t "~%[1/3] Running Init Loader Tests...~%")
  (format t "----------------------------------------~%")
  (let ((init-passed (run! 'init-loader-tests)))
    (format t "~%Init Loader Tests: ~a~%"
            (if init-passed "PASSED" "FAILED"))

    ;; Run consumer tests
    (format t "~%[2/3] Running Consumer Thread Tests...~%")
    (format t "----------------------------------------~%")
    (let ((consumer-passed (run! 'consumer-tests)))
      (format t "~%Consumer Tests: ~a~%"
              (if consumer-passed "PASSED" "FAILED"))

      ;; Run HTTP API tests
      (format t "~%[3/3] Running HTTP API Tests...~%")
      (format t "----------------------------------------~%")
      (let ((http-passed (handler-case
                             (run-http-api-tests)
                           (error (e)
                             (format t "~%HTTP tests error: ~a~%" e)
                             nil))))

        ;; Summary
        (format t "~%~%========================================~%")
        (format t "   Test Summary~%")
        (format t "========================================~%")
        (format t "Init Loader Tests: ~a~%"
                (if init-passed "PASSED" "FAILED"))
        (format t "Consumer Tests: ~a~%"
                (if consumer-passed "PASSED" "FAILED"))
        (format t "HTTP API Tests: ~a~%"
                (if http-passed "PASSED" "FAILED"))
        (format t "~%~%")

        ;; Return overall status (all tests must pass)
        (and init-passed consumer-passed (not (null http-passed)))))))
