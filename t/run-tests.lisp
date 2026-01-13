(in-package :star-server-tests)

;;;; Test Runner - Main entry point for running all tests

(defun tests-passed-p (results)
  "Check if test results indicate all tests passed.
   Handles both T (FiveAM shorthand for all passed) and list of test-result objects."
  (cond
    ((eq results t) t)
    ((null results) nil)
    ((listp results)
     (every (lambda (r) (typep r 'fiveam::test-passed)) results))
    (t nil)))

(defun run-all-gserver-tests ()
  "Run all test suites (init-loader, consumers, and HTTP API)"
  (format t "~%~%========================================~%")
  (format t "   StarIntel Gserver Test Suite~%")
  (format t "========================================~%~%")

  ;; Run init-loader tests
  (format t "~%[1/3] Running Init Loader Tests...~%")
  (format t "----------------------------------------~%")
  (let* ((init-results (run! 'init-loader-tests))
         (init-passed (tests-passed-p init-results)))
    (format t "~%Init Loader Tests: ~a~%"
            (if init-passed "PASSED" "FAILED"))

    ;; Run consumer tests
    (format t "~%[2/3] Running Consumer Thread Tests...~%")
    (format t "----------------------------------------~%")
    (let* ((consumer-results (run! 'consumer-tests))
           (consumer-passed (tests-passed-p consumer-results)))
      (format t "~%Consumer Tests: ~a~%"
              (if consumer-passed "PASSED" "FAILED"))

      ;; Run HTTP API tests
      (format t "~%[3/3] Running HTTP API Tests...~%")
      (format t "----------------------------------------~%")
      (let ((http-passed (handler-case
                             (let ((http-results (run-http-api-tests)))
                               (tests-passed-p http-results))
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
        (let ((all-passed (and init-passed consumer-passed http-passed)))
          (unless all-passed
            (error "Some tests failed"))
          all-passed)))))
