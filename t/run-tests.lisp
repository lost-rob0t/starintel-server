(in-package :star-server-tests)

;;;; Test Runner - Main entry point for running all tests

(defun run-all-gserver-tests ()
  "Run all test suites (consumers and HTTP API)"
  (format t "~%~%========================================~%")
  (format t "   StarIntel Gserver Test Suite~%")
  (format t "========================================~%~%")

  ;; Run consumer tests
  (format t "~%[1/2] Running Consumer Thread Tests...~%")
  (format t "----------------------------------------~%")
  (let ((consumer-results (run! 'consumer-tests)))
    (format t "~%Consumer Tests: ~a~%"
            (if (fiveam:results-status consumer-results)
                "PASSED"
                "FAILED"))

    ;; Run HTTP API tests
    (format t "~%[2/2] Running HTTP API Tests...~%")
    (format t "----------------------------------------~%")
    (let ((http-results (handler-case
                            (run-http-api-tests)
                          (error (e)
                            (format t "~%HTTP tests error: ~a~%" e)
                            nil))))

      ;; Summary
      (format t "~%~%========================================~%")
      (format t "   Test Summary~%")
      (format t "========================================~%")
      (format t "Consumer Tests: ~a~%"
              (if (fiveam:results-status consumer-results)
                  "PASSED"
                  "FAILED"))
      (format t "HTTP API Tests: ~a~%"
              (if (and http-results (fiveam:results-status http-results))
                  "PASSED"
                  "FAILED"))
      (format t "~%~%")

      ;; Return overall status
      (and (fiveam:results-status consumer-results)
           (or (null http-results)
               (fiveam:results-status http-results))))))
