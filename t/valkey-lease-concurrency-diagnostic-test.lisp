(in-package :star-server-tests)

(in-suite valkey-lease-integration-tests)

(test one-hundred-concurrent-acquires-report-unexpected-outcomes
  "Diagnostic twin of the 100-way arbitration regression. Keep the hard
1-winner/99-conflict contract while printing the typed outcome histogram so a
transport ambiguity is not silently misclassified as a lease conflict."
  (with-real-valkey-store
      (store :label "concurrency-diagnostic"
       :pool-size 100
       :pool-wait-timeout-ms 10000
       :operation-timeout-ms 5000)
    (let ((identity (real-valkey-identity "concurrent-diagnostic-target"))
          (results nil)
          (results-lock (bt:make-lock "valkey-acquire-diagnostic-results")))
      (let ((threads
              (loop for index below 100
                    collect
                    (let ((thread-index index))
                      (bt:make-thread
                       (lambda ()
                         (let ((result
                                 (acquire-real-valkey-lease
                                  store identity
                                  (format nil "diagnostic-request-~d"
                                          thread-index)
                                  (format nil "diagnostic-owner-~d"
                                          thread-index)
                                  :deadline-ms 10000)))
                           (bt:with-lock-held (results-lock)
                             (push result results)))))))))
        (mapc #'bt:join-thread threads))
      (let* ((codes (mapcar #'star.leases:lease-outcome-code results))
             (unique-codes (remove-duplicates codes :test #'eq))
             (histogram
               (mapcar (lambda (code)
                         (cons code (count code codes :test #'eq)))
                       unique-codes)))
        (format t "~&100-way lease outcome histogram: ~s~%" histogram)
        (finish-output)
        (is (= 100 (length results)))
        (is (= 1 (count :acquired codes :test #'eq)))
        (is (= 99 (count :conflict codes :test #'eq)))
        (is (every (lambda (code)
                     (member code '(:acquired :conflict) :test #'eq))
                   codes))))))
