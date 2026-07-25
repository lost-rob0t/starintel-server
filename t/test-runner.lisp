(in-package :star-server-tests)

(defstruct suite-summary
  name
  discovered
  executed
  passed
  failed
  skipped)

(defun suite-test-names (suite-name)
  (labels ((walk (object)
             (typecase object
               (fiveam::test-case
                (list (fiveam::name object)))
               (fiveam::test-suite
                (mapcan (lambda (name)
                          (walk (fiveam:get-test name)))
                        (remove-duplicates
                         (fiveam::%test-names
                          (fiveam::tests object)))))
               (t nil))))
    (let ((suite (fiveam:get-test suite-name)))
      (unless (typep suite 'fiveam::test-suite)
        (error "Required FiveAM suite ~s is not defined." suite-name))
      (remove-duplicates (walk suite)))))

(defun result-test-name (result)
  (let ((test-case (fiveam::test-case result)))
    (and test-case
         (fiveam::name test-case))))

(defun result-test-names (results result-type)
  (remove-duplicates
   (loop for result in results
         for name = (result-test-name result)
         when (and name
                   (typep result result-type))
           collect name)))

(defun executed-test-names (discovered)
  (loop for name in discovered
        for test = (fiveam:get-test name)
        unless (eq :unknown (fiveam::status test))
          collect name))

(defun summarize-suite (suite-name discovered results)
  (let* ((executed (executed-test-names discovered))
         (failed (result-test-names results 'fiveam::test-failure))
         (skipped (set-difference
                   (result-test-names results 'fiveam::test-skipped)
                   failed))
         (passed (set-difference executed
                                 (union failed skipped))))
    (make-suite-summary
     :name suite-name
     :discovered (length discovered)
     :executed (length executed)
     :passed (length passed)
     :failed (length failed)
     :skipped (length skipped))))

(defun print-suite-summary (summary)
  (format t
          "~&SUITE ~a discovered=~d executed=~d passed=~d failed=~d skipped=~d~%"
          (suite-summary-name summary)
          (suite-summary-discovered summary)
          (suite-summary-executed summary)
          (suite-summary-passed summary)
          (suite-summary-failed summary)
          (suite-summary-skipped summary)))

(defun validate-required-suite (summary)
  (when (zerop (suite-summary-discovered summary))
    (error "Required suite ~a discovered zero tests."
           (suite-summary-name summary)))
  (when (zerop (suite-summary-executed summary))
    (error "Required suite ~a executed zero tests."
           (suite-summary-name summary)))
  (when (< (suite-summary-executed summary)
           (suite-summary-discovered summary))
    (error "Required suite ~a discovered ~d tests but executed only ~d."
           (suite-summary-name summary)
           (suite-summary-discovered summary)
           (suite-summary-executed summary)))
  (when (plusp (suite-summary-failed summary))
    (error "Required suite ~a has ~d failed tests."
           (suite-summary-name summary)
           (suite-summary-failed summary)))
  (when (plusp (suite-summary-skipped summary))
    (error "Required suite ~a has ~d skipped tests."
           (suite-summary-name summary)
           (suite-summary-skipped summary)))
  summary)

(defun run-required-suite (suite-name &key setup teardown)
  (let ((discovered (suite-test-names suite-name))
        (summary nil))
    (when (zerop (length discovered))
      (error "Required suite ~a discovered zero tests." suite-name))
    (unwind-protect
         (progn
           (when setup
             (funcall setup))
           (setf summary
                 (summarize-suite suite-name
                                  discovered
                                  (fiveam:run suite-name)))
           (print-suite-summary summary)
           (validate-required-suite summary))
      (when teardown
        (funcall teardown)))
    summary))

(defun run-required-suites (suite-names)
  (mapcar #'run-required-suite suite-names))
