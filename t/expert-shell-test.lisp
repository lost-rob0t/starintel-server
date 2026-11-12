(uiop:define-package :star.expert.shell.tests
  (:use :cl :fiveam)
  (:export #:run-expert-shell-tests))

(in-package :star.expert.shell.tests)

(def-suite expert-shell-tests
  :description "Lisa-backed StarIntel operator expert shell")

(in-suite expert-shell-tests)

(defun expert-shell-fake-json-response
    (&key
       (status 200)
       (body "{\"status\":\"ok\"}")
       (correlation-id "expert-shell-test")
       (content-type "application/json"))
  "Construct a CLIENT-RESPONSE for isolated expert-shell transport tests."
  (star.api.client::make-client-response
   :status status
   :headers (append
             (list (cons "content-type" content-type))
             (when correlation-id
               (list (cons "x-correlation-id" correlation-id))))
   :body body
   :uri "http://example.test"
   :correlation-id correlation-id
   :content-type content-type))

(defun make-expert-shell-test-session (&key request-hook responder)
  "Create an expert-shell session with a deterministic in-memory transport."
  (let ((transport
          (star.api.client:make-function-transport
           (lambda (request)
             (when request-hook
               (funcall request-hook request))
             (if responder
                 (funcall responder request)
                 (expert-shell-fake-json-response))))))
    (star.expert.shell:make-shell-session
     :client (star.api.client:make-star-client
              :base-url "http://example.test"
              :transport transport))))

(defun command-rejected-p (command)
  "Return true when COMMAND is rejected by the shell parser."
  (handler-case
      (progn
        (star.expert.shell:parse-command command)
        nil)
    (error () t)))

(test expert-shell-parses-text-and-lisp-commands-as-data
  (let ((request
          (star.expert.shell:parse-command
           "doc search \"alice smith\" --limit 10")))
    (is (eq :search (star.expert.shell:shell-request-verb request)))
    (is (eq :document (star.expert.shell:shell-request-resource request)))
    (is (string= "alice smith"
                 (getf (star.expert.shell:shell-request-args request) :query)))
    (is (= 10
           (getf (star.expert.shell:shell-request-args request) :limit))))
  (let ((request
          (star.expert.shell:parse-command
           "(target create github \"{\\\"repo\\\":\\\"x/y\\\"}\" :transient :yes)")))
    (is (eq :create (star.expert.shell:shell-request-verb request)))
    (is (eq :target (star.expert.shell:shell-request-resource request)))
    (is-true (star.expert.shell:shell-request-confirmed-p request))
    (is-true
     (getf (star.expert.shell:shell-request-args request) :transient))))

(test expert-shell-read-operation-is-planned-and-executed-by-lisa
  (let ((requests '()))
    (let* ((session
             (make-expert-shell-test-session
              :request-hook
              (lambda (request)
                (push request requests))))
           (result (star.expert.shell:run-command session "health")))
      (is-true (star.expert.shell:shell-result-success-p result))
      (is (eq :health (star.expert.shell:shell-result-operation result)))
      (is (= 1 (length requests)))
      (let ((plan (star.expert.shell:shell-session-last-plan session)))
        (is (eq :health (star.expert.shell:shell-plan-operation plan)))
        (is (eq :read (star.expert.shell:shell-plan-risk plan)))
        (is (eq 'star.expert.shell::plan-health
                (star.expert.shell:shell-plan-rule-name plan)))))))

(test expert-shell-whoami-uses-auth-context-plan
  (let ((captured nil))
    (let* ((session
             (make-expert-shell-test-session
              :request-hook (lambda (request) (setf captured request))
              :responder
              (lambda (request)
                (declare (ignore request))
                (expert-shell-fake-json-response
                 :body "{\"principal_id\":\"alice\"}"))))
           (result (star.expert.shell:run-command session "whoami")))
      (is-true (star.expert.shell:shell-result-success-p result))
      (is (eq :auth-context
              (star.expert.shell:shell-result-operation result)))
      (is (search "/auth/context"
                  (star.api.client:client-request-uri captured)))
      (is (eq :read
              (star.expert.shell:shell-plan-risk
               (star.expert.shell:shell-session-last-plan session)))))))

(test expert-shell-unconfirmed-mutation-never-reaches-transport
  (let ((calls 0))
    (let* ((session
             (make-expert-shell-test-session
              :request-hook
              (lambda (request)
                (declare (ignore request))
                (incf calls))))
           (result
             (star.expert.shell:run-command
              session "doc delete deadbeef")))
      (is-false (star.expert.shell:shell-result-success-p result))
      (is (eq :confirmation-required
              (star.expert.shell:shell-result-code result)))
      (is (zerop calls))
      (is (eq :destructive
              (star.expert.shell:shell-plan-risk
               (star.expert.shell:shell-session-last-plan session)))))))

(test expert-shell-confirmed-mutation-reaches-transport-once
  (let ((calls 0)
        (captured nil))
    (let* ((session
             (make-expert-shell-test-session
              :request-hook
              (lambda (request)
                (incf calls)
                (setf captured request))))
           (result
             (star.expert.shell:run-command
              session
              "raw delete /document/deadbeef --yes")))
      (is-true (star.expert.shell:shell-result-success-p result))
      (is (= 1 calls))
      (is (eq :delete
              (star.api.client:client-request-method captured)))
      (is (search "/document/deadbeef"
                  (star.api.client:client-request-uri captured))))))

(test expert-shell-parser-rejects-bad-options-strictly
  (is-true
   (command-rejected-p "doc search alice --limit 10wat"))
  (is-true
   (command-rejected-p "doc search alice --limit"))
  (is-true
   (command-rejected-p "doc search alice --bogus 10"))
  (is-true
   (command-rejected-p "groups --limit 0")))

(test expert-shell-parse-errors-clear-stale-why-state
  (let ((session (make-expert-shell-test-session)))
    (star.expert.shell:run-command session "health")
    (let ((bad
            (star.expert.shell:run-command
             session "doc search alice --limit 10wat")))
      (is-false (star.expert.shell:shell-result-success-p bad))
      (is (eq :invalid-command
              (star.expert.shell:shell-result-code bad))))
    (let* ((why (star.expert.shell:run-command session "why"))
           (value (star.expert.shell:shell-result-value why)))
      (is (null (getf value :operation)))
      (is (search "No Lisa plan"
                  (getf value :message)))
      (is (some (lambda (event)
                  (eq :parse-error (getf event :event)))
                (getf value :trace))))))

(test expert-shell-one-shot-argv-preserves-json-bytes
  (let* ((json "{\"name\":\"Alice Smith\",\"n\":1}")
         (command
           (star.expert.shell::command-from-argv
            (list "doc" "submit" "person" json "--yes")))
         (request (star.expert.shell:parse-command command)))
    (is-true (star.expert.shell:shell-request-confirmed-p request))
    (is (string= json
                 (getf (star.expert.shell:shell-request-args request)
                       :json)))))

(test expert-shell-invalid-json-is-rejected-before-transport
  (let ((calls 0))
    (let* ((session
             (make-expert-shell-test-session
              :request-hook
              (lambda (request)
                (declare (ignore request))
                (incf calls))))
           (result
             (star.expert.shell:run-command
              session "doc submit person '{broken' --yes")))
      (is-false (star.expert.shell:shell-result-success-p result))
      (is (eq :invalid-command
              (star.expert.shell:shell-result-code result)))
      (is (zerop calls)))))

(test expert-shell-raw-operation-is-contained-to-server-paths
  (let ((calls 0))
    (let* ((session
             (make-expert-shell-test-session
              :request-hook
              (lambda (request)
                (declare (ignore request))
                (incf calls))))
           (result
             (star.expert.shell:run-command
              session "raw get https://example.invalid/escape")))
      (is-false (star.expert.shell:shell-result-success-p result))
      (is (eq :invalid-command
              (star.expert.shell:shell-result-code result)))
      (is (zerop calls)))))

(test expert-shell-preserves-typed-client-errors
  (let* ((session
           (make-expert-shell-test-session
            :responder
            (lambda (request)
              (declare (ignore request))
              (expert-shell-fake-json-response
               :status 403
               :body "{\"status\":\"error\",\"msg\":\"Denied\",\"code\":\"missing_scope\",\"correlation_id\":\"corr-denied\"}"
               :correlation-id "corr-denied"))))
         (result (star.expert.shell:run-command session "whoami")))
    (is-false (star.expert.shell:shell-result-success-p result))
    (is (eq :authorization-error
            (star.expert.shell:shell-result-code result)))
    (is (= 403 (getf (star.expert.shell:shell-result-value result) :status)))
    (is (string= "missing_scope"
                 (getf (star.expert.shell:shell-result-value result)
                       :server-code)))
    (is (string= "corr-denied"
                 (getf (star.expert.shell:shell-result-value result)
                       :correlation-id)))))

(test expert-shell-rules-introspects-installed-rulebase
  (let* ((session (make-expert-shell-test-session))
         (result (star.expert.shell:run-command session "rules"))
         (rules (star.expert.shell:shell-result-value result)))
    (is-true (star.expert.shell:shell-result-success-p result))
    (is (member "plan-health" rules :test #'string=))
    (is (member "plan-auth-context" rules :test #'string=))
    (is (member "block-unconfirmed-write" rules :test #'string=))))

(test expert-shell-json-output-keeps-structured-error-details
  (let* ((result
           (star.expert.shell::make-result
            :success-p nil
            :operation :auth-context
            :code :authorization-error
            :value (list :status 403
                         :server-code "missing_scope"
                         :correlation-id "corr-denied")))
         (object
           (jsown:parse (star.expert.shell::result-json result)))
         (value (jsown:val object "value")))
    (is (eq :false (jsown:val object "ok")))
    (is (= 403 (jsown:val value "status")))
    (is (string= "missing_scope"
                 (jsown:val value "server-code")))))

(test expert-shell-fallback-is-a-lisa-rule
  (let* ((session (make-expert-shell-test-session))
         (result
           (star.expert.shell:run-command
            session "frobnicate everything")))
    (is-false (star.expert.shell:shell-result-success-p result))
    (is (eq :unsupported-command
            (star.expert.shell:shell-result-code result)))
    (is (some (lambda (event)
                (eq :unsupported (getf event :event)))
              (star.expert.shell:shell-session-trace session)))))

(test expert-shell-why-exposes-rule-and-risk
  (let ((session (make-expert-shell-test-session)))
    (star.expert.shell:run-command session "doc delete deadbeef")
    (let* ((result (star.expert.shell:run-command session "why"))
           (value (star.expert.shell:shell-result-value result)))
      (is-true (star.expert.shell:shell-result-success-p result))
      (is (eq :document-delete (getf value :operation)))
      (is (eq :destructive (getf value :risk))))))

(test expert-shell-reader-disables-read-time-evaluation
  (let ((*read-eval* t))
    (let ((result
            (handler-case
                (progn
                  (star.expert.shell:parse-command
                   "(doc search #.(error \"must-not-run\"))")
                  :parsed)
              (error () :rejected))))
      (is (eq :rejected result)))))

(defun run-expert-shell-tests ()
  "Run the standalone expert-shell suite and signal on failure."
  (unless (run! 'expert-shell-tests)
    (error "StarIntel expert-shell tests failed"))
  t)
