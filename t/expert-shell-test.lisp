(in-package :star-server-tests)

(def-suite expert-shell-tests
  :description "Lisa-backed StarIntel operator expert shell")

(in-suite expert-shell-tests)

(defun expert-shell-fake-json-response (&optional (body "{\"status\":\"ok\"}"))
  (star.api.client::make-client-response
   :status 200
   :headers '(("content-type" . "application/json"))
   :body body
   :uri "http://example.test"
   :correlation-id "expert-shell-test"
   :content-type "application/json"))

(defun make-expert-shell-test-session (&optional request-hook)
  (let ((transport
          (star.api.client:make-function-transport
           (lambda (request)
             (when request-hook
               (funcall request-hook request))
             (expert-shell-fake-json-response)))))
    (star.expert.shell:make-shell-session
     :client (star.api.client:make-star-client
              :base-url "http://example.test"
              :transport transport))))

(test expert-shell-parses-text-and-lisp-commands-as-data
  (let ((request (star.expert.shell:parse-command
                  "doc search \"alice smith\" --limit 10")))
    (is (eq :search (star.expert.shell:shell-request-verb request)))
    (is (eq :document (star.expert.shell:shell-request-resource request)))
    (is (string= "alice smith"
                 (getf (star.expert.shell:shell-request-args request) :query)))
    (is (= 10 (getf (star.expert.shell:shell-request-args request) :limit))))
  (let ((request (star.expert.shell:parse-command
                  "(target create github \"{\\\"repo\\\":\\\"x/y\\\"}\" :transient :yes)")))
    (is (eq :create (star.expert.shell:shell-request-verb request)))
    (is (eq :target (star.expert.shell:shell-request-resource request)))
    (is-true (star.expert.shell:shell-request-confirmed-p request))
    (is-true (getf (star.expert.shell:shell-request-args request) :transient))))

(test expert-shell-read-operation-is-planned-and-executed-by-lisa
  (let ((requests '()))
    (let* ((session
             (make-expert-shell-test-session
              (lambda (request) (push request requests))))
           (result (star.expert.shell:run-command session "health")))
      (is-true (star.expert.shell:shell-result-success-p result))
      (is (eq :health (star.expert.shell:shell-result-operation result)))
      (is (= 1 (length requests)))
      (let ((plan (star.expert.shell:shell-session-last-plan session)))
        (is (eq :health (star.expert.shell:shell-plan-operation plan)))
        (is (eq :read (star.expert.shell:shell-plan-risk plan)))
        (is (eq 'star.expert.shell::plan-health
                (star.expert.shell:shell-plan-rule-name plan)))))))

(test expert-shell-unconfirmed-mutation-never-reaches-transport
  (let ((calls 0))
    (let* ((session
             (make-expert-shell-test-session
              (lambda (request)
                (declare (ignore request))
                (incf calls))))
           (result (star.expert.shell:run-command session "doc delete deadbeef")))
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
              (lambda (request)
                (incf calls)
                (setf captured request))))
           (result
             (star.expert.shell:run-command
              session
              "raw delete /document/deadbeef --yes")))
      (is-true (star.expert.shell:shell-result-success-p result))
      (is (= 1 calls))
      (is (eq :delete (star.api.client:client-request-method captured)))
      (is (search "/document/deadbeef"
                  (star.api.client:client-request-uri captured))))))

(test expert-shell-fallback-is-a-lisa-rule
  (let* ((session (make-expert-shell-test-session))
         (result (star.expert.shell:run-command session "frobnicate everything")))
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
