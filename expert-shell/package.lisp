(uiop:define-package :star.expert.shell
  (:use :cl)
  (:export
   #:shell-session
   #:shell-session-client
   #:shell-session-engine
   #:shell-session-last-result
   #:shell-session-last-plan
   #:shell-session-trace
   #:shell-request
   #:shell-request-verb
   #:shell-request-resource
   #:shell-request-qualifier
   #:shell-request-args
   #:shell-request-confirmed-p
   #:shell-request-raw
   #:shell-plan
   #:shell-plan-operation
   #:shell-plan-risk
   #:shell-plan-rule-name
   #:shell-plan-reason
   #:shell-result
   #:shell-result-success-p
   #:shell-result-operation
   #:shell-result-code
   #:shell-result-value
   #:shell-result-message
   #:make-shell-session
   #:parse-command
   #:run-command
   #:render-result
   #:start-repl
   #:main))

(in-package :star.expert.shell)
