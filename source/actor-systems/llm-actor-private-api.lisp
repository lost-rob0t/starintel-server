;;;; Keep llm.starintel.actor implementation private to starintel-gserver.

(in-package :star.actors)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (name '(llm-fine-tune-job
                  llm-fine-tune-job-id
                  llm-fine-tune-job-status
                  llm-job-json-object
                  llm-job-get
                  llm-synthesize-jsonl
                  llm-submit-fine-tune
                  llm-budget-seconds
                  *llm-actor*
                  llm-submit-synthesis
                  llm-synthesis-job-get
                  llm-synthesis-job-json-object
                  llm-validate-synthesis-request))
    (multiple-value-bind (symbol status)
        (find-symbol (symbol-name name) :star.actors)
      (when (and symbol (eq status :external))
        (unexport symbol :star.actors)))))
