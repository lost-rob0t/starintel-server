;;;; HTTP surface for llm.starintel.actor

(in-package :star.frontends.http-api)

(defparameter +llm-submit-timeout-seconds+ 30)
(defparameter +llm-status-timeout-seconds+ 10)

(defun require-llm-administrator ()
  "Require an administrator principal for spend-bearing LLM operations."
  (unless (star.auth:administrator-principal-p)
    (signal-http-input-error
     403
     "administrator_required"
     "Administrator privileges are required for LLM synthesis and fine-tuning")))

(defun set-response-content-type (content-type)
  "Replace the response Content-Type without disturbing other boundary headers."
  (let ((headers (copy-list (lack.response:response-headers *response*))))
    (remf headers :content-type)
    (setf (lack.response:response-headers *response*)
          (list* :content-type content-type headers))))

(defun require-llm-actor ()
  (or star.actors::*llm-actor*
      (signal-http-input-error
       503
       "llm_actor_unavailable"
       "llm.starintel.actor is not available")))

(defun llm-actor-ask (message &optional (timeout +llm-status-timeout-seconds+))
  (sento.actor:ask-s (require-llm-actor) message :time-out timeout))

(defun handle-llm-dataset-synthesize-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (require-llm-administrator)
    (let* ((request (require-json-object (parse-json-request)))
           (receipt
             (llm-actor-ask
              (list :op :synthesize-submit :request request)
              +llm-submit-timeout-seconds+)))
      (setf (lack.response:response-status *response*) 202)
      (jsown:to-json receipt))))

(defun handle-llm-dataset-synthesis-status-route (params)
  (with-http-boundary ()
    (require-llm-administrator)
    (let* ((job-id (query-value params "job-id"))
           (job
             (and job-id
                  (llm-actor-ask
                   (list :op :synthesize-status :job-id job-id)))))
      (unless job
        (signal-http-input-error
         404
         "synthesis_job_not_found"
         "Dataset synthesis job was not found"))
      (jsown:to-json job))))

(defun handle-llm-dataset-synthesis-result-route (params)
  (with-http-boundary ()
    (require-llm-administrator)
    (let* ((job-id (query-value params "job-id"))
           (result
             (and job-id
                  (llm-actor-ask
                   (list :op :synthesize-result :job-id job-id)))))
      (unless result
        (signal-http-input-error
         404
         "synthesis_job_not_found"
         "Dataset synthesis job was not found"))
      (let ((status (jsown:val result "status")))
        (cond
          ((string= status "succeeded")
           (set-response-content-type "application/x-ndjson; charset=utf-8")
           (jsown:val result "output"))
          ((string= status "failed")
           (signal-http-input-error
            502
            "synthesis_failed"
            (or (jsown:val-safe result "error")
                "Dataset synthesis failed")))
          (t
           (signal-http-input-error
            409
            "synthesis_not_ready"
            (format nil "Dataset synthesis is ~a" status))))))))

(defun handle-llm-fine-tune-create-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (require-llm-administrator)
    (let* ((request (require-json-object (parse-json-request)))
           (receipt
             (llm-actor-ask
              (list :op :fine-tune :request request)
              +llm-submit-timeout-seconds+)))
      (setf (lack.response:response-status *response*) 202)
      (jsown:to-json receipt))))

(defun handle-llm-fine-tune-status-route (params)
  (with-http-boundary ()
    (require-llm-administrator)
    (let* ((job-id (query-value params "job-id"))
           (job
             (and job-id
                  (llm-actor-ask
                   (list :op :status :job-id job-id)))))
      (unless job
        (signal-http-input-error
         404
         "fine_tune_job_not_found"
         "Fine-tune job was not found"))
      (jsown:to-json job))))

(setf (ningle:route *app* "/v1/dataset/synthesize" :method :post)
      #'handle-llm-dataset-synthesize-route)

(setf (ningle:route *app* "/v1/dataset/synthesize/:job-id" :method :get)
      #'handle-llm-dataset-synthesis-status-route)

(setf (ningle:route *app* "/v1/dataset/synthesize/:job-id/result" :method :get)
      #'handle-llm-dataset-synthesis-result-route)

(setf (ningle:route *app* "/v1/fine-tunes" :method :post)
      #'handle-llm-fine-tune-create-route)

(setf (ningle:route *app* "/v1/fine-tunes/:job-id" :method :get)
      #'handle-llm-fine-tune-status-route)
