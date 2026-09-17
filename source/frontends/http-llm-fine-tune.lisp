;;;; HTTP surface for llm.starintel.actor

(in-package :star.frontends.http-api)

(defparameter +llm-synthesis-timeout-seconds+ 3600)
(defparameter +llm-fine-tune-submit-timeout-seconds+ 30)
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
  (or star.actors:*llm-actor*
      (signal-http-input-error
       503
       "llm_actor_unavailable"
       "llm.starintel.actor is not available")))

(defun handle-llm-dataset-synthesize-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (require-llm-administrator)
    (let* ((request (require-json-object (parse-json-request)))
           (actor (require-llm-actor))
           (body
             (sento.actor:ask-s
              actor
              (list :op :synthesize :request request)
              :time-out +llm-synthesis-timeout-seconds+)))
      (set-response-content-type "application/x-ndjson; charset=utf-8")
      body)))

(defun handle-llm-fine-tune-create-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (require-llm-administrator)
    (let* ((request (require-json-object (parse-json-request)))
           (actor (require-llm-actor))
           (receipt
             (sento.actor:ask-s
              actor
              (list :op :fine-tune :request request)
              :time-out +llm-fine-tune-submit-timeout-seconds+)))
      (setf (lack.response:response-status *response*) 202)
      (jsown:to-json receipt))))

(defun handle-llm-fine-tune-status-route (params)
  (with-http-boundary ()
    (require-llm-administrator)
    (let* ((job-id (query-value params "job-id"))
           (actor (require-llm-actor))
           (job
             (and job-id
                  (sento.actor:ask-s
                   actor
                   (list :op :status :job-id job-id)
                   :time-out +llm-status-timeout-seconds+))))
      (unless job
        (signal-http-input-error
         404
         "fine_tune_job_not_found"
         "Fine-tune job was not found"))
      (jsown:to-json job))))

(setf (ningle:route *app* "/v1/dataset/synthesize" :method :post)
      #'handle-llm-dataset-synthesize-route)

(setf (ningle:route *app* "/v1/fine-tunes" :method :post)
      #'handle-llm-fine-tune-create-route)

(setf (ningle:route *app* "/v1/fine-tunes/:job-id" :method :get)
      #'handle-llm-fine-tune-status-route)
