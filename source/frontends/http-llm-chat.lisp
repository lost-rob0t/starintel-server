;;;; OpenAI-compatible authenticated teacher endpoint for llm.starintel.actor

(in-package :star.frontends.http-api)

(defun handle-llm-chat-completions-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (require-llm-administrator)
    (let ((request (require-json-object (parse-json-request))))
      ;; Teacher inference is intentionally outside the Sento receive handler:
      ;; the actor remains responsive while this HTTP worker waits on OpenRouter.
      (jsown:to-json
       (star.actors::llm-openrouter-chat-completion request)))))

(setf (ningle:route *app* "/v1/chat/completions" :method :post)
      #'handle-llm-chat-completions-route)
