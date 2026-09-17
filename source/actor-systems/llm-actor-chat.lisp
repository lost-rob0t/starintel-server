;;;; OpenAI-compatible teacher completion facade for llm.starintel.actor

(in-package :star.actors)

(defparameter +llm-chat-max-completion-tokens+ 32768)
(defparameter +llm-chat-max-messages+ 256)

(defun llm-chat-message-valid-p (message)
  (and (listp message)
       (let ((role (jsown:val-safe message "role"))
             (content (jsown:val-safe message "content")))
         (and (member role '("system" "user" "assistant" "tool")
                      :test #'string=)
              (or (stringp content)
                  (listp content)
                  (vectorp content)
                  (null content))))))

(defun llm-validate-chat-request (request)
  "Validate the bounded OpenAI-compatible teacher request before network I/O."
  (let* ((model (jsown:val-safe request "model"))
         (messages (llm-sequence-list (jsown:val-safe request "messages")))
         (temperature (or (jsown:val-safe request "temperature") 0.45))
         (max-tokens (or (jsown:val-safe request "max_tokens")
                         (jsown:val-safe request "max_completion_tokens")
                         +llm-default-max-completion-tokens+)))
    (unless (llm-safe-model-name-p model)
      (error "model contains unsupported characters"))
    (unless (and messages
                 (<= (length messages) +llm-chat-max-messages+)
                 (every #'llm-chat-message-valid-p messages))
      (error "messages must contain 1 through ~d valid OpenAI-style messages"
             +llm-chat-max-messages+))
    (unless (and (realp temperature)
                 (<= 0 temperature 2))
      (error "temperature must be between 0 and 2"))
    (unless (and (integerp max-tokens)
                 (plusp max-tokens)
                 (<= max-tokens +llm-chat-max-completion-tokens+))
      (error "max_tokens must be between 1 and ~d"
             +llm-chat-max-completion-tokens+))
    (values model messages temperature max-tokens)))

(defun llm-copy-chat-optional-field (source destination key)
  (let ((value (jsown:val-safe source key)))
    (when value
      (setf (jsown:val destination key) value))))

(defun llm-openrouter-chat-completion (request)
  "Run one bounded teacher completion with server-enforced price/quantization policy.

The caller cannot override provider routing. The returned JSON is OpenRouter's
OpenAI-compatible response object so existing training-data clients can consume
it without provider-specific code."
  (multiple-value-bind (model messages temperature max-tokens)
      (llm-validate-chat-request request)
    (let ((payload
            (jsown:new-js
              ("model" model)
              ("messages" messages)
              ("temperature" temperature)
              ("max_completion_tokens" max-tokens)
              ("provider" (llm-openrouter-provider-policy))
              ("usage" (jsown:new-js ("include" t))))))
      ;; Keep this a strict allowlist. In particular, never accept a caller's
      ;; provider object because the server owns cost and quantization policy.
      (dolist (key '("response_format" "tools" "tool_choice" "stop" "seed"
                     "top_p" "frequency_penalty" "presence_penalty"))
        (llm-copy-chat-optional-field request payload key))
      (let ((body
              (dex:post
               +llm-openrouter-url+
               :headers
               `(("Authorization" . ,(format nil "Bearer ~a"
                                             (llm-openrouter-key)))
                 ("Content-Type" . "application/json")
                 ("X-Title" . "StarIntel llm.starintel.actor"))
               :content (jsown:to-json payload))))
        (jsown:parse body)))))
