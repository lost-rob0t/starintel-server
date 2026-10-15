;;;; OpenRouter cost/quantization policy for llm.starintel.actor

(in-package :star.actors)

(defparameter +llm-default-max-completion-tokens+ 8192)
(defparameter +llm-default-allowed-quantizations+ '("fp16" "bf16" "fp8"))

(defun llm-env-real (name)
  (let ((value (uiop:getenv name)))
    (when (and value (plusp (length value)))
      (let ((*read-eval* nil))
        (multiple-value-bind (number end)
            (read-from-string value nil nil)
          (unless (and (realp number) (= end (length value)) (> number 0))
            (error "Environment variable ~a must be a positive real number" name))
          number)))))

(defun llm-trim-string (value)
  (string-trim '(#\Space #\Tab #\Newline #\Return) value))

(defun llm-split-comma-list (value)
  (when (and value (plusp (length value)))
    (loop with start = 0
          for comma = (position #\, value :start start)
          for end = (or comma (length value))
          for item = (llm-trim-string (subseq value start end))
          when (plusp (length item)) collect item
          while comma
          do (setf start (1+ comma)))))

(defun llm-openrouter-allowed-quantizations ()
  (or (llm-split-comma-list (uiop:getenv "STAR_LLM_ALLOWED_QUANTIZATIONS"))
      +llm-default-allowed-quantizations+))

(defun llm-openrouter-provider-policy ()
  "Prefer the cheapest compatible provider and reject unapproved quantizations."
  (let* ((prompt-max (llm-env-real "STAR_LLM_MAX_PROMPT_USD_PER_M"))
         (completion-max (llm-env-real "STAR_LLM_MAX_COMPLETION_USD_PER_M"))
         (provider
           (jsown:new-js
             ("sort" "price")
             ("allow_fallbacks" t)
             ("require_parameters" t)
             ("quantizations" (llm-openrouter-allowed-quantizations)))))
    (when (or prompt-max completion-max)
      (let ((max-price (jsown:empty-object)))
        (when prompt-max
          (setf (jsown:val max-price "prompt") prompt-max))
        (when completion-max
          (setf (jsown:val max-price "completion") completion-max))
        (setf (jsown:val provider "max_price") max-price)))
    provider))

(defun llm-openrouter-request (task-family count)
  "Call the configured stronger teacher using lowest-price compatible routing."
  (let* ((model (llm-teacher-model))
         (max-completion-tokens
           (llm-env-integer "STAR_LLM_MAX_COMPLETION_TOKENS"
                            +llm-default-max-completion-tokens+))
         (payload
           (jsown:new-js
             ("model" model)
             ("temperature" 0.7)
             ("max_completion_tokens" max-completion-tokens)
             ("provider" (llm-openrouter-provider-policy))
             ("usage" (jsown:new-js ("include" t)))
             ("messages"
              (list
               (jsown:new-js
                 ("role" "system")
                 ("content" (llm-synthesis-system-prompt task-family count)))
               (jsown:new-js
                 ("role" "user")
                 ("content"
                  (format nil
                          "Generate the ~d requested ~a examples now."
                          count task-family)))))
             ("response_format" (jsown:new-js ("type" "json_object"))))))
         (body
           (dex:post
            +llm-openrouter-url+
            :headers `(("Authorization" . ,(format nil "Bearer ~a" (llm-openrouter-key)))
                       ("Content-Type" . "application/json")
                       ("X-Title" . "StarIntel llm.starintel.actor"))
            :content (jsown:to-json payload)))
         (response (jsown:parse body))
         (choice (llm-first (jsown:val response "choices")))
         (message (and choice (jsown:val-safe choice "message")))
         (content (and message (jsown:val-safe message "content"))))
    (unless (and content (stringp content))
      (error "Teacher response did not contain message content"))
    (values (jsown:parse content)
            (or (jsown:val-safe response "model") model))))
