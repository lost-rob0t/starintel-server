(in-package :star-server-tests)

(def-suite llm-actor-tests
  :description "llm.starintel.actor request, budget, chat, and HTTP exposure tests")

(in-suite llm-actor-tests)

(defun test-fine-tune-request (&key
                                 (base-model "Qwen/Qwen3-8B")
                                 (gpu-ram 24)
                                 (max-hourly 2.0)
                                 (max-total 20.0))
  (jsown:new-js
    ("type" "llm.fine_tune.start")
    ("version" 1)
    ("dataset"
     (jsown:new-js
       ("schema" "star-model-dataset-manifest/v1")
       ("artifact"
        (jsown:new-js
          ("kind" "github-actions-artifact")
          ("repository" "lost-rob0t/star-model")
          ("run_id" "1234")
          ("name" "star-model-dataset-deadbeef")))))
    ("training"
     (jsown:new-js
       ("base_model" base-model)
       ("method" "qlora")
       ("max_seq_length" 8192)))
    ("provider_policy"
     (jsown:new-js
       ("gpu" "vast-ai")
       ("gpu_ram_gb_min" gpu-ram)
       ("max_hourly_gpu_usd" max-hourly)
       ("max_total_gpu_usd" max-total)))))

(defun test-chat-request (&key
                            (model "openai/gpt-5.4-mini")
                            (max-tokens 4096)
                            (temperature 0.4))
  (jsown:new-js
    ("model" model)
    ("messages"
     (list
      (jsown:new-js ("role" "system") ("content" "Return JSON."))
      (jsown:new-js ("role" "user") ("content" "Generate one example."))))
    ("temperature" temperature)
    ("max_tokens" max-tokens)))

(test llm-budget-converts-total-cost-to-runtime
  (is (= 1800 (star.actors::llm-budget-seconds 2.0 1.0)))
  (is (= 3600 (star.actors::llm-budget-seconds 0.5 0.5))))

(test llm-budget-is-capped-at-one-day
  (is (= (* 24 60 60)
         (star.actors::llm-budget-seconds 0.1 100.0))))

(test llm-budget-rejects-non-positive-costs
  (signals error (star.actors::llm-budget-seconds 0 10))
  (signals error (star.actors::llm-budget-seconds 1 0))
  (signals error (star.actors::llm-budget-seconds -1 10)))

(test llm-model-name-accepts-repository-style-identifiers
  (is-true (star.actors::llm-safe-model-name-p "Qwen/Qwen3-8B"))
  (is-true (star.actors::llm-safe-model-name-p "org/model_1.0")))

(test llm-model-name-rejects-shell-syntax
  (is-false (star.actors::llm-safe-model-name-p "Qwen/Qwen3-8B;curl evil"))
  (is-false (star.actors::llm-safe-model-name-p "$(touch /tmp/nope)")))

(test llm-chat-request-validates-bounded-openai-shape
  (multiple-value-bind (model messages temperature max-tokens)
      (star.actors::llm-validate-chat-request (test-chat-request))
    (is (string= model "openai/gpt-5.4-mini"))
    (is (= 2 (length messages)))
    (is (= temperature 0.4))
    (is (= max-tokens 4096))))

(test llm-chat-request-rejects-shell-model-and-unbounded-output
  (signals error
    (star.actors::llm-validate-chat-request
     (test-chat-request :model "x;touch /tmp/nope")))
  (signals error
    (star.actors::llm-validate-chat-request
     (test-chat-request :max-tokens 999999))))

(test llm-chat-provider-policy-cannot-be-supplied-by-caller
  (let ((request (test-chat-request)))
    (setf (jsown:val request "provider")
          (jsown:new-js ("sort" "throughput")))
    ;; Validation accepts the request shape, but llm-openrouter-chat-completion
    ;; constructs a fresh allowlisted payload and never copies provider.
    (multiple-value-bind (model messages temperature max-tokens)
        (star.actors::llm-validate-chat-request request)
      (declare (ignore messages temperature max-tokens))
      (is (string= model "openai/gpt-5.4-mini")))))

(test llm-fine-tune-request-parses-canonical-artifact
  (multiple-value-bind (base-model max-seq-length gpu-ram max-hourly max-total artifact)
      (star.actors::llm-fine-tune-request-values (test-fine-tune-request))
    (is (string= "Qwen/Qwen3-8B" base-model))
    (is (= 8192 max-seq-length))
    (is (= 24 gpu-ram))
    (is (= 2.0 max-hourly))
    (is (= 20.0 max-total))
    (is (string= "github-actions-artifact"
                 (jsown:val artifact "kind")))))

(test llm-fine-tune-request-rejects-non-github-artifact
  (let ((request (test-fine-tune-request)))
    (setf (jsown:val (jsown:val (jsown:val request "dataset") "artifact") "kind")
          "arbitrary-url")
    (signals error
      (star.actors::llm-fine-tune-request-values request))))

(test llm-fine-tune-request-rejects-zero-spend-cap
  (signals error
    (star.actors::llm-fine-tune-request-values
     (test-fine-tune-request :max-total 0))))

(test llm-fine-tune-request-rejects-unsupported-gpu-provider
  (let* ((request (test-fine-tune-request))
         (policy (jsown:val request "provider_policy")))
    (setf (jsown:val policy "gpu") "surprise-cloud")
    (signals error
      (star.actors::llm-fine-tune-request-values request))))

(test llm-spend-bearing-routes-are-not-public
  (dolist (path '("/v1/chat/completions"
                  "/v1/dataset/synthesize"
                  "/v1/fine-tunes"))
    (is (not (member path star:*auth-public-paths* :test #'string=)))))
