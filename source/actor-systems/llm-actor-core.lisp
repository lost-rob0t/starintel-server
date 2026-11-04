;;;; llm.starintel.actor -- core dataset and fine-tune orchestration helpers

(in-package :star.actors)

(defparameter +llm-openrouter-url+ "https://openrouter.ai/api/v1/chat/completions")
(defparameter +llm-synthesis-prompt-version+ "star-model-synth-v1")
(defparameter +llm-default-synth-batch-size+ 8)
(defparameter +llm-max-synth-examples+ 10000)
(defparameter +llm-default-gpu-disk-gb+ 100)
(defparameter +llm-default-provision-timeout-seconds+ 900)
(defparameter +llm-default-max-training-seconds+ (* 24 60 60))

(defvar *llm-actor* nil "Local Sento actor registered as llm.starintel.actor.")
(defvar *llm-fine-tune-jobs* (make-hash-table :test #'equal))
(defvar *llm-fine-tune-jobs-lock* (bt:make-lock "llm-fine-tune-jobs"))

(defstruct llm-fine-tune-job
  id
  status
  created-at
  updated-at
  request
  provider
  offer-id
  instance-id
  hourly-rate
  work-directory
  output-directory
  error)

(defun llm-env-required (name)
  (let ((value (uiop:getenv name)))
    (unless (and value (plusp (length value)))
      (error "Required environment variable ~a is not configured" name))
    value))

(defun llm-env-integer (name default)
  (let ((value (uiop:getenv name)))
    (if (and value (plusp (length value)))
        (parse-integer value)
        default)))

(defun llm-sequence-list (value)
  (cond
    ((null value) nil)
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t (list value))))

(defun llm-first (value)
  (car (llm-sequence-list value)))

(defun llm-sha256-string (text)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (babel:string-to-octets text :encoding :utf-8))))

(defun llm-stable-split (digest)
  (let ((n (/ (parse-integer (subseq digest 0 8) :radix 16)
              #xffffffff)))
    (cond
      ((< n 0.96) "train")
      ((< n 0.98) "validation")
      (t "test"))))

(defun llm-now ()
  (get-universal-time))

(defun llm-job-put (job)
  (bt:with-lock-held (*llm-fine-tune-jobs-lock*)
    (setf (gethash (llm-fine-tune-job-id job) *llm-fine-tune-jobs*) job))
  job)

(defun llm-job-get (job-id)
  (bt:with-lock-held (*llm-fine-tune-jobs-lock*)
    (gethash job-id *llm-fine-tune-jobs*)))

(defun llm-job-update (job &key status provider offer-id instance-id hourly-rate
                               work-directory output-directory error)
  (bt:with-lock-held (*llm-fine-tune-jobs-lock*)
    (when status
      (setf (llm-fine-tune-job-status job) status))
    (when provider
      (setf (llm-fine-tune-job-provider job) provider))
    (when offer-id
      (setf (llm-fine-tune-job-offer-id job) offer-id))
    (when instance-id
      (setf (llm-fine-tune-job-instance-id job) instance-id))
    (when hourly-rate
      (setf (llm-fine-tune-job-hourly-rate job) hourly-rate))
    (when work-directory
      (setf (llm-fine-tune-job-work-directory job) work-directory))
    (when output-directory
      (setf (llm-fine-tune-job-output-directory job) output-directory))
    (when error
      (setf (llm-fine-tune-job-error job) error))
    (setf (llm-fine-tune-job-updated-at job) (llm-now)))
  job)

(defun llm-job-json-object (job)
  (let ((json
          (jsown:new-js
            ("job_id" (llm-fine-tune-job-id job))
            ("status"
             (string-downcase
              (symbol-name (llm-fine-tune-job-status job))))
            ("created_at" (llm-fine-tune-job-created-at job))
            ("updated_at" (llm-fine-tune-job-updated-at job)))))
    (when (llm-fine-tune-job-provider job)
      (setf (jsown:val json "provider")
            (llm-fine-tune-job-provider job)))
    (when (llm-fine-tune-job-offer-id job)
      (setf (jsown:val json "offer_id")
            (llm-fine-tune-job-offer-id job)))
    (when (llm-fine-tune-job-instance-id job)
      (setf (jsown:val json "instance_id")
            (llm-fine-tune-job-instance-id job)))
    (when (llm-fine-tune-job-hourly-rate job)
      (setf (jsown:val json "hourly_rate_usd")
            (llm-fine-tune-job-hourly-rate job)))
    (when (llm-fine-tune-job-output-directory job)
      (setf (jsown:val json "output_directory")
            (llm-fine-tune-job-output-directory job)))
    (when (llm-fine-tune-job-error job)
      (setf (jsown:val json "error")
            (llm-fine-tune-job-error job)))
    json))

(defun llm-teacher-model ()
  (llm-env-required "STAR_LLM_TEACHER_MODEL"))

(defun llm-openrouter-key ()
  (or (uiop:getenv "OPENROUTER_API_KEY")
      (llm-env-required "STAR_LLM_OPENROUTER_API_KEY")))

(defun llm-synthesis-system-prompt (task-family count)
  (format nil
          "You are the teacher model generating supervised fine-tuning examples for StarIntel. Generate exactly ~d diverse examples for task family ~a. Return one JSON object with key examples, whose value is an array. Each item must contain messages (OpenAI-style role/content objects), optional tools for tool-calling examples, and metadata. Keep examples technically correct and internally consistent. For Prolog generate valid facts, rules, schemas, queries, reasoning, debugging, and migration tasks. For StarIntel JSON generate realistic JSON/JSON-LD documents, actor/dataset manifests, transforms, validation and repair tasks. Tool-calling examples must include realistic tool schemas, arguments, tool results, retries/errors where useful, and final synthesis; simulated results must be clearly consistent simulations. Ingest examples should cover parsing, provenance, checkpoints, idempotency, backpressure and recovery. Migration examples should include preconditions, forward/rollback logic and validation. OSINT-tool-design examples must be restricted to lawful public-source research, provenance, rate limiting, entity resolution and auditability; do not include credential theft, bypass techniques, exploitation, invasive tracking, or private-data acquisition. Do not include secrets or real private personal data. Output JSON only."
          count task-family))

(defun llm-synthetic-candidate-valid-p (candidate)
  (let ((messages
          (and (listp candidate)
               (jsown:val-safe candidate "messages"))))
    (and messages
         (>= (length (llm-sequence-list messages)) 2))))

(defun llm-decorate-synthetic-candidate (candidate task-family model)
  (unless (llm-synthetic-candidate-valid-p candidate)
    (error "Teacher candidate is missing a usable messages array"))
  (let* ((material
           (format nil "~a|~a|~a"
                   task-family model (jsown:to-json candidate)))
         (digest (llm-sha256-string material))
         (kind
           (if (string= task-family "tool-calling")
               "teacher-trace"
               "synthetic"))
         (row
           (jsown:new-js
             ("id" (format nil "star-~a" (subseq digest 0 24)))
             ("task" task-family)
             ("messages" (jsown:val candidate "messages"))
             ("provenance"
              (jsown:new-js
                ("kind" kind)
                ("generator" "llm.starintel.actor")
                ("model" model)
                ("prompt_version" +llm-synthesis-prompt-version+)))
             ("content_sha256" digest)
             ("split" (llm-stable-split digest))
             ("metadata"
              (or (jsown:val-safe candidate "metadata")
                  (jsown:empty-object)))))))
    (when (jsown:val-safe candidate "tools")
      (setf (jsown:val row "tools")
            (jsown:val candidate "tools")))
    row))

(defun llm-synthesize-jsonl (request)
  "Generate canonical star-model JSONL using the configured teacher model."
  (let* ((requested (or (jsown:val-safe request "max_examples") 0))
         (count (min +llm-max-synth-examples+ (max 0 requested)))
         (families
           (llm-sequence-list
            (jsown:val-safe request "task_families")))
         (batch-size
           (max 1
                (llm-env-integer
                 "STAR_LLM_SYNTH_BATCH_SIZE"
                 +llm-default-synth-batch-size+))))
    (unless (plusp count)
      (error "max_examples must be positive"))
    (unless families
      (error "task_families must be non-empty"))
    (with-output-to-string (out)
      (loop with emitted = 0
            with family-index = 0
            while (< emitted count)
            for family = (nth (mod family-index (length families)) families)
            for n = (min batch-size (- count emitted))
            do (multiple-value-bind (teacher-json model)
                   (llm-openrouter-request family n)
                 (let ((examples
                         (llm-sequence-list
                          (jsown:val-safe teacher-json "examples"))))
                   (unless (= (length examples) n)
                     (error "Teacher returned ~d examples; expected ~d"
                            (length examples) n))
                   (dolist (candidate examples)
                     (write-string
                      (jsown:to-json
                       (llm-decorate-synthetic-candidate
                        candidate family model))
                      out)
                     (terpri out)
                     (incf emitted))))
               (incf family-index)))))

(defun llm-run-program (argv)
  (uiop:run-program argv
                    :output :string
                    :error-output :string
                    :ignore-error-status nil))

(defun llm-safe-model-name-p (value)
  (and (stringp value)
       (plusp (length value))
       (every
        (lambda (ch)
          (or (alphanumericp ch)
              (find ch "-._/" :test #'char=)))
        value)))

(defun llm-github-headers ()
  `(("Authorization" . ,(format nil "Bearer ~a"
                                (llm-env-required
                                 "STAR_MODEL_GITHUB_TOKEN")))
    ("Accept" . "application/vnd.github+json")
    ("X-GitHub-Api-Version" . "2022-11-28")))

(defun llm-github-artifact-id (artifact)
  (let* ((repository (jsown:val artifact "repository"))
         (run-id (jsown:val artifact "run_id"))
         (name (jsown:val artifact "name"))
         (url
           (format nil
                   "https://api.github.com/repos/~a/actions/runs/~a/artifacts?name=~a&per_page=100"
                   repository run-id (quri:url-encode name)))
         (body (dex:get url :headers (llm-github-headers)))
         (json (jsown:parse body))
         (matches
           (llm-sequence-list
            (jsown:val-safe json "artifacts")))
         (match
           (find name matches
                 :test #'string=
                 :key (lambda (item)
                        (jsown:val-safe item "name")))))
    (or (and match (jsown:val-safe match "id"))
        (error "GitHub Actions artifact ~a was not found" name))))

(defun llm-write-curl-config (path)
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format out "header = \"Authorization: Bearer ~a\"~%"
            (llm-env-required "STAR_MODEL_GITHUB_TOKEN"))
    (format out "header = \"Accept: application/vnd.github+json\"~%")
    (format out "header = \"X-GitHub-Api-Version: 2022-11-28\"~%")))

(defun llm-download-github-artifact (artifact destination work-dir)
  (let* ((artifact-id (llm-github-artifact-id artifact))
         (repository (jsown:val artifact "repository"))
         (url
           (format nil
                   "https://api.github.com/repos/~a/actions/artifacts/~a/zip"
                   repository artifact-id))
         (config (merge-pathnames "curl.conf" work-dir)))
    (unwind-protect
         (progn
           (llm-write-curl-config config)
           (llm-run-program
            (list "curl" "--fail" "--location" "--retry" "4"
                  "--retry-all-errors"
                  "--config" (namestring config)
                  "--output" (namestring destination)
                  url))
           destination)
      (ignore-errors
        (delete-file config)))))

(defun llm-vast-search-offers (gpu-ram max-hourly)
  (llm-env-required "VAST_API_KEY")
  (let* ((query
           (format nil
                   "gpu_ram>=~a num_gpus=1 verified=true rentable=true direct_port_count>=1 dph_total<=~a"
                   gpu-ram max-hourly))
         (body
           (llm-run-program
            (list "vastai" "--raw" "search" "offers" query
                  "--limit" "20" "-o" "dph_total")))
         (offers (llm-sequence-list (jsown:parse body))))
    (sort offers #'<
          :key
          (lambda (offer)
            (or (jsown:val-safe offer "dph_total")
                most-positive-fixnum)))))

(defun llm-vast-offer-id (offer)
  (or (jsown:val-safe offer "id")
      (jsown:val-safe offer "ask_contract_id")
      (error "Vast offer has no id")))

(defun llm-vast-hourly-rate (offer)
  (or (jsown:val-safe offer "dph_total")
      (error "Vast offer has no dph_total")))

(defun llm-vast-create-instance (offer-id job-id)
  (let* ((body
           (llm-run-program
            (list "vastai" "--raw" "create" "instance"
                  (princ-to-string offer-id)
                  "--image" "vastai/pytorch:@vastai-automatic-tag"
                  "--disk"
                  (princ-to-string +llm-default-gpu-disk-gb+)
                  "--ssh" "--direct"
                  "--label" (format nil "star-model-~a" job-id)
                  "--cancel-unavail")))
         (json (jsown:parse body)))
    (or (jsown:val-safe json "new_contract")
        (jsown:val-safe json "id")
        (error "Vast did not return an instance id: ~a" body))))

(defun llm-vast-instance-json (instance-id)
  (jsown:parse
   (llm-run-program
    (list "vastai" "--raw" "show" "instance"
          (princ-to-string instance-id)))))

(defun llm-vast-copy-to-instance (local-path instance-id remote-path)
  (llm-run-program
   (list "vastai" "copy"
         (format nil "local:~a" (namestring local-path))
         (format nil "~a:~a" instance-id remote-path))))

(defun llm-vast-copy-from-instance (instance-id remote-path local-path)
  (ensure-directories-exist local-path)
  (llm-run-program
   (list "vastai" "copy"
         (format nil "~a:~a" instance-id remote-path)
         (format nil "local:~a" (namestring local-path)))))

(defun llm-vast-execute (instance-id command)
  (llm-run-program
   (list "vastai" "execute"
         (princ-to-string instance-id)
         command)))

(defun llm-vast-destroy (instance-id)
  (when instance-id
    (ignore-errors
      (llm-run-program
       (list "vastai" "destroy" "instance"
             (princ-to-string instance-id)
             "-y")))))

(defun llm-training-command (base-model max-seq-length budget-seconds)
  (format nil
          "set -euo pipefail; cd /workspace/star-model; python -m pip install --upgrade pip; python -m pip install -r trainer/requirements.txt; timeout --signal=TERM ~ds python trainer/train_qlora.py --base-model ~a --train train.jsonl --validation validation.jsonl --manifest manifest.json --output /workspace/output --max-seq-length ~d"
          budget-seconds base-model max-seq-length))

(defun llm-run-fine-tune-job (job)
  (let ((instance-id nil))
    (unwind-protect
         (handler-case
             (multiple-value-bind
                   (base-model max-seq-length gpu-ram
                    max-hourly max-total artifact)
                 (llm-fine-tune-request-values
                  (llm-fine-tune-job-request job))
               (let* ((job-id (llm-fine-tune-job-id job))
                      (root
                        (pathname
                         (or (uiop:getenv "STAR_LLM_JOB_ROOT")
                             "/var/tmp/starintel-llm/")))
                      (work-dir
                        (merge-pathnames
                         (format nil "~a/" job-id)
                         root))
                      (archive
                        (merge-pathnames "dataset.zip" work-dir))
                      (dataset-dir
                        (merge-pathnames "dataset/" work-dir))
                      (output-dir
                        (merge-pathnames "output/" work-dir)))
                 (ensure-directories-exist
                  (merge-pathnames "sentinel" work-dir))
                 (llm-job-update
                  job
                  :status :fetching-dataset
                  :work-directory (namestring work-dir))
                 (llm-download-github-artifact
                  artifact archive work-dir)
                 (ensure-directories-exist
                  (merge-pathnames "sentinel" dataset-dir))
                 (llm-run-program
                  (list "unzip" "-q"
                        (namestring archive)
                        "-d" (namestring dataset-dir)))
                 (llm-job-update
                  job :status :selecting-gpu :provider "vast-ai")
                 (let* ((offers
                          (llm-vast-search-offers
                           gpu-ram max-hourly))
                        (offer
                          (or (first offers)
                              (error
                               "No Vast offers matched the configured budget")))
                        (offer-id (llm-vast-offer-id offer))
                        (hourly (llm-vast-hourly-rate offer))
                        (budget-seconds
                          (llm-budget-seconds
                           hourly max-total)))
                   (when (> hourly max-hourly)
                     (error
                      "Selected Vast offer exceeds hourly budget"))
                   (llm-job-update
                    job
                    :status :provisioning
                    :offer-id offer-id
                    :hourly-rate hourly)
                   (setf instance-id
                         (llm-vast-create-instance
                          offer-id job-id))
                   (llm-job-update
                    job :instance-id instance-id)
                   (llm-vast-wait-running instance-id)
                   (llm-job-update job :status :staging)
                   (llm-vast-copy-to-instance
                    dataset-dir instance-id
                    "/workspace/star-model/")
                   (llm-job-update job :status :training)
                   (llm-vast-execute
                    instance-id
                    (llm-training-command
                     base-model
                     max-seq-length
                     budget-seconds))
                   (llm-job-update job :status :collecting)
                   (llm-vast-copy-from-instance
                    instance-id
                    "/workspace/output/"
                    output-dir)
                   (llm-job-update
                    job
                    :status :succeeded
                    :output-directory
                    (namestring output-dir)))))
           (error (condition)
             (log:error
              "llm fine-tune job ~a failed: ~a"
              (llm-fine-tune-job-id job)
              condition)
             (llm-job-update
              job
              :status :failed
              :error (princ-to-string condition))))
      (llm-vast-destroy instance-id))
    job))
