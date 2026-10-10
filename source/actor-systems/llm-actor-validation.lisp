;;;; Validation and non-blocking runtime guards for llm.starintel.actor

(in-package :star.actors)

(defparameter +llm-synthetic-task-families+
  '("starintel-prolog-synthetic"
    "starintel-json-synthetic"
    "tool-calling"
    "ingest-jobs"
    "migrations"
    "osint-tool-design"))

(defvar *llm-synthesis-jobs* (make-hash-table :test #'equal))
(defvar *llm-synthesis-jobs-lock* (bt:make-lock "llm-synthesis-jobs"))

(defstruct llm-synthesis-job
  id
  status
  created-at
  updated-at
  request
  output
  error)

(defun llm-positive-real-p (value)
  (and (realp value) (> value 0)))

(defun llm-positive-integer-p (value)
  (and (integerp value) (> value 0)))

(defun llm-required-json-string (object key)
  (let ((value (and object (jsown:val-safe object key))))
    (unless (and (stringp value) (plusp (length value)))
      (error "~a must be a non-empty string" key))
    value))

(defun llm-fine-tune-request-values (request)
  "Validate and extract a fine-tune request before any billable action."
  (let* ((training (jsown:val-safe request "training"))
         (policy (jsown:val-safe request "provider_policy"))
         (dataset (jsown:val-safe request "dataset"))
         (artifact (and dataset (jsown:val-safe dataset "artifact")))
         (base-model (and training (jsown:val-safe training "base_model")))
         (max-seq-length (or (and training
                                  (jsown:val-safe training "max_seq_length"))
                             8192))
         (gpu-provider (or (and policy (jsown:val-safe policy "gpu"))
                           "vast-ai"))
         (gpu-ram (or (and policy (jsown:val-safe policy "gpu_ram_gb_min")) 24))
         (max-hourly (or (and policy
                              (jsown:val-safe policy "max_hourly_gpu_usd"))
                         2.0))
         (max-total (or (and policy
                             (jsown:val-safe policy "max_total_gpu_usd"))
                        20.0)))
    (unless (llm-safe-model-name-p base-model)
      (error "training.base_model contains unsupported characters"))
    (unless (llm-positive-integer-p max-seq-length)
      (error "training.max_seq_length must be a positive integer"))
    (unless (string= gpu-provider "vast-ai")
      (error "Unsupported GPU provider ~s; this adapter supports vast-ai" gpu-provider))
    (unless (llm-positive-integer-p gpu-ram)
      (error "provider_policy.gpu_ram_gb_min must be a positive integer"))
    (unless (llm-positive-real-p max-hourly)
      (error "provider_policy.max_hourly_gpu_usd must be positive"))
    (unless (llm-positive-real-p max-total)
      (error "provider_policy.max_total_gpu_usd must be positive"))
    (unless artifact
      (error "dataset.artifact is required"))
    (unless (string= (or (jsown:val-safe artifact "kind") "")
                     "github-actions-artifact")
      (error "Only github-actions-artifact datasets are currently supported"))
    (llm-required-json-string artifact "repository")
    (llm-required-json-string artifact "name")
    (let ((run-id (jsown:val-safe artifact "run_id")))
      (unless (or (and (integerp run-id) (plusp run-id))
                  (and (stringp run-id)
                       (plusp (length run-id))
                       (every #'digit-char-p run-id)))
        (error "dataset.artifact.run_id must be a positive GitHub Actions run id")))
    (values base-model max-seq-length gpu-ram max-hourly max-total artifact)))

(defun llm-validate-synthesis-request (request)
  "Validate synthesis work before it leaves the actor mailbox."
  (let ((count (jsown:val-safe request "max_examples"))
        (families (llm-sequence-list
                   (jsown:val-safe request "task_families"))))
    (unless (and (integerp count)
                 (plusp count)
                 (<= count +llm-max-synth-examples+))
      (error "max_examples must be an integer from 1 through ~d"
             +llm-max-synth-examples+))
    (unless families
      (error "task_families must be non-empty"))
    (dolist (family families)
      (unless (member family +llm-synthetic-task-families+ :test #'string=)
        (error "Unsupported synthesis task family ~s" family)))
    request))

(defun llm-synthesis-job-put (job)
  (bt:with-lock-held (*llm-synthesis-jobs-lock*)
    (setf (gethash (llm-synthesis-job-id job) *llm-synthesis-jobs*) job))
  job)

(defun llm-synthesis-job-get (job-id)
  (bt:with-lock-held (*llm-synthesis-jobs-lock*)
    (gethash job-id *llm-synthesis-jobs*)))

(defun llm-synthesis-job-update (job &key status output error)
  (bt:with-lock-held (*llm-synthesis-jobs-lock*)
    (when status
      (setf (llm-synthesis-job-status job) status))
    (when output
      (setf (llm-synthesis-job-output job) output))
    (when error
      (setf (llm-synthesis-job-error job) error))
    (setf (llm-synthesis-job-updated-at job) (llm-now)))
  job)

(defun llm-synthesis-job-json-object (job)
  (let ((json
          (jsown:new-js
            ("job_id" (llm-synthesis-job-id job))
            ("status" (string-downcase
                        (symbol-name (llm-synthesis-job-status job))))
            ("created_at" (llm-synthesis-job-created-at job))
            ("updated_at" (llm-synthesis-job-updated-at job)))))
    (when (llm-synthesis-job-error job)
      (setf (jsown:val json "error") (llm-synthesis-job-error job)))
    json))

(defun llm-run-synthesis-job (job)
  "Run teacher calls outside the Sento receive handler."
  (handler-case
      (progn
        (llm-synthesis-job-update job :status :running)
        (let ((output (llm-synthesize-jsonl (llm-synthesis-job-request job))))
          (llm-synthesis-job-update job :status :succeeded :output output)))
    (error (condition)
      (log:error "llm synthesis job ~a failed: ~a"
                 (llm-synthesis-job-id job) condition)
      (llm-synthesis-job-update job
                                :status :failed
                                :error (princ-to-string condition))))
  job)

(defun llm-submit-synthesis (request)
  "Validate and queue synthesis work without pinning the actor mailbox."
  (llm-validate-synthesis-request request)
  (let* ((id (star.ids:ulid))
         (now (llm-now))
         (job (make-llm-synthesis-job
               :id id
               :status :queued
               :created-at now
               :updated-at now
               :request request)))
    (llm-synthesis-job-put job)
    (bt:make-thread (lambda () (llm-run-synthesis-job job))
                    :name (format nil "llm-synthesis-~a" id))
    (llm-synthesis-job-json-object job)))

(defun llm-synthesis-result-json-object (job)
  (let ((json (llm-synthesis-job-json-object job)))
    (when (llm-synthesis-job-output job)
      (setf (jsown:val json "output") (llm-synthesis-job-output job)))
    json))

(defun llm-vast-wait-running (instance-id)
  "Wait for INSTANCE-ID to run, handling Vast's null provisioning status safely."
  (loop with deadline = (+ (get-universal-time)
                           +llm-default-provision-timeout-seconds+)
        for state = (llm-vast-instance-json instance-id)
        for status = (jsown:val-safe state "actual_status")
        do (cond
             ((and (stringp status) (string= status "running"))
              (return state))
             ((and (stringp status)
                   (member status '("exited" "unknown" "offline")
                           :test #'string=))
              (error "Vast instance ~a entered terminal state ~a"
                     instance-id status))
             ((> (get-universal-time) deadline)
              (error "Timed out waiting for Vast instance ~a; last status ~s"
                     instance-id status)))
           (sleep 5)))

(defun llm-budget-seconds (hourly max-total)
  "Return a bounded runtime derived from hourly and total USD caps."
  (unless (llm-positive-real-p hourly)
    (error "hourly GPU cost must be positive"))
  (unless (llm-positive-real-p max-total)
    (error "total GPU budget must be positive"))
  (let ((seconds (floor (* 3600 (/ max-total hourly)))))
    (max 60 (min seconds +llm-default-max-training-seconds+))))

(defun llm-submit-fine-tune (request)
  "Validate and queue a fine-tune without blocking the actor dispatcher."
  (llm-fine-tune-request-values request)
  (let* ((id (star.ids:ulid))
         (now (llm-now))
         (job (make-llm-fine-tune-job
               :id id
               :status :queued
               :created-at now
               :updated-at now
               :request request)))
    (llm-job-put job)
    (bt:make-thread (lambda () (llm-run-fine-tune-job job))
                    :name (format nil "llm-fine-tune-~a" id))
    (llm-job-json-object job)))

(defun llm-dispatch-message (message)
  "Keep the actor mailbox bounded: dispatch long work to job threads."
  (case (getf message :op)
    ((:synthesize :synthesize-submit)
     (llm-submit-synthesis (getf message :request)))
    (:synthesize-status
     (let ((job (llm-synthesis-job-get (getf message :job-id))))
       (and job (llm-synthesis-job-json-object job))))
    (:synthesize-result
     (let ((job (llm-synthesis-job-get (getf message :job-id))))
       (and job (llm-synthesis-result-json-object job))))
    (:fine-tune
     (llm-submit-fine-tune (getf message :request)))
    (:status
     (let ((job (llm-job-get (getf message :job-id))))
       (and job (llm-job-json-object job))))
    (otherwise
     (error "Unknown llm.starintel.actor operation ~s" (getf message :op)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(llm-submit-synthesis
            llm-synthesis-job-get
            llm-synthesis-job-json-object
            llm-validate-synthesis-request)
          :star.actors))
