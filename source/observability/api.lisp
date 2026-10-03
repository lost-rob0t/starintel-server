(in-package :star.observability)

;; Resource identity. Overridden via env in the gserver glue; defaults are
;; honest unknowns rather than fabrications.

(defparameter *service-name*
  (or (uiop:getenv "STAR_SERVICE_NAME") "starintel-server")
  "service.name resource attribute.")

(defparameter *service-version*
  (or (uiop:getenv "STAR_SERVICE_VERSION") "dev")
  "service.version resource attribute.")

(defparameter *service-instance-id*
  (or (uiop:getenv "STAR_SERVICE_INSTANCE_ID") (make-span-id))
  "service.instance.id resource attribute.")

(defparameter *deployment-environment*
  (or (uiop:getenv "STAR_DEPLOYMENT_ENVIRONMENT") "production")
  "deployment.environment.name resource attribute.")

(defparameter *node-id*
  (or (uiop:getenv "STARINTEL_NODE_ID") "unknown-node")
  "starintel.node.id resource attribute (rendered from networking.hostName
by the NixOS service definitions).")

(defparameter *component*
  (or (uiop:getenv "STARINTEL_COMPONENT") "app")
  "starintel.component resource attribute.")

;; ---- Records --------------------------------------------------------------

(defstruct span-record
  "One finished span queued for export."
  context name kind start-nanos end-nanos attributes status)

(defstruct log-record
  "One structured log event queued for export."
  context time-nanos severity body attributes)

(defstruct metric-record
  "One metric sample queued for export."
  name value time-nanos attributes)

;; ---- Span API -------------------------------------------------------------

(defmacro with-span ((name &key kind (attributes '())) &body body)
  "Run BODY inside a span named NAME, exporting it when done.

Attributes must be an alist of (KEY . VALUE) and never contain payload
bodies. An error marks the span :error and re-signals: telemetry must not
change application behavior."
  (let ((start (gensym "START-"))
        (context (gensym "CTX-"))
        (result (gensym "RESULT-"))
        (ok (gensym "OK-")))
    `(let* ((,context (child-context (current-context)))
            (,start (now-unix-nanos))
            (,result nil)
            (,ok nil))
       (let ((*current-trace-context* ,context))
         (unwind-protect
              (multiple-value-prog1
                  (progn ,@body)
                (setf ,ok t))
           (declare (ignorable ,result ,ok))
           (queue-span ,context ,name (or ,kind 1) ,start (now-unix-nanos)
                       ,attributes (and ,ok t)))))))

(defun queue-span (context name kind start end attributes ok)
  "Queue one finished span. Never blocks; drops when the queue is full."
  (when (signal-enabled-p :traces)
    (let ((queued (queue-push
                   (getf *otlp-queues* :traces)
                   (make-span-record
                    :context context :name name :kind kind
                    :start-nanos start :end-nanos end
                    :attributes attributes
                    :status (if ok :ok :error))
                   *export-queue-limit*)))
      (when (eq queued :dropped)
        (counter-increment :traces-dropped)))))

(defun sampled-p ()
  "SDK-side sampling decision; the collector tail-samples authoritatively.
The default rate of 1.0 defers everything to the collector."
  (<= (random 1.0d0) *trace-sample-rate*))

;; ---- Structured log events ------------------------------------------------

(defun emit-log-event (severity message &key (attributes '()))
  "Emit one structured log event with trace correlation.

SEVERITY is one of :info :warn :error :debug. MESSAGE must never contain
credentials or payload bodies; callers pass structured attributes instead."
  (when (signal-enabled-p :logs)
    (let ((queued (queue-push
                   (getf *otlp-queues* :logs)
                   (make-log-record
                    :context (current-context)
                    :time-nanos (now-unix-nanos)
                    :severity (string-upcase (symbol-name severity))
                    :body message
                    :attributes attributes)
                   *export-queue-limit*)))
      (when (eq queued :dropped)
        (counter-increment :logs-dropped)))))

;; ---- Metrics --------------------------------------------------------------

(defparameter *metric-label-guard*
  '("starintel.actor.id" "starintel.operation.id" "starintel.run.id"
    "starintel.tenant.id" "user.id" "target.id" "document.id" "http.url")
  "Label keys removed from metrics before queueing: IDs must never become
metric dimensions. They remain valid in traces and structured logs.")

(defun record-counter (name value &key (attributes '()))
  "Record one counter increment with bounded-cardinality labels."
  (when (signal-enabled-p :metrics)
    (let ((attributes
            (remove-if
             (lambda (pair) (member (string-downcase
                                     (if (symbolp (car pair))
                                         (symbol-name (car pair))
                                         (princ-to-string (car pair))))
                                    *metric-label-guard*
                                    :test #'string=))
             attributes)))
      (let ((queued (queue-push
                     (getf *otlp-queues* :metrics)
                     (make-metric-record
                      :name name :value value
                      :time-nanos (now-unix-nanos)
                      :attributes attributes)
                     *export-queue-limit*)))
        (when (eq queued :dropped)
          (counter-increment :metrics-dropped))))))

;; ---- Security events ------------------------------------------------------

(defun security-event (event-name &key outcome (attributes '()))
  "Emit a dedicated security/tenancy event. Never record API keys, tokens,
authorization headers, cookies or client secrets here; record the OUTCOME."
  (when (signal-enabled-p :logs)
    (emit-log-event
     (if (member outcome '(:denied :failed :rejected))
         :warn
         :info)
     (format nil "security event: ~a (~a)"
             (string-downcase (symbol-name event-name))
             (if outcome (string-downcase (symbol-name outcome)) "observed"))
     :attributes (append (list (cons "event.name"
                                     (string-downcase
                                      (symbol-name event-name))))
                         (when outcome
                           (list (cons "security.outcome"
                                       (string-downcase
                                        (symbol-name outcome)))))
                         attributes))))
