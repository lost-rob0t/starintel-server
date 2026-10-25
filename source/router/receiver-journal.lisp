(in-package :star.router.lifecycle)
(export '(runtime-journal-key))

(defun receiver-wire (binding envelope)
  (babel:octets-to-string
   (star-actor-wire:encode-binding-message
    :zmq '("star.actor2actor/1") (runtime-binding-manifest binding) envelope)
   :encoding :utf-8))

(defun receiver-decode (binding wire)
  (unless (stringp wire) (star.router.application::reject :invalid-receiver-journal))
  (star-actor-wire:decode-binding-message
   :zmq '("star.actor2actor/1") (runtime-binding-manifest binding)
   (babel:string-to-octets wire :encoding :utf-8)))

(defun receiver-principal (context)
  ;; Key rotation may change credentials without changing application ownership.
  ;; Principal type prevents two authentication realms reusing an ID colliding.
  (let ((id (star.auth:service-call-context-principal-id context))
        (type (or (star.auth:service-call-context-principal-type context) "")))
    (unless (and (stringp id) (plusp (length id)) (stringp type))
      (star.router.application::reject :invalid-journal-principal))
    (format nil "~D:~A~D:~A" (length type) type (length id) id)))

(defun receiver-key (binding context command direction scope)
  (declare (ignore binding))
  (let ((tenant (or (star:tenant-adaptation-for (getf command :dataset)) "default")))
    ;; Tenant selection is the existing server mapping, never a wire credential
    ;; or ROUTER identity. The actual effect still enters application policy.
    (star.router.journal:journal-key (receiver-principal context) tenant direction scope)))

(defun runtime-journal-key (binding context command)
  "Derive the protected receiver row key using canonical scope and trusted identity."
  (receiver-key binding context command "receiver"
                (staractorprotocol:idempotency-scope-key command)))

(defun receiver-marker-key (binding context command dimension value)
  (receiver-key binding context command (format nil "receiver-cancel-~A" dimension)
                (list (getf command :actor) value)))

(defun receiver-alias-key (binding context command message correlation)
  (receiver-key binding context command "receiver-cancel-target"
                (list (getf command :actor) message correlation)))

(defun receiver-cancelled-p (binding context command)
  (or (star.router.journal:journal-read
       (runtime-binding-journal binding)
       (receiver-marker-key binding context command "message" (getf command :message-id)))
      (star.router.journal:journal-read
       (runtime-binding-journal binding)
       (receiver-marker-key binding context command "correlation" (getf command :correlation-id)))))

(defun receiver-check-identity (binding row command)
  (starlangruntime:validate-command-idempotency-identity
   (receiver-decode binding (jsown:val row "commandWire")) command))

(defun receiver-observation (binding command code message &optional message-id now)
  (let ((envelope
          (staractorprotocol:make-error-envelope
           command :message-id (or message-id (princ-to-string (uuid:make-v4-uuid)))
           :actor (getf command :actor) :sender (getf command :actor)
           :code code :message message :retryable nil
           :sent-at (or now (funcall (runtime-binding-clock binding))))))
    ;; Canonical constructors and manifest codec own observation semantics.
    (receiver-wire binding envelope)
    (list envelope)))

(defun receiver-unknown (binding command)
  (receiver-observation binding command "star.outcome-unknown"
                        "Durable admission exists without a reconciled terminal outcome; execution was not repeated."))

(defun receiver-public-outcomes (binding command outcomes)
  ;; The canonical runtime catches invalid handler results as well as errors
  ;; outside the application call (for example an admission I/O error).
  ;; Neither proves the absence of effects. Project that diagnostic to the
  ;; canonical nonterminal unknown observation at this durable boundary;
  ;; leave the runtime record untouched and persist exactly what is emitted.
  (mapcar (lambda (envelope)
            (if (and (eq :error (getf envelope :kind))
                     (equal "star.native-handler-error"
                            (getf (getf envelope :payload) :code)))
                (first (receiver-observation
                        binding command "star.outcome-unknown"
                        "Application effect did not produce a reconciled result."
                        (getf envelope :message-id) (getf envelope :sent-at)))
                envelope))
          outcomes))

(defun receiver-replay (binding row)
  (let ((wires (jsown:val row "outcomeWires"))
        (command (receiver-decode binding (jsown:val row "activeWire"))))
    (unless (and (typep wires 'sequence) (<= 1 (length wires) 64))
      (star.router.application::reject :invalid-receiver-outcomes))
    (map 'list
         (lambda (wire)
           (let ((envelope (receiver-decode binding wire)))
             ;; Validate persisted output against canonical task identity without
             ;; reimplementing actor/correlation/status guards in this adapter.
             (staractorprotocol:actor2actor-advance-task
              (staractorprotocol:make-actor2actor-task
               :task-id (getf command :message-id) :actor (getf command :actor)
               :correlation-id (getf command :correlation-id) :status :accepted)
              envelope)
             envelope))
         wires)))

(defun receiver-checkpoint (binding stage command)
  (when (runtime-binding-journal-checkpoint binding)
    (funcall (runtime-binding-journal-checkpoint binding) stage
             (staractorprotocol:snapshot-portable-wire-value command))))

(defun receiver-record-alias (binding context command key)
  (star.router.journal:journal-transition
   (runtime-binding-journal binding)
   (receiver-alias-key binding context command
                       (getf command :message-id) (getf command :correlation-id))
   (lambda (row)
     (when (and row (not (equal key (jsown:val row "commandKey"))))
       (star.router.application::reject :cancel-target-identity-conflict))
     (if row (values nil nil)
         (values (jsown:new-js ("commandKey" key)) nil)))))

(defun durable-admit-command (binding session command)
  (let* ((context (runtime-session-context session))
         (key (runtime-journal-key binding context command))
         (wire (receiver-wire binding command))
         (now (funcall (runtime-binding-clock binding))))
    (multiple-value-bind (action row)
        (star.router.journal:journal-transition
         (runtime-binding-journal binding) key
         (lambda (row)
           (when row (receiver-check-identity binding row command))
           (cond
             ((and row (member (jsown:val row "state") '("terminal" "unknown") :test #'equal))
              (values nil :replay))
             ((and row (equal "admitted" (jsown:val row "state"))
                   (jsown:keyp row "cancelWire"))
              ;; Cancellation won the same-row CAS before effect admission.
              ;; Deterministic outcome ID makes a CAS retry side-effect free.
              (let ((outcomes
                      (receiver-observation
                       binding (receiver-decode binding (jsown:val row "activeWire"))
                       "star.cancelled" "Durable cancellation preceded effect admission."
                       (concatenate 'string "cancelled/" key) now)))
                (setf (jsown:val row "state") "terminal"
                      (jsown:val row "owner") (runtime-binding-journal-owner binding)
                      (jsown:val row "outcomeWires")
                      (map 'vector (lambda (output) (receiver-wire binding output)) outcomes))
                (values row :replay)))
             ((and row (not (equal "retry" (jsown:val row "state"))))
              (values nil :unknown))
             (t
              (let ((new (or row (jsown:new-js ("commandWire" wire)))))
                (setf (jsown:val new "state") "admitted"
                      (jsown:val new "owner") (runtime-binding-journal-owner binding)
                      (jsown:val new "activeWire") wire)
                (values new :admit))))))
      (ecase action
        (:replay (values nil (receiver-replay binding row)))
        (:unknown
         ;; A crash may have occurred anywhere after admission. Even an
         ;; apparently unstarted row is not implicitly retried on a new owner.
         (values nil (receiver-unknown binding command)))
        (:admit
         ;; Alias persistence must finish before queue/effect admission.
         (receiver-record-alias binding context command key)
         (receiver-checkpoint binding :admitted command)
         (values t nil))))))

(defun durable-admit-cancel (binding session cancel)
  (let* ((context (runtime-session-context session))
         (message (staractorprotocol:cancel-target-message-id cancel))
         (correlation (staractorprotocol:cancel-target-correlation-id cancel))
         (alias (star.router.journal:journal-read
                 (runtime-binding-journal binding)
                 (receiver-alias-key binding context cancel message correlation))))
    (unless alias (star.router.application::reject :unknown-durable-cancel-target))
    (let* ((key (jsown:val alias "commandKey"))
           (row (star.router.journal:journal-read (runtime-binding-journal binding) key))
           (command (and row (receiver-decode binding (jsown:val row "activeWire")))))
      (unless (and command (equal key (runtime-journal-key binding context command)))
        (star.router.application::reject :invalid-durable-cancel-target))
      (receiver-check-identity binding row command)
      ;; Serialize targeted cancellation against the effect-admission CAS.
      ;; If STARTED already won, cancellation is best-effort, never rollback.
      (multiple-value-bind (ignored saved)
          (star.router.journal:journal-transition
           (runtime-binding-journal binding) key
           (lambda (current)
             (unless current (star.router.application::reject :missing-durable-cancel-target))
             (receiver-check-identity binding current command)
             (if (jsown:keyp current "cancelWire") (values nil nil)
                 (progn (setf (jsown:val current "cancelWire") (receiver-wire binding cancel))
                        (values current nil)))))
        (declare (ignore ignored))
        (setf row saved))
      ;; Preserve the canonical OR(message,correlation) cancellation semantics
      ;; across restart. Markers are written BEFORE runtime cancellation.
      ;; Only the direct target shares the effect-admission CAS. These separate
      ;; marker rows are not a transaction with other commands' admission: a
      ;; concurrently starting command sharing the correlation can still run.
      (dolist (pair (list (cons "message" message) (cons "correlation" correlation)))
        (star.router.journal:journal-transition
         (runtime-binding-journal binding)
         (receiver-marker-key binding context command (car pair) (cdr pair))
         (lambda (old)
           (if old (values nil nil)
               (values (jsown:new-js ("cancelWire" (receiver-wire binding cancel))) nil)))))
      (cond
        ((member (jsown:val row "state") '("terminal" "unknown") :test #'equal)
         (values nil (receiver-replay binding row)))
        ((not (equal (jsown:val row "owner") (runtime-binding-journal-owner binding)))
         ;; Cancellation observation does not prove an already-started effect
         ;; rolled back. Preserve the pending row for explicit reconciliation.
         (values nil (receiver-unknown binding command)))
        (t (values t nil))))))

(defun durable-admit-envelope (binding session envelope)
  (ecase (getf envelope :kind)
    (:command (durable-admit-command binding session envelope))
    (:cancel (durable-admit-cancel binding session envelope))))

(defun durable-invoke-handler (binding session command)
  (let* ((context (runtime-session-context session))
         (key (runtime-journal-key binding context command))
         (cancelled (receiver-cancelled-p binding context command)))
    (when cancelled
      (return-from durable-invoke-handler
        (starlangruntime:fail-dispatch :code "star.cancelled"
                                     :message "Durable cancellation preceded effect admission." :retryable nil)))
    (multiple-value-bind (admission row)
        (star.router.journal:journal-transition
         (runtime-binding-journal binding) key
         (lambda (row)
           (unless row (star.router.application::reject :missing-durable-admission))
           (receiver-check-identity binding row command)
           (cond
             ((and (equal "admitted" (jsown:val row "state")) (jsown:keyp row "cancelWire"))
              (values nil :cancelled))
             ((and (equal "admitted" (jsown:val row "state"))
                   (equal (runtime-binding-journal-owner binding) (jsown:val row "owner")))
              (setf (jsown:val row "state") "started") (values row :execute))
             (t (values nil :unknown)))))
      (declare (ignore row))
      (when (eq admission :cancelled)
        (return-from durable-invoke-handler
          (starlangruntime:fail-dispatch :code "star.cancelled" :retryable nil
                                       :message "Durable cancellation preceded effect admission.")))
      (unless (eq admission :execute)
        (return-from durable-invoke-handler
          (starlangruntime:fail-dispatch :code "star.outcome-unknown" :retryable nil
                                       :message "Effect admission was already consumed; execution was not repeated."))))
    (receiver-checkpoint binding :started command)
    (handler-case
        (funcall (runtime-binding-handler binding) context command)
      (error ()
        ;; An unclassified failure after effect admission is not proof of no
        ;; effects. Persist an unknown observation, never an automatic retry.
        (starlangruntime:fail-dispatch :code "star.outcome-unknown" :retryable nil
                                     :message "Application effect did not produce a reconciled result.")))))

(defun durable-checkpoint-result (binding session dispatcher command)
  (let* ((record (gethash (staractorprotocol:idempotency-scope-key command)
                          (starlangruntime:deterministic-dispatcher-idempotency dispatcher)))
         (status (getf record :status))
         (outcomes (receiver-public-outcomes binding command (getf record :outcomes))))
    (when (member status '(:terminal :retry))
      (let ((key (runtime-journal-key binding (runtime-session-context session) command))
            (wires (map 'vector (lambda (envelope) (receiver-wire binding envelope)) outcomes))
            (state (cond ((some (lambda (envelope)
                                 (and (eq :error (getf envelope :kind))
                                      (equal "star.outcome-unknown"
                                             (getf (getf envelope :payload) :code)))) outcomes) "unknown")
                         ((eq status :retry) "retry") (t "terminal"))))
        (let ((saved
                (star.router.journal:journal-transition
                 (runtime-binding-journal binding) key
                 (lambda (row)
                   (unless row (star.router.application::reject :missing-durable-admission))
                   (receiver-check-identity binding row command)
                   (unless (equal (runtime-binding-journal-owner binding) (jsown:val row "owner"))
                     (star.router.application::reject :stale-durable-owner))
                   (if (member (jsown:val row "state") '("terminal" "unknown") :test #'equal)
                       (values nil nil)
                       (progn (setf (jsown:val row "state") state
                                    (jsown:val row "outcomeWires") wires)
                              (values row t)))))))
          (when saved (receiver-checkpoint binding :terminal command)))))))

(defun durable-checkpoint-cancel-results (binding session dispatcher)
  (maphash
   (lambda (key record)
     (declare (ignore key))
     (when (eq :terminal (getf record :status))
       (durable-checkpoint-result binding session dispatcher (getf record :command))))
   (starlangruntime:deterministic-dispatcher-idempotency dispatcher)))
