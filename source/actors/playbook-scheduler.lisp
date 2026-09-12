(in-package :star.playbook)

(defvar *playbook-scheduler* nil
  "Supervised StarIntel actor serializing playbook run lifecycle commands.")

(defun command-value (message key &optional default)
  (if (getf message key)
      (getf message key)
      default))

(defun required-command-value (message key)
  (or (getf message key)
      (fail-playbook :invalid-command "Missing command field ~s" key)))

(defun handle-playbook-command (message)
  "Execute one scheduler command.

HTTP/control-plane adapters must derive TENANT-ID from authenticated context
before sending commands here. The scheduler still requires a tenant on every
lookup so a run id alone is never authority."
  (unless (listp message)
    (fail-playbook :invalid-command "Scheduler message must be a property list"))
  (let ((operation (required-command-value message :op)))
    (ecase operation
      (:start
       (start-playbook-run
        (required-command-value message :tenant-id)
        (required-command-value message :playbook-id)
        (required-command-value message :version)
        :principal-id (getf message :principal-id)
        :facts (getf message :facts)
        :autorun (if (member :autorun message) (getf message :autorun) t)))
      (:pause
       (pause-playbook-run
        (required-command-value message :tenant-id)
        (required-command-value message :run-id)))
      (:resume
       (resume-playbook-run
        (required-command-value message :tenant-id)
        (required-command-value message :run-id)))
      (:stop
       (stop-playbook-run
        (required-command-value message :tenant-id)
        (required-command-value message :run-id)))
      (:step
       (step-playbook-run
        (required-command-value message :tenant-id)
        (required-command-value message :run-id)))
      (:assert-fact
       (let ((run
               (require-run
                (required-command-value message :tenant-id)
                (required-command-value message :run-id))))
         (assert-playbook-fact
          run
          (required-command-value message :kind)
          (getf message :payload))
         run))
      (:get
       (require-run
        (required-command-value message :tenant-id)
        (required-command-value message :run-id)))
      (:list
       (list-playbook-runs (required-command-value message :tenant-id))))))

(defun start-playbook-scheduler ()
  (unless star.actors:*sys*
    (fail-playbook :actor-system-unavailable
                   "StarIntel actor system is not running"))
  (setf *playbook-scheduler*
        (sento.actor:actor-of
         star.actors:*sys*
         :name "*playbook-scheduler*"
         :receive
         (lambda (message)
           (handler-case
               (sento.actor:reply (handle-playbook-command message))
             (playbook-error (condition)
               (sento.actor:reply
                (list :status :error
                      :code (playbook-error-code condition)
                      :message (playbook-error-message condition))))))))
  *playbook-scheduler*)

(nhooks:add-hook star:*actors-start-hook* #'start-playbook-scheduler)
