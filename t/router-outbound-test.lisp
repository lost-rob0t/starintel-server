(defpackage :star.router.outbound.tests (:use :cl :fiveam))
(in-package :star.router.outbound.tests)
(def-suite :router-outbound)
(in-suite :router-outbound)

(defun manifest ()
  '(:messages ((:name "fixture/run@1" :fields ((:name "text" :type "string" :required t)))
               (:name "fixture/result@1" :fields ((:name "text" :type "string" :required t))))))
(defun reference (&optional (generation 1))
  (staractorprotocol:make-star-actor-reference :domain-id "fixture" :logical-path "worker"
                                             :node-id "localhost" :generation generation))
(defun context (&optional (id "actor-principal"))
  (star.auth::%make-service-call-context :principal-id id :principal-type "actor"
                                       :scopes '("targets:read") :credential-id "fixture-key"))
(defun key (&optional (value 7))
  (make-array 32 :element-type '(unsigned-byte 8) :initial-element value))
(defun command ()
  (staractorprotocol:make-command-envelope
   :message-id "execution-1" :message-type "fixture/run@1" :actor "worker"
   :sender "server" :reply-to "server" :correlation-id "trace-1"
   :idempotency-key "execution-1" :dataset "fixture"
   :payload '(("text" . "synthetic"))))
(defun ack (status &optional (id "ack-1"))
  (staractorprotocol:make-ack-envelope (command) :message-id id :actor "worker"
                                      :sender "worker" :status status))
(defun observed (&key (generation 1) (attempt 1) (fence 9) cancel)
  (list :actor-reference (reference generation) :attempt attempt
        :execution-id "execution-1" :lease-id "lease-1" :fencing-token fence
        :lease-valid-p t :authorized-p t :cancel-won-p cancel))
(defun store ()
  ;; Only external persistence is in memory; canonical guard and journal CAS
  ;; boundary execute normally. This is not a CouchDB/crash durability proof.
  (let ((rows (make-hash-table :test #'equal)) (revision 0))
    (star.router.journal:make-journal-store
     :load-fn (lambda (id) (gethash id rows))
     :save-fn (lambda (row)
                (setf (jsown:val row "_rev") (format nil "~D" (incf revision))
                      (gethash (jsown:val row "_id") rows) (copy-tree row))
                row))))
(defun coordinator (store &key (observation (constantly (observed)))
                                    (authenticate (lambda (key) (declare (ignore key))
                                                    (values (context) "tenant")))
                                    (clock (constantly 100)))
  (star.router.outbound:make-coordinator
   (manifest) store authenticate
   (lambda (&rest ignored) (declare (ignore ignored)) (funcall observation)) :clock clock))
(defun register (coordinator)
  (star.router.outbound:register-invocation coordinator (command) (reference)
                                           (context) (key) "tenant" "execution-1" "lease-1" 9))

(test canonical-outcomes-not-queueing-own-completion
  (let ((coordinator (coordinator (store))))
    (register coordinator)
    (star.router.outbound:mark-transport-queued coordinator "execution-1")
    (is (equal "no" (jsown:val (star.router.outbound:invocation-snapshot coordinator "execution-1") "terminal")))
    (is (eq :accepted (star.router.outbound:observe-outcome coordinator (ack :accepted) (key))))
    (is (eq :duplicate (star.router.outbound:observe-outcome coordinator (ack :accepted) (key))))
    (let ((reply (staractorprotocol:make-reply-envelope
                  (command) :message-id "reply" :actor "worker" :sender "worker"
                  :message-type "fixture/result@1" :payload '(("text" . "result")))))
      (is (eq :accepted (star.router.outbound:observe-outcome coordinator reply (key))))
      (is (equal "no" (jsown:val (star.router.outbound:invocation-snapshot coordinator "execution-1") "terminal"))))
    (is (eq :accepted (star.router.outbound:observe-outcome coordinator (ack :completed "done") (key))))
    (is (eq :already-terminal (star.router.outbound:observe-outcome coordinator (ack :rejected "late") (key))))
    (is (equal "completed" (jsown:val (star.router.outbound:invocation-snapshot coordinator "execution-1") "state")))))

(test wrong-key-principal-generation-attempt-and-target-fence
  (let* ((observation (observed)) (principal "actor-principal")
         (coordinator (coordinator (store) :observation (lambda () observation)
                                   :authenticate (lambda (key) (declare (ignore key))
                                                   (values (context principal) "tenant")))))
    (register coordinator)
    (signals star.router.application:application-rejection
      (star.router.outbound:observe-outcome coordinator (ack :completed) (key 8)))
    (setf principal "other")
    (signals star.router.application:application-rejection
      (star.router.outbound:observe-outcome coordinator (ack :completed) (key)))
    (setf principal "actor-principal" observation (observed :generation 2))
    (is (eq :stale-generation (star.router.outbound:observe-outcome coordinator (ack :completed) (key))))
    (setf observation (observed :attempt 2))
    (is (eq :stale-attempt (star.router.outbound:observe-outcome coordinator (ack :completed) (key))))
    (setf observation (observed :fence 10))
    (signals star.router.application:application-rejection
      (star.router.outbound:observe-outcome coordinator (ack :completed) (key)))
    (is (equal "pending" (jsown:val (star.router.outbound:invocation-snapshot coordinator "execution-1") "state")))))

(test durable-existing-record-does-not-authorize-reexecution
  (let* ((store (store)) (first (coordinator store)) (recovered (coordinator store)))
    (is (eq :registered (nth-value 1 (register first))))
    (is (eq :existing (nth-value 1 (register recovered))))
    (star.router.outbound:observe-outcome first (ack :completed) (key))
    (is (eq :already-terminal (star.router.outbound:observe-outcome recovered (ack :rejected) (key))))))

(test deadline-and-authoritative-cancellation-win-before-terminal-result
  (let* ((now 100) (observation (observed))
         (coordinator (coordinator (store) :clock (lambda () now) :observation (lambda () observation))))
    (register coordinator)
    (setf now 131)
    (is (eq :deadline-exceeded (star.router.outbound:arbitrate-invocation coordinator "execution-1" (key))))
    (is (equal "outcome-unknown"
               (jsown:val (star.router.outbound:invocation-snapshot coordinator "execution-1") "observation")))
    (is (eq :deadline-exceeded (star.router.outbound:observe-outcome coordinator (ack :completed) (key))))
    (setf observation (observed :cancel t))
    (is (eq :cancelled (star.router.outbound:arbitrate-invocation coordinator "execution-1" (key))))
    (is (eq :already-terminal (star.router.outbound:observe-outcome coordinator (ack :completed) (key))))))

(test unknown-outcome-never-completes-and-store-failure-propagates
  (let ((coordinator (coordinator (store))))
    (register coordinator)
    (star.router.outbound:observe-outcome
     coordinator (staractorprotocol:make-error-envelope
                  (command) :message-id "uncertain" :actor "worker" :sender "worker"
                  :code "star.outcome-unknown" :message "reconcile" :retryable nil) (key))
    (is (equal "no" (jsown:val (star.router.outbound:invocation-snapshot coordinator "execution-1") "terminal"))))
  (let ((coordinator
          (coordinator (star.router.journal:make-journal-store
                        :load-fn (constantly nil)
                        :save-fn (lambda (row) (declare (ignore row)) (error "storage unavailable"))))))
    (signals error (register coordinator))
    (is (zerop (hash-table-count (star.router.outbound::coordinator-index coordinator))))))

(test canonical-correlation-and-dataset-cannot-be-rewritten-by-peer
  (let ((coordinator (coordinator (store))))
    (register coordinator)
    (dolist (field '(:correlation-id :dataset :actor))
      (let ((outcome (ack :completed)))
        (setf (getf outcome field) "other")
        (signals staractorprotocol:invalid-wire-envelope-error
          (star.router.outbound:observe-outcome coordinator outcome (key)))))
    (is (equal "pending" (jsown:val (star.router.outbound:invocation-snapshot coordinator "execution-1") "state")))))

(test registration-identity-is-case-sensitive
  (let ((coordinator (coordinator (store))))
    (register coordinator)
    (let ((changed (command)))
      (setf (getf changed :payload) '(("text" . "Synthetic")))
      (signals error
        (star.router.outbound:register-invocation coordinator changed (reference)
                                                 (context) (key) "tenant" "execution-1" "lease-1" 9)))
    (let ((changed-reference
            (staractorprotocol:make-star-actor-reference
             :domain-id "fixture" :logical-path "Worker" :node-id "localhost" :generation 1)))
      (signals error
        (star.router.outbound:register-invocation coordinator (command) changed-reference
                                                 (context) (key) "tenant" "execution-1" "lease-1" 9)))))
