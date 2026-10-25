(in-package :star.router.lifecycle.tests)
(def-suite :router-receiver-journal)
(in-suite :router-receiver-journal)

(defun receiver-test-store (&optional fail-state)
  ;; External CAS port fixture only; lifecycle and codecs execute unchanged.
  (let ((rows (make-hash-table :test #'equal)) (revision 0))
    (star.router.journal:make-journal-store
     :load-fn (lambda (key) (gethash key rows))
     :save-fn (lambda (row)
                (when (and fail-state (equal fail-state (jsown:val-safe row "state")))
                  (error "synthetic durable save failure"))
                (setf (jsown:val row "_rev") (write-to-string (incf revision))
                      (gethash (jsown:val row "_id") rows) row)
                row))))

(defun durable-runtime (store handler &key checkpoint (principal "fixture") (contract (manifest)))
  (star.router.lifecycle:make-runtime-binding
   contract "worker" handler
   (lambda (key)
     (declare (ignore key))
     (star.auth::%make-service-call-context
      :principal-id principal :principal-type "api_client" :credential-id "fixture-key"
      :scopes '("documents:write" "tenant:default" "dataset:fixture")))
   :journal store :durable-p t :journal-checkpoint checkpoint
   :clock (lambda () "2026-10-25T12:00:00Z")))

(test durable-mode-requires-storage-and-volatile-is-explicit
  (signals star.router.application:application-rejection
    (star.router.lifecycle:make-runtime-binding
     (manifest) "worker" #'complete (lambda (key) (declare (ignore key)) (context)))))

(test canonical-array-identity-survives-real-wire-journal-roundtrip
  (let* ((store (receiver-test-store)) (calls 0)
         (contract (manifest)) (command (command))
         (handler (lambda (context envelope) (incf calls) (complete context envelope))))
    (push '(:name "tags" :type (:list "string") :required t)
          (getf (first (getf contract :messages)) :fields))
    (push (cons "tags" (vector "CaseSensitive" "second")) (getf command :payload))
    (star.router.lifecycle:runtime-handle-envelope
     (durable-runtime store handler :contract contract) command (peer))
    (let ((replay (star.router.lifecycle:runtime-handle-envelope
                   (durable-runtime store handler :contract contract)
                   (staractorprotocol:snapshot-portable-wire-value command) (peer))))
      (is (equal '(:reply :ack) (kinds replay)))
      (is (= 1 calls)))
    (setf (aref (cdr (assoc "tags" (getf command :payload) :test #'equal)) 0) "casesensitive")
    (signals starlangruntime:wire-dispatcher-idempotency-conflict-error
      (star.router.lifecycle:runtime-handle-envelope
       (durable-runtime store handler :contract contract) command (peer)))
    (is (= 1 calls))))

(test restart-replays-exact-terminal-and-conflicts-never-execute
  (let* ((store (receiver-test-store)) (calls 0)
         (handler (lambda (context command) (incf calls) (complete context command)))
         (first (durable-runtime store handler))
         (outputs (star.router.lifecycle:runtime-handle-envelope first (command) (peer)))
         (second (durable-runtime store handler))
         ;; A rotated transport key retains the same application principal.
         (replay (star.router.lifecycle:runtime-handle-envelope second (command) (peer 2))))
    (is (equal '(:ack :reply :ack) (kinds outputs)))
    (is (equalp (rest outputs) replay))
    (is (= 1 calls))
    (signals starlangruntime:wire-dispatcher-idempotency-conflict-error
      (star.router.lifecycle:runtime-handle-envelope second (command :text "changed") (peer 2)))
    (is (= 1 calls))
    ;; Another application principal cannot observe/reuse the first result.
    (star.router.lifecycle:runtime-handle-envelope
     (durable-runtime store handler :principal "other") (command) (peer))
    (is (= 2 calls))))

(test interruption-at-admission-start-effect-or-terminal-never-reexecutes
  ;; Nonlocal exits model loss of the entire owner. This is a persistence-port
  ;; unit fixture, not the separate SIGKILL/CouchDB acceptance test.
  (dolist (stage '(:admitted :started :effect :terminal))
    (let* ((store (receiver-test-store)) (calls 0)
           (handler (lambda (context command)
                      (incf calls)
                      (when (eq stage :effect) (throw 'interruption :effect))
                      (complete context command)))
           (first (durable-runtime
                   store handler
                   :checkpoint (lambda (current command)
                                 (declare (ignore command))
                                 (when (eq stage current) (throw 'interruption current))))))
      (is (eq stage (catch 'interruption
                      (star.router.lifecycle:runtime-handle-envelope first (command) (peer)))))
      (let* ((before calls)
             (second (durable-runtime store handler))
             (replay (star.router.lifecycle:runtime-handle-envelope second (command) (peer))))
        (is (= before calls))
        (if (eq stage :terminal)
            (is (equal '(:reply :ack) (kinds replay)))
            (progn
              (is (equal '(:error) (kinds replay)))
              (is (equal "star.outcome-unknown" (getf (getf (first replay) :payload) :code)))
              (let ((task (staractorprotocol:actor2actor-advance-task
                           (staractorprotocol:make-actor2actor-task
                            :task-id "one" :actor "worker" :correlation-id "one" :status :accepted)
                           (first replay))))
                (is (not (staractorprotocol:actor2actor-task-terminal-p task)))
                (is (eq :outcome-unknown (staractorprotocol:actor2actor-task-observation task))))))))))

(test admission-or-terminal-write-failure-never-claims-success
  (dolist (state '("admitted" "terminal"))
    (let* ((store (receiver-test-store state)) (calls 0)
           (handler (lambda (context command) (incf calls) (complete context command)))
           (runtime (durable-runtime store handler)))
      (signals error (star.router.lifecycle:runtime-handle-envelope runtime (command) (peer)))
      (is (= (if (equal state "admitted") 0 1) calls))
      (when (equal state "terminal")
        (let ((replay (star.router.lifecycle:runtime-handle-envelope
                       (durable-runtime store handler) (command) (peer))))
          (is (equal "star.outcome-unknown" (getf (getf (first replay) :payload) :code)))
          (is (= 1 calls)))))))

(test durable-cancellation-survives-owner-recreation-before-effect
  (let* ((store (receiver-test-store)) (calls 0)
         (handler (lambda (context command) (incf calls) (complete context command)))
         (first (durable-runtime store handler))
         (cancel (staractorprotocol:make-cancel-envelope
                  (command) :message-id "cancel" :actor "worker" :reason "fixture")))
    (star.router.lifecycle:runtime-submit-envelope first (command) (peer))
    (star.router.lifecycle:runtime-submit-envelope first cancel (peer))
    (let ((replay (star.router.lifecycle:runtime-handle-envelope
                   (durable-runtime store handler) (command) (peer))))
      (is (equal '(:error) (kinds replay)))
      (is (equal "star.cancelled" (getf (getf (first replay) :payload) :code)))
      (is (zerop calls)))))

(test durable-observation-does-not-run-already-queued-work
  (let* ((store (receiver-test-store)) (calls 0)
         (runtime (durable-runtime store
                   (lambda (context command) (incf calls) (complete context command)))))
    (star.router.lifecycle:runtime-handle-envelope runtime (command) (peer))
    (star.router.lifecycle:runtime-submit-envelope runtime (command :id "two") (peer))
    (is (equal '(:reply :ack)
               (kinds (star.router.lifecycle:runtime-handle-envelope runtime (command) (peer)))))
    (is (= 1 calls))
    ;; Even an observation for that same pending command cannot start it.
    (let ((outputs (star.router.lifecycle:runtime-handle-envelope
                    runtime (command :id "two") (peer))))
      (is (equal "star.outcome-unknown" (getf (getf (first outputs) :payload) :code))))
    (is (= 1 calls))
    (star.router.lifecycle:runtime-run-next runtime (peer))
    (is (= 2 calls))))

(test malformed-result-after-effect-is-durable-unknown
  (let* ((store (receiver-test-store)) (calls 0)
         (handler (lambda (context command)
                    (declare (ignore context command))
                    (incf calls)
                    ;; The canonical dispatcher, not a local result codec,
                    ;; rejects this after the application effect has occurred.
                    '(:outcome :not-a-dispatch-result)))
         (runtime (durable-runtime store handler))
         (outputs (star.router.lifecycle:runtime-handle-envelope runtime (command) (peer)))
         (replay (star.router.lifecycle:runtime-handle-envelope
                  (durable-runtime store handler) (command) (peer))))
    (is (equal '(:ack :error) (kinds outputs)))
    (is (equal "star.outcome-unknown" (getf (getf (second outputs) :payload) :code)))
    (is (equalp (rest outputs) replay))
    (is (= 1 calls))))

(test deferred-result-cannot-rebind-command-identity
  (let* ((store (receiver-test-store))
         (runtime (durable-runtime store
                    (lambda (context command)
                      (declare (ignore context command))
                      (starlangruntime:defer-dispatch)))))
    (star.router.lifecycle:runtime-handle-envelope runtime (command) (peer))
    (signals starlangruntime:wire-dispatcher-idempotency-conflict-error
      (star.router.lifecycle:runtime-finish-deferred
       runtime (peer) (command :text "changed") (complete (context) (command))))
    (is (equal '(:reply :ack)
               (kinds (star.router.lifecycle:runtime-finish-deferred
                       runtime (peer) (command) (complete (context) (command))))))))
