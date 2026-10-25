(defpackage :star.router.lifecycle.tests (:use :cl :fiveam))
(in-package :star.router.lifecycle.tests)
(def-suite :router-lifecycle)
(in-suite :router-lifecycle)

(defun manifest ()
  '(:wire-version 1 :types nil
    :messages ((:kind :message :name "fixture/run@1"
                :fields ((:name "text" :type "string" :required t)))
               (:kind :message :name "fixture/result@1"
                :fields ((:name "text" :type "string" :required t))))
    :actors ((:name "worker" :runtime :external :accepts ("fixture/run@1")
              :produces ("fixture/result@1")))))
(defun peer (&optional (byte 1))
  (make-array 32 :element-type '(unsigned-byte 8) :initial-element byte))
(defun context ()
  (star.auth::%make-service-call-context :principal-id "fixture" :principal-type "api_client"
                                       :credential-id "fixture-key" :scopes '("documents:write")))
(defun command (&key (id "one") deadline (text "synthetic"))
  (staractorprotocol:make-command-envelope
   :message-id id :idempotency-key id :correlation-id id
   :message-type "fixture/run@1" :actor "worker" :sender "fixture.client"
   :reply-to "fixture.client" :deadline deadline :dataset "fixture"
   :payload (list (cons "text" text))))
(defun runtime (handler &rest options)
  (apply #'star.router.lifecycle:make-runtime-binding (manifest) "worker" handler
         (lambda (key) (declare (ignore key)) (context))
         :clock (lambda () "2026-10-25T12:00:00Z") options))
(defun complete (context command)
  (declare (ignore context command))
  (starlangruntime:complete-dispatch :message-type "fixture/result@1"
                                    :payload '(("text" . "committed"))))
(defun kinds (outputs) (mapcar (lambda (envelope) (getf envelope :kind)) outputs))

(test real-runtime-completion-replay-and-conflict
  (let* ((calls 0)
         (runtime (runtime (lambda (context command)
                             (incf calls) (complete context command)))))
    (is (equal '(:ack :reply :ack)
               (kinds (star.router.lifecycle:runtime-handle-envelope runtime (command) (peer)))))
    (is (equal '(:reply :ack)
               (kinds (star.router.lifecycle:runtime-handle-envelope runtime (command) (peer)))))
    (is (= 1 calls))
    (signals starlangruntime:wire-dispatcher-idempotency-conflict-error
      (star.router.lifecycle:runtime-handle-envelope runtime (command :text "changed") (peer)))
    (is (= 1 calls))))

(test real-runtime-cancel-before-run-and-peer-isolation
  (let* ((calls 0) (command (command))
         (runtime (runtime (lambda (context command)
                             (incf calls) (complete context command))))
         (cancel (staractorprotocol:make-cancel-envelope
                  command :message-id "cancel" :actor "worker" :reason "fixture")))
    (star.router.lifecycle:runtime-submit-envelope runtime command (peer))
    (star.router.lifecycle:runtime-submit-envelope runtime cancel (peer))
    (let ((output (star.router.lifecycle:runtime-run-next runtime (peer))))
      (is (equal '(:error) (kinds output)))
      (is (equal "star.cancelled" (getf (getf (first output) :payload) :code))))
    (is (zerop calls))
    (is (equal '(:ack :reply :ack)
               (kinds (star.router.lifecycle:runtime-handle-envelope runtime command (peer 2)))))
    (is (= 1 calls))))

(test real-runtime-deadline-and-queue-budget
  (let ((runtime (runtime #'complete :max-queue 1)))
    (star.router.lifecycle:runtime-submit-envelope
     runtime (command :deadline "2026-10-25T11:00:00Z") (peer))
    (signals star.router.application:application-rejection
      (star.router.lifecycle:runtime-submit-envelope runtime (command :id "two") (peer)))
    (let ((output (star.router.lifecycle:runtime-run-next runtime (peer))))
      (is (equal '(:error) (kinds output)))
      (is (equal "star.deadline-exceeded" (getf (getf (first output) :payload) :code))))))

(test real-runtime-defer-cancel-late-result
  (let* ((runtime (runtime (lambda (context command)
                             (declare (ignore context command))
                             (starlangruntime:defer-dispatch))))
         (command (command)))
    (is (equal '(:ack)
               (kinds (star.router.lifecycle:runtime-handle-envelope runtime command (peer)))))
    (is (equal '(:error)
               (kinds (star.router.lifecycle:runtime-submit-envelope
                       runtime (staractorprotocol:make-cancel-envelope
                                command :message-id "cancel" :actor "worker" :reason "fixture")
                       (peer)))))
    (is (null (star.router.lifecycle:runtime-finish-deferred runtime (peer) command
                                                            (complete (context) command))))))

(test runtime-revocation-and-session-budget
  (let* ((allowed t)
         (runtime (star.router.lifecycle:make-runtime-binding
                   (manifest) "worker" #'complete
                   (lambda (key) (declare (ignore key)) (when allowed (context)))
                   :max-requests-per-peer 1)))
    (star.router.lifecycle:runtime-handle-envelope runtime (command) (peer))
    (signals star.router.application:application-rejection
      (star.router.lifecycle:runtime-submit-envelope runtime (command :id "two") (peer)))
    (setf allowed nil)
    (signals star.router.application:application-rejection
      (star.router.lifecycle:runtime-run-next runtime (peer)))))

(test exhausted-budget-allows-only-bounded-known-cancellation
  (let* ((runtime (runtime #'complete :max-requests-per-peer 1))
         (command (command))
         (cancel (staractorprotocol:make-cancel-envelope
                  command :message-id "cancel" :actor "worker" :reason "fixture")))
    (star.router.lifecycle:runtime-submit-envelope runtime command (peer))
    (dotimes (index 100)
      (setf (getf cancel :message-id) (format nil "cancel-~D" index))
      (star.router.lifecycle:runtime-submit-envelope runtime cancel (peer)))
    (signals star.router.application:application-rejection
      (star.router.lifecycle:runtime-submit-envelope
       runtime (staractorprotocol:make-cancel-envelope
                (command :id "unknown") :message-id "unknown-cancel"
                :actor "worker" :reason "fixture") (peer)))
    ;; Matching message with unrelated correlation must not expand the second
    ;; index (StarLang itself matches cancellation by either target dimension).
    (setf (getf (getf cancel :payload) :target-correlation-id) "unknown"
          (getf cancel :correlation-id) "unknown")
    (signals star.router.application:application-rejection
      (star.router.lifecycle:runtime-submit-envelope runtime cancel (peer)))
    (let* ((session (star.router.lifecycle::authenticated-session runtime (peer)))
           (dispatcher (star.router.lifecycle::runtime-session-dispatcher session)))
      (is (= 1 (star.router.lifecycle::runtime-session-requests session)))
      (is (= 1 (hash-table-count
                (starlangruntime::deterministic-dispatcher-cancelled-messages dispatcher))))
      (is (= 1 (hash-table-count
                (starlangruntime::deterministic-dispatcher-cancelled-correlations dispatcher)))))
    (is (equal '(:error)
               (kinds (star.router.lifecycle:runtime-run-next runtime (peer)))))))
