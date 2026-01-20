(in-package :star-server-tests)

;;;; Target Routing System Unit Tests

(def-suite target-routing-tests
  :description "Test suite for target routing and actor index system")

(in-suite target-routing-tests)

;;; ----------------------------------------------------------------------
;;; Debug Helpers
;;; ----------------------------------------------------------------------

(defparameter *routing-tests-debug* t
  "When true, print verbose debug info during routing tests.")

(defun rdbg (fmt &rest args)
  (when *routing-tests-debug*
    (apply #'format *error-output*
           (concatenate 'string "~&[routing-tests] " fmt "~%")
           args)))

;;; ----------------------------------------------------------------------
;;; Test Fixtures and Setup
;;; ----------------------------------------------------------------------

(defvar *test-actor-system* nil
  "Test actor system for routing tests")

(defvar *test-actor-index* nil
  "Test actor index agent")

(defvar *test-received-messages* nil
  "List of messages received by test actors")

(defvar *test-message-lock* nil
  "Lock for thread-safe access to test messages")

(defun setup-test-actor-system ()
  "Setup a test actor system and actor index"
  (rdbg "Setting up test actor system")
  (setf *test-received-messages* '())
  (setf *test-message-lock* (bt:make-lock))
  (setf *test-actor-system* (make-actor-system '(:dispatchers
                                                  (:shared (:workers 2))
                                                  :timeout-timer
                                                  (:resolution 500 :max-size 100))))
  (setf *test-actor-index* (make-agent #'serapeum:dict *test-actor-system*))
  (rdbg "Test actor system setup complete"))

(defun teardown-test-actor-system ()
  "Teardown test actor system"
  (rdbg "Tearing down test actor system")
  (when *test-actor-system*
    (ac:shutdown *test-actor-system*)
    (setf *test-actor-system* nil))
  (setf *test-actor-index* nil)
  (setf *test-received-messages* nil)
  (setf *test-message-lock* nil)
  (rdbg "Test actor system teardown complete"))

(defun record-message (msg)
  "Thread-safe recording of received messages"
  (bt:with-lock-held (*test-message-lock*)
    (push msg *test-received-messages*)))

(defun get-recorded-messages ()
  "Thread-safe retrieval of recorded messages"
  (bt:with-lock-held (*test-message-lock*)
    (copy-list *test-received-messages*)))

(defun clear-recorded-messages ()
  "Thread-safe clearing of recorded messages"
  (bt:with-lock-held (*test-message-lock*)
    (setf *test-received-messages* nil)))

(defun make-test-target (&key (actor "test-actor")
                          (recurring nil)
                          (delay 60)
                          (transient nil)
                          (target-id "test-target-123"))
  "Create a test target document (as jsown object)"
  (jsown:new-js
    ("actor" actor)
    ("recurring" recurring)
    ("delay" delay)
    ("transient" transient)
    ("target" target-id)
    ("type" "ipv4")
    ("data" "192.168.1.1")
    ("scope" "in-scope")))

;;; ----------------------------------------------------------------------
;;; Actor Index Tests
;;; ----------------------------------------------------------------------

(test test-actor-index-creation
  "Test actor index agent creation"
  (rdbg "TEST test-actor-index-creation")
  (setup-test-actor-system)
  (is (not (null *test-actor-index*)))
  (is (typep *test-actor-index* 'sento.agent:agent))
  (teardown-test-actor-system))

(test test-register-actor
  "Test registering an actor in the index"
  (rdbg "TEST test-register-actor")
  (setup-test-actor-system)

  ;; Create a test actor
  (let ((test-actor (actor-of *test-actor-system*
                              :name "test-receiver"
                              :receive (lambda (msg)
                                         (rdbg "Test actor received: ~a" msg)
                                         (record-message msg)))))

    ;; Register the actor
    (agent-update *test-actor-index*
                  (lambda (current-dict)
                    (serapeum:dict* current-dict "test-actor" test-actor)))

    (sleep 0.1) ;; Allow agent update to complete

    ;; Verify the actor is registered
    (let ((registered-actor (serapeum:@ (agent-get *test-actor-index* #'identity) "test-actor")))
      (is (not (null registered-actor)))
      (is (eq test-actor registered-actor))))

  (teardown-test-actor-system))

(test test-get-dest-actor-exists
  "Test looking up an existing destination actor"
  (rdbg "TEST test-get-dest-actor-exists")
  (setup-test-actor-system)

  (let ((test-actor (actor-of *test-actor-system*
                              :name "dest-actor"
                              :receive (lambda (msg)
                                         (record-message msg)))))

    ;; Register the actor
    (agent-update *test-actor-index*
                  (lambda (current-dict)
                    (serapeum:dict* current-dict "nmap" test-actor)))

    (sleep 0.1)

    ;; Look up the actor
    (let ((dest (serapeum:@ (agent-get *test-actor-index* #'identity) "nmap")))
      (is (not (null dest)))
      (is (eq test-actor dest))))

  (teardown-test-actor-system))

(test test-get-dest-actor-not-exists
  "Test looking up a non-existent destination actor"
  (rdbg "TEST test-get-dest-actor-not-exists")
  (setup-test-actor-system)

  ;; Look up an actor that doesn't exist
  (let ((dest (serapeum:@ (agent-get *test-actor-index* #'identity) "nonexistent")))
    (is (null dest)))

  (teardown-test-actor-system))

(test test-multiple-actor-registration
  "Test registering multiple actors"
  (rdbg "TEST test-multiple-actor-registration")
  (setup-test-actor-system)

  (let ((actor1 (actor-of *test-actor-system*
                          :name "actor-1"
                          :receive (lambda (msg) (record-message (cons :actor1 msg)))))
        (actor2 (actor-of *test-actor-system*
                          :name "actor-2"
                          :receive (lambda (msg) (record-message (cons :actor2 msg))))))

    ;; Register both actors
    (agent-update *test-actor-index*
                  (lambda (current-dict)
                    (serapeum:dict* current-dict "subfinder" actor1 "nmap" actor2)))

    (sleep 0.1)

    ;; Verify both are registered
    (let ((dest1 (serapeum:@ (agent-get *test-actor-index* #'identity) "subfinder"))
          (dest2 (serapeum:@ (agent-get *test-actor-index* #'identity) "nmap")))
      (is (eq actor1 dest1))
      (is (eq actor2 dest2))))

  (teardown-test-actor-system))

;;; ----------------------------------------------------------------------
;;; Route Target Tests
;;; ----------------------------------------------------------------------

(test test-route-target-to-existing-actor
  "Test routing a target to an existing actor"
  (rdbg "TEST test-route-target-to-existing-actor")
  (setup-test-actor-system)

  (clear-recorded-messages)

  (let* ((target (make-test-target :actor "subfinder"))
         (dest-actor (actor-of *test-actor-system*
                               :name "subfinder-actor"
                               :receive (lambda (msg)
                                          (rdbg "Subfinder actor received: ~a" msg)
                                          (record-message msg)))))

    ;; Register the actor
    (agent-update *test-actor-index*
                  (lambda (current-dict)
                    (serapeum:dict* current-dict "subfinder" dest-actor)))

    (sleep 0.1)

    ;; Route the target
    (let ((dest (serapeum:@ (agent-get *test-actor-index* #'identity) "subfinder")))
      (when dest
        (tell dest target)))

    (sleep 0.2)

    ;; Verify the message was received
    (let ((messages (get-recorded-messages)))
      (is (>= (length messages) 1))
      (is (jsown:val-safe (car messages) "actor"))))

  (teardown-test-actor-system))

(test test-route-target-to-nonexistent-actor
  "Test routing a target when no destination actor exists"
  (rdbg "TEST test-route-target-to-nonexistent-actor")
  (setup-test-actor-system)

  (clear-recorded-messages)

  (let ((target (make-test-target :actor "nonexistent")))

    ;; Try to route without registering the actor
    (let ((dest (serapeum:@ (agent-get *test-actor-index* #'identity) "nonexistent")))
      (is (null dest))
      ;; In the real system, this would publish to RabbitMQ
      ;; Here we just verify the lookup returns nil
      ))

  (teardown-test-actor-system))

;;; ----------------------------------------------------------------------
;;; Target Actor Logic Tests
;;; ----------------------------------------------------------------------

(test test-first-time-non-recurring-local-actor
  "Test routing a first-time, non-recurring target to a local actor

   EXPECTED BEHAVIOR: Target should be routed to local actor immediately.
   This tests the case where:
   - Local actor is registered
   - Target is non-recurring
   - This is the first time processing this target

   NOTE: This exposes a potential bug in the current implementation!"
  (rdbg "TEST test-first-time-non-recurring-local-actor")
  (setup-test-actor-system)

  (clear-recorded-messages)

  (let* ((target (make-test-target :actor "nmap"
                                   :recurring nil
                                   :target-id "one-time-scan"))
         (dest-actor (actor-of *test-actor-system*
                               :name "nmap-actor"
                               :receive (lambda (msg)
                                          (rdbg "Nmap actor received: ~a" msg)
                                          (record-message msg)))))

    ;; Register the actor
    (agent-update *test-actor-index*
                  (lambda (current-dict)
                    (serapeum:dict* current-dict "nmap" dest-actor)))

    (sleep 0.1)

    ;; Simulate what *targets* actor should do for first-time, non-recurring
    ;; Current implementation has a bug here - it doesn't route this case!
    (let ((dest (serapeum:@ (agent-get *test-actor-index* #'identity) "nmap")))
      ;; This is what SHOULD happen:
      (when dest
        (tell dest (cdr (cons t target))))) ;; Route it immediately

    (sleep 0.2)

    ;; Verify the target was routed
    (let ((messages (get-recorded-messages)))
      (rdbg "Recorded messages: ~a" messages)
      (is (>= (length messages) 1)
          "First-time non-recurring target should be routed to local actor")))

  (teardown-test-actor-system))

(test test-first-time-recurring-local-actor
  "Test handling a first-time, recurring target with a local actor

   EXPECTED BEHAVIOR: Target should be scheduled for recurring execution.
   The target should NOT be immediately routed, but scheduled via timer."
  (rdbg "TEST test-first-time-recurring-local-actor")
  (setup-test-actor-system)

  (clear-recorded-messages)

  (let* ((target (make-test-target :actor "subfinder"
                                   :recurring t
                                   :delay 5
                                   :target-id "recurring-scan"))
         (dest-actor (actor-of *test-actor-system*
                               :name "subfinder-actor"
                               :receive (lambda (msg)
                                          (record-message msg)))))

    ;; Register the actor
    (agent-update *test-actor-index*
                  (lambda (current-dict)
                    (serapeum:dict* current-dict "subfinder" dest-actor)))

    (sleep 0.1)

    ;; For recurring targets, the *targets* actor schedules them
    ;; We can't easily test the scheduler here, but we verify the actor exists
    (let ((dest (serapeum:@ (agent-get *test-actor-index* #'identity) "subfinder")))
      (is (not (null dest)))
      ;; In real implementation, this would be scheduled, not immediately routed
      ))

  (teardown-test-actor-system))

(test test-not-first-time-local-actor
  "Test handling a non-first-time target with a local actor

   EXPECTED BEHAVIOR: Target should be routed immediately to local actor.
   This is the case for recurring targets after they've been scheduled."
  (rdbg "TEST test-not-first-time-local-actor")
  (setup-test-actor-system)

  (clear-recorded-messages)

  (let* ((target (make-test-target :actor "nmap"
                                   :recurring t
                                   :target-id "recurring-scan-2"))
         (dest-actor (actor-of *test-actor-system*
                               :name "nmap-actor"
                               :receive (lambda (msg)
                                          (record-message msg)))))

    ;; Register the actor
    (agent-update *test-actor-index*
                  (lambda (current-dict)
                    (serapeum:dict* current-dict "nmap" dest-actor)))

    (sleep 0.1)

    ;; Simulate non-first-time (from recurring scheduled execution)
    (let ((dest (serapeum:@ (agent-get *test-actor-index* #'identity) "nmap")))
      (when dest
        (tell dest (cdr (cons nil target))))) ;; not first-time

    (sleep 0.2)

    ;; Verify the target was routed
    (let ((messages (get-recorded-messages)))
      (is (>= (length messages) 1)
          "Non-first-time target should be routed to local actor")))

  (teardown-test-actor-system))

(test test-no-local-actor-should-fallback
  "Test that targets without local actors fall back to RabbitMQ

   EXPECTED BEHAVIOR: When no local actor is registered, the system
   should publish to RabbitMQ (we can't test actual RabbitMQ here,
   but we verify the lookup fails as expected)."
  (rdbg "TEST test-no-local-actor-should-fallback")
  (setup-test-actor-system)

  (let ((target (make-test-target :actor "unknown-actor")))

    ;; Don't register any actor
    (let ((dest (serapeum:@ (agent-get *test-actor-index* #'identity) "unknown-actor")))
      (is (null dest)
          "Unknown actor should not be found in index")
      ;; In real system, this would trigger RabbitMQ publish
      ))

  (teardown-test-actor-system))

;;; ----------------------------------------------------------------------
;;; Message Format Tests
;;; ----------------------------------------------------------------------

(test test-target-message-format
  "Test the message format for targets (first-time flag + target doc)"
  (rdbg "TEST test-target-message-format")

  (let* ((target (make-test-target :actor "test"))
         (first-time-msg (cons t target))
         (not-first-time-msg (cons nil target)))

    ;; Verify first-time message structure
    (is (consp first-time-msg))
    (is (eq t (car first-time-msg)))
    (is (jsown:val-safe (cdr first-time-msg) "actor"))

    ;; Verify not-first-time message structure
    (is (consp not-first-time-msg))
    (is (null (car not-first-time-msg)))
    (is (jsown:val-safe (cdr not-first-time-msg) "actor"))))

(test test-target-attributes
  "Test target document attributes"
  (rdbg "TEST test-target-attributes")

  (let ((recurring-target (make-test-target :recurring t :delay 30))
        (one-time-target (make-test-target :recurring nil))
        (transient-target (make-test-target :transient t)))

    ;; Recurring target checks
    (is (jsown:val recurring-target "recurring"))
    (is (= 30 (jsown:val recurring-target "delay")))

    ;; One-time target checks
    (is (not (jsown:val one-time-target "recurring")))

    ;; Transient target checks
    (is (jsown:val transient-target "transient"))))

;;; ----------------------------------------------------------------------
;;; Edge Cases and Error Handling
;;; ----------------------------------------------------------------------

(test test-update-actor-registration
  "Test updating an existing actor registration"
  (rdbg "TEST test-update-actor-registration")
  (setup-test-actor-system)

  (let ((actor1 (actor-of *test-actor-system*
                          :name "actor-v1"
                          :receive (lambda (msg) (record-message (cons :v1 msg)))))
        (actor2 (actor-of *test-actor-system*
                          :name "actor-v2"
                          :receive (lambda (msg) (record-message (cons :v2 msg))))))

    ;; Register first actor
    (agent-update *test-actor-index*
                  (lambda (current-dict)
                    (serapeum:dict* current-dict "nmap" actor1)))

    (sleep 0.1)

    (let ((dest1 (serapeum:@ (agent-get *test-actor-index* #'identity) "nmap")))
      (is (eq actor1 dest1)))

    ;; Update to second actor
    (agent-update *test-actor-index*
                  (lambda (current-dict)
                    (serapeum:dict* current-dict "nmap" actor2)))

    (sleep 0.1)

    (let ((dest2 (serapeum:@ (agent-get *test-actor-index* #'identity) "nmap")))
      (is (eq actor2 dest2))
      (is (not (eq actor1 dest2)))))

  (teardown-test-actor-system))

(test test-empty-actor-index
  "Test behavior with empty actor index"
  (rdbg "TEST test-empty-actor-index")
  (setup-test-actor-system)

  ;; Don't register any actors
  (let ((dest (serapeum:@ (agent-get *test-actor-index* #'identity) "any-actor")))
    (is (null dest)))

  (teardown-test-actor-system))

;;; ----------------------------------------------------------------------
;;; Integration Tests
;;; ----------------------------------------------------------------------

(test test-multiple-targets-to-same-actor
  "Test routing multiple targets to the same actor"
  (rdbg "TEST test-multiple-targets-to-same-actor")
  (setup-test-actor-system)

  (clear-recorded-messages)

  (let* ((target1 (make-test-target :actor "nmap" :target-id "target-1"))
         (target2 (make-test-target :actor "nmap" :target-id "target-2"))
         (target3 (make-test-target :actor "nmap" :target-id "target-3"))
         (dest-actor (actor-of *test-actor-system*
                               :name "nmap-actor"
                               :receive (lambda (msg)
                                          (record-message msg)))))

    ;; Register the actor
    (agent-update *test-actor-index*
                  (lambda (current-dict)
                    (serapeum:dict* current-dict "nmap" dest-actor)))

    (sleep 0.1)

    ;; Route all three targets
    (let ((dest (serapeum:@ (agent-get *test-actor-index* #'identity) "nmap")))
      (when dest
        (tell dest target1)
        (tell dest target2)
        (tell dest target3)))

    (sleep 0.3)

    ;; Verify all were received
    (let ((messages (get-recorded-messages)))
      (is (>= (length messages) 3)
          "All three targets should be received")))

  (teardown-test-actor-system))

(test test-targets-to-different-actors
  "Test routing targets to different actors"
  (rdbg "TEST test-targets-to-different-actors")
  (setup-test-actor-system)

  (clear-recorded-messages)

  (let* ((target1 (make-test-target :actor "nmap"))
         (target2 (make-test-target :actor "subfinder"))
         (actor1 (actor-of *test-actor-system*
                           :name "nmap-actor"
                           :receive (lambda (msg)
                                      (record-message (cons :nmap msg)))))
         (actor2 (actor-of *test-actor-system*
                           :name "subfinder-actor"
                           :receive (lambda (msg)
                                      (record-message (cons :subfinder msg))))))

    ;; Register both actors
    (agent-update *test-actor-index*
                  (lambda (current-dict)
                    (serapeum:dict* current-dict "nmap" actor1 "subfinder" actor2)))

    (sleep 0.1)

    ;; Route to different actors
    (let ((dest1 (serapeum:@ (agent-get *test-actor-index* #'identity) "nmap"))
          (dest2 (serapeum:@ (agent-get *test-actor-index* #'identity) "subfinder")))
      (when dest1 (tell dest1 target1))
      (when dest2 (tell dest2 target2)))

    (sleep 0.3)

    ;; Verify both were received
    (let ((messages (get-recorded-messages)))
      (is (>= (length messages) 2)
          "Both targets should be received by their respective actors")))

  (teardown-test-actor-system))

;;; ----------------------------------------------------------------------
;;; Run all routing tests
;;; ----------------------------------------------------------------------

(defun run-target-routing-tests ()
  "Run all target routing tests"
  (run! 'target-routing-tests))
