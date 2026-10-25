(defpackage :star.router.leases.tests (:use :cl :fiveam))
(in-package :star.router.leases.tests)
(def-suite :router-lease)
(in-suite :router-lease)

(defun owner (&optional (allowed t))
  (star.auth::%make-service-call-context
   :principal-id "server" :principal-type "service" :credential-id "server-key"
   :scopes (if allowed '("targets:dispatch" "targets:lease" "targets:read"
                         "tenant:tenant-a" "dataset:dataset-a" "actor:actor-a" "target:*")
               '("targets:read" "tenant:tenant-a" "dataset:dataset-a"))))
(defun lease-identity (document)
  (star.leases:make-lease-identity
   :tenant-id "tenant-a" :program-id "program-a" :target-namespace "target"
   :target-id (star.documents:document-id document) :actor-name "actor-a"))
(defun dispatch (&key (delay 1) recurring (attempt 0))
  ;; Canonical schema validation executes in parse-target-record.
  (let* ((doc (jsown:new-js ("id" "target-a") ("dtype" "target")
                           ("schemaVersion" "0.10.1") ("dataset" "dataset-a")
                           ("tenant_id" "tenant-a") ("actor" "actor-a")
                           ("target" "example.invalid") ("delay" delay)
                           ("recurring" (if recurring t :false)) ("options" (jsown:empty-object))))
         (record (star.actors::parse-target-record doc)))
    (star.actors::make-target-dispatch-envelope
     record :attempt attempt
     :destination (star.actors::make-target-destination-handle :zmq "actor-a"))))
(defun setup (&key (allowed t) (ttl-ms 60000))
  (let* ((now (* 1000 (- (get-universal-time) 2208988800))) (count 0) (ids 0)
         (store (star.leases:make-memory-lease-store
                 :clock (lambda () now)
                 :id-generator (lambda ()
                                 (if (= 1 (incf ids)) "real-lease"
                                     (format nil "real-lease-~d" ids)))
                 :audit-hook (lambda (&rest ignored) (declare (ignore ignored)) (incf count)))))
    (values (star.router.leases:make-policy store (owner allowed) #'lease-identity "instance-a"
                                           :ttl-ms ttl-ms :clock (lambda () now))
            (lambda () count) (lambda (delta) (incf now delta)))))

(test admission-records-real-fence-and-preserves-one-shot-execution
  (multiple-value-bind (policy audits advance) (setup)
    (declare (ignore audits advance))
    (let* ((dispatch (dispatch :attempt 2))
           (execution (star.actors::target-dispatch-envelope-execution-id dispatch)))
      (star.router.leases:prepare-envelope policy dispatch :reply-budget-ms 30000)
      (let ((lease (star.router.leases:verify-envelope policy dispatch)))
        (is (equal "real-lease" (star.actors::target-dispatch-envelope-lease-id dispatch)))
        (is (= (star.leases:lease-record-fencing-token lease)
               (star.actors::target-dispatch-envelope-fencing-token dispatch)))
        (is (= 3 (jsown:val (star.leases:lease-record-metadata lease) "wireAttempt"))))
      (is (eq dispatch (star.actors::next-target-occurrence-envelope dispatch)))
      (is (equal execution (star.actors::target-dispatch-envelope-execution-id dispatch)))
      (incf (star.actors::target-dispatch-envelope-fencing-token dispatch))
      (signals star.actors::target-destination-unavailable
        (star.router.leases:verify-envelope policy dispatch)))))

(test authorization-precedes-lease-side-effects
  (multiple-value-bind (policy audits advance) (setup :allowed nil)
    (declare (ignore advance))
    (signals star.authorization:authorization-error
      (star.router.leases:prepare-envelope policy (dispatch)))
    (is (zerop (funcall audits)))))

(test recurring-delay-and-deadline-budget-fail-before-acquire
  (multiple-value-bind (policy audits advance) (setup)
    (declare (ignore advance))
    (signals star.actors::target-destination-unavailable
      (star.router.leases:prepare-envelope policy (dispatch :recurring t)))
    (signals star.actors::target-destination-unavailable
      (star.router.leases:prepare-envelope policy (dispatch :delay 40) :reply-budget-ms 30000))
    (let ((dispatch (dispatch)))
      (setf (star.actors::target-dispatch-envelope-deadline dispatch)
            (+ 2 (- (get-universal-time) 2208988800)))
      (signals star.actors::target-destination-unavailable
        (star.router.leases:prepare-envelope policy dispatch :reply-budget-ms 30000)))
    (is (zerop (funcall audits)))))

(test real-expiry-and-idempotent-acquire-cannot-revive-old-execution
  (multiple-value-bind (policy audits advance) (setup)
    (declare (ignore audits))
    (let ((dispatch (dispatch)))
      (star.router.leases:prepare-envelope policy dispatch)
      (funcall advance 60001)
      (signals star.actors::target-destination-unavailable
        (star.router.leases:verify-envelope policy dispatch))
      (signals star.actors::target-destination-unavailable
        (star.router.leases:prepare-envelope policy dispatch)))))

(test rabbit-and-local-occurrences-keep-existing-independent-execution
  (dolist (kind '(:rabbit :local))
    (let ((dispatch (dispatch)))
      (setf (star.actors::target-dispatch-envelope-destination dispatch)
            (star.actors::make-target-destination-handle kind "actor-a"))
      (let ((next (star.actors::next-target-occurrence-envelope dispatch)))
        (is (not (eq next dispatch)))
        (is (not (equal (star.actors::target-dispatch-envelope-execution-id dispatch)
                        (star.actors::target-dispatch-envelope-execution-id next))))
        (is (equal (star.actors::target-dispatch-envelope-lease-id dispatch)
                   (star.actors::target-dispatch-envelope-lease-id next)))))))

(defclass test-lease-binding () ((policy :initarg :policy :reader binding-policy)))
(defmethod star.actors::prepare-zmq-target-envelope ((binding test-lease-binding) dispatch)
  (star.router.leases:prepare-envelope (binding-policy binding) dispatch))

(test acceptance-snapshot-is-created-after-authoritative-acquisition
  (multiple-value-bind (policy audits advance) (setup)
    (declare (ignore audits advance))
    (let* ((star.actors::*active-target-schedules* (make-hash-table :test #'equal))
           (dispatch (dispatch)) (saved nil) (scheduled nil))
      (setf (star.actors::target-dispatch-envelope-destination dispatch)
            (star.actors::make-target-destination-handle
             :zmq "actor-a" :component (make-instance 'test-lease-binding :policy policy)))
      (let ((result
              (star.actors::process-target-dispatch-envelope
               dispatch
               (lambda (row equivalent)
                 (declare (ignore equivalent))
                 (is (equal "real-lease" (jsown:val row "lease_id")))
                 (is (= (star.leases:lease-record-fencing-token
                         (star.router.leases:verify-envelope policy dispatch))
                        (jsown:val row "fencing_token")))
                 (setf saved row) (values row :created))
               (lambda (id update) (declare (ignore id)) (funcall update saved))
               :schedule-once-fn (lambda (id delay callback)
                                   (declare (ignore id delay callback))
                                   (setf scheduled t)))))
        (is (eq :accepted (star.actors::target-dispatch-outcome-status result)))
        (is (not (null saved)))
        (is scheduled)))))

(test active-replay-adopts-original-lease-without-changing-execution
  (multiple-value-bind (policy audits advance) (setup)
    (declare (ignore audits advance))
    (let ((first (dispatch)) (replay (dispatch)))
      (star.router.leases:prepare-envelope policy first)
      (star.router.leases:prepare-envelope policy replay)
      (is (equal (star.actors::target-dispatch-envelope-lease-id first)
                 (star.actors::target-dispatch-envelope-lease-id replay)))
      (is (equal (star.actors::target-dispatch-envelope-trace-id first)
                 (star.actors::target-dispatch-envelope-trace-id replay)))
      (let ((changed (dispatch)))
        (setf (jsown:val (star.actors::target-record-document
                         (star.actors::target-dispatch-envelope-record changed)) "target") "different.invalid")
        (signals star.actors::target-destination-unavailable
          (star.router.leases:prepare-envelope policy changed))))))

(test explicit-expired-recovery-acquires-a-new-fence-without-mutating-predecessor
  (multiple-value-bind (policy audits advance) (setup)
    (declare (ignore audits))
    (let ((original (dispatch)))
      (star.router.leases:prepare-envelope policy original)
      (let ((execution (star.actors::target-dispatch-envelope-execution-id original))
            (fence (star.actors::target-dispatch-envelope-fencing-token original)))
        (funcall advance 60001)
        (let* ((recovered (star.router.leases:prepare-recovery-envelope
                           policy original "persisted-recovery-execution-1"))
               (lease (star.router.leases:verify-envelope policy recovered))
               (replay (star.router.leases:prepare-recovery-envelope
                        policy original "persisted-recovery-execution-1")))
          (is (not (eq original recovered)))
          (is (equal execution (star.actors::target-dispatch-envelope-execution-id original)))
          (is (= fence (star.actors::target-dispatch-envelope-fencing-token original)))
          (is (equal "persisted-recovery-execution-1" (star.leases:lease-record-execution-id lease)))
          (is (> (star.leases:lease-record-fencing-token lease) fence))
          (is (not (equal (star.actors::target-dispatch-envelope-lease-id original)
                          (star.actors::target-dispatch-envelope-lease-id recovered))))
          (is (= 2 (jsown:val (star.leases:lease-record-metadata lease) "wireAttempt")))
          (is (equal (star.actors::target-dispatch-envelope-lease-id recovered)
                     (star.actors::target-dispatch-envelope-lease-id replay)))
          (is (= (star.actors::target-dispatch-envelope-fencing-token recovered)
                 (star.actors::target-dispatch-envelope-fencing-token replay)))
          (signals star.actors::target-destination-unavailable
            (star.router.leases:verify-envelope policy original))
          (funcall advance 60001)
          (signals star.actors::target-destination-unavailable
            (star.router.leases:prepare-recovery-envelope
             policy original "persisted-recovery-execution-1")))))))

(test explicit-recovery-cannot-displace-an-active-predecessor
  (multiple-value-bind (policy audits advance) (setup)
    (declare (ignore audits advance))
    (let ((original (dispatch)))
      (star.router.leases:prepare-envelope policy original)
      (signals star.actors::target-destination-unavailable
        (star.router.leases:prepare-recovery-envelope policy original "new-execution"))
      (is (not (null (star.router.leases:verify-envelope policy original)))))))

(test recovery-requires-explicit-new-identity-and-native-one-shot-before-io
  (multiple-value-bind (policy audits advance) (setup)
    (declare (ignore advance))
    (let ((original (dispatch)))
      (dolist (identity (list nil "" (make-string 1025 :initial-element #\x)
                            (star.actors::target-dispatch-envelope-execution-id original)))
        (signals star.actors::target-destination-unavailable
          (star.router.leases:prepare-recovery-envelope policy original identity)))
      (signals star.actors::target-destination-unavailable
        (star.router.leases:prepare-recovery-envelope policy (dispatch :recurring t) "new-execution"))
      (setf (star.actors::target-dispatch-envelope-destination original)
            (star.actors::make-target-destination-handle :rabbit "actor-a"))
      (signals star.actors::target-destination-unavailable
        (star.router.leases:prepare-recovery-envelope policy original "new-execution"))
      (is (zerop (funcall audits))))))

(test recovery-authorization-still-precedes-lease-io
  (multiple-value-bind (policy audits advance) (setup :allowed nil)
    (declare (ignore advance))
    (signals star.authorization:authorization-error
      (star.router.leases:prepare-recovery-envelope policy (dispatch) "new-execution"))
    (is (zerop (funcall audits)))))
