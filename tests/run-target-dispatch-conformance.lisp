(in-package :cl-user)

(defun target-dispatch-check (condition control &rest arguments)
  (unless condition
    (error (apply #'format nil control arguments))))

(defun target-dispatch-document
    (id &key (actor "scanner") (target "example.org")
             (delay 30) (recurring nil) schedule-id transient deadline
             (revision "1-dispatch"))
  (let ((document
          (target-recovery-document
           id
           :actor actor
           :target target
           :delay delay
           :recurring recurring
           :revision revision)))
    (when schedule-id
      (setf (jsown:val (jsown:val document "data") "schedule_id") schedule-id))
    (when deadline
      (setf (jsown:val (jsown:val document "data") "deadline") deadline))
    (when transient
      (let ((extensions (jsown:empty-object)))
        (setf (jsown:val extensions "transient") :true
              (jsown:val document "extensions") extensions)))
    document))

(defun make-target-memory-store ()
  (let ((store (make-hash-table :test #'equal)))
    (values
     store
     (lambda (desired duplicate-predicate)
       (let* ((id (jsown:val desired "_id"))
              (existing (gethash id store)))
         (cond
           ((null existing)
            (let ((saved
                    (star.databases.couchdb::clone-outbox-json desired)))
              (setf (gethash id store) saved)
              (values saved :created)))
           ((not (funcall duplicate-predicate existing desired))
            (values existing :conflict))
           ((member (jsown:val existing "status")
                    '("accepted" "scheduled" "dispatched")
                    :test #'string=)
            (values existing :duplicate))
           (t
            (values existing :resumed)))))
     (lambda (acceptance-id updater)
       (let* ((current
                (or (gethash acceptance-id store)
                    (error "Missing acceptance ~a" acceptance-id)))
              (updated
                (funcall updater
                         (star.databases.couchdb::clone-outbox-json current))))
         (setf (gethash acceptance-id store) updated)
         updated)))))

(defun local-target-destination (name)
  (star.actors::make-target-destination-handle
   :local name :component (list :component name)))

(defun remote-target-destination (name)
  (star.actors::make-target-destination-handle
   :rabbit name
   :routing-key (star.actors:canonical-target-routing-key name)
   :compatibility-routing-keys
   (star.actors:compatibility-target-routing-keys name)))

(defun test_local_and_remote_recurring_targets_share_schedule_semantics ()
  (clrhash star.actors:*active-target-schedules*)
  (multiple-value-bind (store persist update)
      (make-target-memory-store)
    (declare (ignore store))
    (let ((scheduled nil)
          (dispatched nil))
      (flet ((schedule-recurring (schedule-id delay callback)
               (push (list schedule-id delay callback) scheduled))
             (schedule-once (&rest arguments)
               (declare (ignore arguments))
               (error "Recurring target used one-shot scheduler"))
             (dispatch (envelope)
               (push
                (star.actors:target-destination-handle-kind
                 (star.actors:target-dispatch-envelope-destination envelope))
                dispatched)
               t))
        (dolist (entry
                 (list
                  (list
                   (star.actors:parse-target-record
                    (target-dispatch-document
                     "target:local-repeat"
                     :actor "local-scanner"
                     :recurring t
                     :delay 30))
                   (local-target-destination "local-scanner"))
                  (list
                   (star.actors:parse-target-record
                    (target-dispatch-document
                     "target:remote-repeat"
                     :actor "remote-scanner"
                     :recurring t
                     :delay 30))
                   (remote-target-destination "remote-scanner"))))
          (let* ((envelope
                   (star.actors:make-target-dispatch-envelope
                    (first entry) :destination (second entry)))
                 (outcome
                   (star.actors:process-target-dispatch-envelope
                    envelope persist update
                    :dispatch-fn #'dispatch
                    :schedule-once-fn #'schedule-once
                    :schedule-recurring-fn #'schedule-recurring)))
            (target-dispatch-check
             (eq :accepted
                 (star.actors:target-dispatch-outcome-status outcome))
             "Recurring target was not accepted: ~s"
             (star.actors:target-dispatch-outcome-status outcome))))
        (target-dispatch-check (= 2 (length scheduled))
                               "Expected two schedules, got ~d"
                               (length scheduled))
        (dolist (schedule scheduled)
          (target-dispatch-check (= 30 (second schedule))
                                 "Schedule delay drifted: ~s" schedule)
          (funcall (third schedule)))
        (target-dispatch-check
         (equal (sort (copy-list dispatched) #'string< :key #'symbol-name)
                '(:LOCAL :RABBIT))
         "Local/remote callbacks diverged: ~s" dispatched)))
    (clrhash star.actors:*active-target-schedules*)))

(defun test_invalid_delays_and_missing_ids_are_rejected ()
  (dolist (delay '(0 -1))
    (handler-case
        (let ((record
                (if (minusp delay)
                    (star.actors::%make-target-record
                     "target:negative" "scanner" "example.org" delay nil #()
                     (target-dispatch-document "target:negative")
                     "1-negative" nil nil)
                    (star.actors:parse-target-record
                     (target-dispatch-document "target:zero" :delay delay)))))
          (star.actors:make-target-dispatch-envelope
           record :destination (local-target-destination "scanner"))
          (error "Delay ~d was accepted" delay))
      (star.actors:invalid-target-dispatch () t)))
  (handler-case
      (progn
        (star.actors:make-target-dispatch-envelope
         (star.actors::%make-target-record
          "" "scanner" "example.org" 10 nil #()
          (target-dispatch-document "target:missing")
          "1-missing" nil nil)
         :destination (local-target-destination "scanner"))
        (error "Missing target id was accepted"))
    (star.actors:invalid-target-dispatch () t)))

(defun test_duplicate_active_schedule_is_rejected_idempotently ()
  (clrhash star.actors:*active-target-schedules*)
  (multiple-value-bind (store persist update)
      (make-target-memory-store)
    (declare (ignore store))
    (let* ((record
             (star.actors:parse-target-record
              (target-dispatch-document
               "target:duplicate" :delay 20 :schedule-id "schedule:shared")))
           (destination (local-target-destination "scanner"))
           (scheduled 0))
      (flet ((schedule-once (schedule-id delay callback)
               (declare (ignore schedule-id delay callback))
               (incf scheduled))
             (schedule-recurring (&rest arguments)
               (declare (ignore arguments))
               (error "One-shot target used recurring scheduler")))
        (let ((first
                (star.actors:process-target-dispatch-envelope
                 (star.actors:make-target-dispatch-envelope
                  record :destination destination)
                 persist update
                 :schedule-once-fn #'schedule-once
                 :schedule-recurring-fn #'schedule-recurring))
              (second
                (star.actors:process-target-dispatch-envelope
                 (star.actors:make-target-dispatch-envelope
                  record :destination destination)
                 persist update
                 :schedule-once-fn #'schedule-once
                 :schedule-recurring-fn #'schedule-recurring)))
          (target-dispatch-check
           (eq :accepted (star.actors:target-dispatch-outcome-status first))
           "First schedule was not accepted")
          (target-dispatch-check
           (eq :duplicate (star.actors:target-dispatch-outcome-status second))
           "Duplicate schedule was not deduplicated")
          (target-dispatch-check (= 1 scheduled)
                                 "Duplicate created ~d schedules" scheduled))))
    (clrhash star.actors:*active-target-schedules*)))

(defun test_conflicting_schedule_identity_is_invalid ()
  (clrhash star.actors:*active-target-schedules*)
  (multiple-value-bind (store persist update)
      (make-target-memory-store)
    (declare (ignore store))
    (flet ((schedule-once (&rest arguments)
             (declare (ignore arguments)) t)
           (schedule-recurring (&rest arguments)
             (declare (ignore arguments)) t))
      (let* ((left
               (star.actors:parse-target-record
                (target-dispatch-document
                 "target:left" :target "left.example"
                 :schedule-id "schedule:collision")))
             (right
               (star.actors:parse-target-record
                (target-dispatch-document
                 "target:right" :target "right.example"
                 :schedule-id "schedule:collision")))
             (destination (local-target-destination "scanner")))
        (star.actors:process-target-dispatch-envelope
         (star.actors:make-target-dispatch-envelope
          left :destination destination)
         persist update
         :schedule-once-fn #'schedule-once
         :schedule-recurring-fn #'schedule-recurring)
        (let ((outcome
                (star.actors:process-target-dispatch-envelope
                 (star.actors:make-target-dispatch-envelope
                  right :destination destination)
                 persist update
                 :schedule-once-fn #'schedule-once
                 :schedule-recurring-fn #'schedule-recurring)))
          (target-dispatch-check
           (eq :invalid (star.actors:target-dispatch-outcome-status outcome))
           "Conflicting schedule identity was not rejected"))))
    (clrhash star.actors:*active-target-schedules*)))

(defun test_pending_acceptance_resumes_after_schedule_failure ()
  (clrhash star.actors:*active-target-schedules*)
  (multiple-value-bind (store persist update)
      (make-target-memory-store)
    (let* ((record
             (star.actors:parse-target-record
              (target-dispatch-document "target:resume" :delay 15)))
           (destination (local-target-destination "scanner"))
           (envelope
             (star.actors:make-target-dispatch-envelope
              record :destination destination :trace-id "trace:stable"))
           (order nil))
      (flet ((persist-ordered (desired predicate)
               (push :persist order)
               (funcall persist desired predicate))
             (fail-schedule (&rest arguments)
               (declare (ignore arguments))
               (push :schedule order)
               (error 'star.actors:target-ingress-overloaded
                      :reason "scheduler saturated"))
             (good-schedule (&rest arguments)
               (declare (ignore arguments))
               (push :schedule order)
               t)
             (never-recurring (&rest arguments)
               (declare (ignore arguments))
               (error "Unexpected recurring schedule")))
        (let ((failed
                (star.actors:process-target-dispatch-envelope
                 envelope #'persist-ordered update
                 :schedule-once-fn #'fail-schedule
                 :schedule-recurring-fn #'never-recurring)))
          (target-dispatch-check
           (eq :overloaded
               (star.actors:target-dispatch-outcome-status failed))
           "Schedule saturation was not retryable")
          (target-dispatch-check
           (equal (reverse order) '(:persist :schedule))
           "Scheduling occurred before durable acceptance: ~s" order)
          (let* ((acceptance-id
                   (star.actors:target-acceptance-id
                    (star.actors:target-dispatch-envelope-schedule-id envelope)))
                 (pending (gethash acceptance-id store)))
            (target-dispatch-check
             (string= "pending" (jsown:val pending "status"))
             "Failed schedule did not leave resumable pending state")))
        (setf order nil)
        (let ((resumed
                (star.actors:process-target-dispatch-envelope
                 (star.actors:make-target-dispatch-envelope
                  record :destination destination :trace-id "trace:new")
                 #'persist-ordered update
                 :schedule-once-fn #'good-schedule
                 :schedule-recurring-fn #'never-recurring)))
          (target-dispatch-check
           (eq :accepted
               (star.actors:target-dispatch-outcome-status resumed))
           "Pending target did not resume")
          (target-dispatch-check
           (string= "trace:stable"
                    (star.actors:target-dispatch-envelope-trace-id
                     (star.actors:target-dispatch-outcome-envelope resumed)))
           "Resume lost durable trace metadata"))))
    (clrhash star.actors:*active-target-schedules*)))

(defun test_canonical_remote_binding_matches_publish_route ()
  (let* ((actor "remote-scanner")
         (expected (star.actors:canonical-target-routing-key actor))
         (consumer
           (star.actors:make-remote-target-consumer
            actor (lambda (document)
                    (declare (ignore document))
                    (star.consumers:settlement-ack))))
         (stream (star.consumers:consumer-stream consumer)))
    (target-dispatch-check
     (string= expected
              (star.consumers:rabbit-stream-routing-key stream))
     "Remote consumer binding does not match canonical route")
    (target-dispatch-check
     (string= expected "documents.target.dispatch.remote-scanner")
     "Canonical route drifted: ~a" expected)
    (target-dispatch-check
     (member "actors.remote-scanner.new.target"
             (star.actors:compatibility-target-routing-keys actor)
             :test #'string=)
     "Compatibility alias missing")))

(defun test_overloaded_and_invalid_outcomes_map_to_retry_or_quarantine ()
  (let ((overloaded
          (star.actors::make-target-dispatch-outcome
           :overloaded :reason "mailbox full" :retryable-p t))
        (invalid
          (star.actors::make-target-dispatch-outcome
           :invalid :reason "bad target")))
    (target-dispatch-check
     (eq :retry
         (star.consumers:consumer-settlement-action
          (star.rabbit::target-outcome-settlement overloaded)))
     "Overloaded ingress did not retry")
    (target-dispatch-check
     (eq :dead-letter
         (star.consumers:consumer-settlement-action
          (star.rabbit::target-outcome-settlement invalid)))
     "Invalid ingress did not quarantine")))

(defun test_acceptance_contains_execution_lease_and_fencing_metadata ()
  (let* ((record
           (star.actors:parse-target-record
            (target-dispatch-document "target:metadata" :delay 12)))
         (envelope
           (star.actors:make-target-dispatch-envelope
            record
            :destination (remote-target-destination "scanner")
            :attempt 3
            :trace-id "trace:metadata"
            :lease-id "lease:metadata"
            :fencing-token 9))
         (document (star.actors:target-acceptance-document envelope)))
    (target-dispatch-check
     (string= "trace:metadata" (jsown:val document "trace_id"))
     "Trace id missing from acceptance")
    (target-dispatch-check
     (string= "lease:metadata" (jsown:val document "lease_id"))
     "Lease id missing from acceptance")
    (target-dispatch-check (= 9 (jsown:val document "fencing_token"))
                           "Fencing token missing")
    (target-dispatch-check (= 3 (jsown:val document "attempt"))
                           "Attempt missing")
    (target-dispatch-check
     (target-dispatch-check
      (search "target-execution:"
              (jsown:val document "execution_id"))
      "Execution id missing")
     "Execution metadata assertion failed")))

(defun run-target-dispatch-conformance-tests ()
  (format t "~&Running durable target dispatch tests~%")
  (test_local_and_remote_recurring_targets_share_schedule_semantics)
  (test_invalid_delays_and_missing_ids_are_rejected)
  (test_duplicate_active_schedule_is_rejected_idempotently)
  (test_conflicting_schedule_identity_is_invalid)
  (test_pending_acceptance_resumes_after_schedule_failure)
  (test_canonical_remote_binding_matches_publish_route)
  (test_overloaded_and_invalid_outcomes_map_to_retry_or_quarantine)
  (test_acceptance_contains_execution_lease_and_fencing_metadata)
  (format t "~&Durable target dispatch tests passed~%")
  t)

(run-target-dispatch-conformance-tests)
