(in-package :cl-user)

(defun dispatch-check (condition control &rest arguments)
  (unless condition
    (error (apply #'format nil control arguments))))

(defun dispatch-document
    (id &key (actor "scanner") (target "example.org")
             (delay 30) (recurring nil) schedule-id)
  (let ((document
          (target-recovery-document
           id :actor actor :target target :delay delay :recurring recurring)))
    (when schedule-id
      (setf (jsown:val (jsown:val document "data") "schedule_id") schedule-id))
    document))

(defun dispatch-memory-store ()
  (let ((store (make-hash-table :test #'equal)))
    (values
     store
     (lambda (desired equivalent-p)
       (let* ((id (jsown:val desired "_id"))
              (existing (gethash id store)))
         (cond
           ((null existing)
            (let ((copy
                    (star.databases.couchdb::clone-outbox-json desired)))
              (setf (gethash id store) copy)
              (values copy :created)))
           ((not (funcall equivalent-p existing desired))
            (values existing :conflict))
           ((member (jsown:val existing "status")
                    '("scheduled" "accepted" "dispatched")
                    :test #'string=)
            (values existing :duplicate))
           (t (values existing :resumed)))))
     (lambda (id updater)
       (let ((updated
               (funcall updater
                        (star.databases.couchdb::clone-outbox-json
                         (gethash id store)))))
         (setf (gethash id store) updated)
         updated)))))

(defun dispatch-local-destination (name)
  (star.actors::make-target-destination-handle
   :local name :component (list :component name)))

(defun dispatch-remote-destination (name)
  (star.actors::make-target-destination-handle
   :rabbit name
   :routing-key (star.actors:canonical-target-routing-key name)
   :compatibility-routing-keys
   (star.actors:compatibility-target-routing-keys name)))

(defun test-local-and-remote-recurrence-share-one-scheduler-contract ()
  (clrhash star.actors:*active-target-schedules*)
  (multiple-value-bind (store persist update)
      (dispatch-memory-store)
    (declare (ignore store))
    (let ((scheduled nil)
          (dispatched nil))
      (flet ((schedule-recurring (id delay callback)
               (push (list id delay callback) scheduled))
             (schedule-once (&rest arguments)
               (declare (ignore arguments))
               (error "Recurring target used one-shot scheduler"))
             (dispatch-now (envelope)
               (push
                (star.actors:target-destination-handle-kind
                 (star.actors:target-dispatch-envelope-destination envelope))
                dispatched)
               t))
        (dolist (spec
                 (list
                  (list "target:local" "local-scanner"
                        (dispatch-local-destination "local-scanner"))
                  (list "target:remote" "remote-scanner"
                        (dispatch-remote-destination "remote-scanner"))))
          (let* ((record
                   (star.actors:parse-target-record
                    (dispatch-document
                     (first spec) :actor (second spec)
                     :delay 30 :recurring t)))
                 (outcome
                   (star.actors:process-target-dispatch-envelope
                    (star.actors:make-target-dispatch-envelope
                     record :destination (third spec))
                    persist update
                    :dispatch-fn #'dispatch-now
                    :schedule-once-fn #'schedule-once
                    :schedule-recurring-fn #'schedule-recurring)))
            (dispatch-check
             (eq :accepted
                 (star.actors:target-dispatch-outcome-status outcome))
             "Recurring target outcome was ~s"
             (star.actors:target-dispatch-outcome-status outcome))))
        (dispatch-check (= 2 (length scheduled))
                        "Expected two schedules, got ~d" (length scheduled))
        (dolist (schedule scheduled)
          (dispatch-check (= 30 (second schedule))
                          "Schedule delay drifted")
          (funcall (third schedule)))
        (dispatch-check
         (and (member :local dispatched) (member :rabbit dispatched))
         "Local/remote callbacks diverged: ~s" dispatched)))
    (clrhash star.actors:*active-target-schedules*)))

(defun test-invalid-zero-negative-delays-and-missing-id ()
  (dolist (record
           (list
            (star.actors:parse-target-record
             (dispatch-document "target:zero" :delay 0))
            (star.actors::%make-target-record
             "target:negative" "scanner" "example.org" -1 nil #()
             (dispatch-document "target:negative") "1-neg" nil nil)
            (star.actors::%make-target-record
             "" "scanner" "example.org" 10 nil #()
             (dispatch-document "target:missing") "1-missing" nil nil)))
    (handler-case
        (progn
          (star.actors:make-target-dispatch-envelope
           record :destination (dispatch-local-destination "scanner"))
          (error "Invalid target was accepted"))
      (star.actors:invalid-target-dispatch () t))))

(defun test-duplicate-and-conflicting-schedule-identities ()
  (clrhash star.actors:*active-target-schedules*)
  (multiple-value-bind (store persist update)
      (dispatch-memory-store)
    (declare (ignore store))
    (flet ((schedule-once (&rest arguments)
             (declare (ignore arguments)) t)
           (schedule-recurring (&rest arguments)
             (declare (ignore arguments)) t))
      (let* ((destination (dispatch-local-destination "scanner"))
             (left
               (star.actors:parse-target-record
                (dispatch-document
                 "target:left" :schedule-id "schedule:shared")))
             (same
               (star.actors:parse-target-record
                (dispatch-document
                 "target:left" :schedule-id "schedule:shared")))
             (conflict
               (star.actors:parse-target-record
                (dispatch-document
                 "target:right" :target "other.example"
                 :schedule-id "schedule:shared"))))
        (labels ((run (record)
                   (star.actors:process-target-dispatch-envelope
                    (star.actors:make-target-dispatch-envelope
                     record :destination destination)
                    persist update
                    :schedule-once-fn #'schedule-once
                    :schedule-recurring-fn #'schedule-recurring)))
          (dispatch-check
           (eq :accepted
               (star.actors:target-dispatch-outcome-status (run left)))
           "Initial schedule was not accepted")
          (dispatch-check
           (eq :duplicate
               (star.actors:target-dispatch-outcome-status (run same)))
           "Duplicate schedule was not deduplicated")
          (dispatch-check
           (eq :invalid
               (star.actors:target-dispatch-outcome-status (run conflict)))
           "Conflicting schedule identity was not rejected"))))
    (clrhash star.actors:*active-target-schedules*)))

(defun test-durable-pending-state-resumes-after-scheduler-crash ()
  (clrhash star.actors:*active-target-schedules*)
  (multiple-value-bind (store persist update)
      (dispatch-memory-store)
    (let* ((record
             (star.actors:parse-target-record
              (dispatch-document "target:resume" :delay 15)))
           (destination (dispatch-local-destination "scanner"))
           (first-envelope
             (star.actors:make-target-dispatch-envelope
              record :destination destination :trace-id "trace:durable"))
           (order nil))
      (flet ((ordered-persist (desired predicate)
               (push :persist order)
               (funcall persist desired predicate))
             (fail-schedule (&rest arguments)
               (declare (ignore arguments))
               (push :schedule order)
               (error 'star.actors:target-ingress-overloaded
                      :reason "scheduler full"))
             (good-schedule (&rest arguments)
               (declare (ignore arguments))
               (push :schedule order)
               t)
             (no-recurring (&rest arguments)
               (declare (ignore arguments))
               (error "Unexpected recurring scheduler")))
        (let ((failed
                (star.actors:process-target-dispatch-envelope
                 first-envelope #'ordered-persist update
                 :schedule-once-fn #'fail-schedule
                 :schedule-recurring-fn #'no-recurring)))
          (dispatch-check
           (eq :overloaded
               (star.actors:target-dispatch-outcome-status failed))
           "Scheduler failure was not retryable")
          (dispatch-check (equal (reverse order) '(:persist :schedule))
                          "Acceptance was not durable before scheduling")
          (let* ((id
                   (star.actors:target-acceptance-id
                    (star.actors:target-dispatch-envelope-schedule-id
                     first-envelope)))
                 (pending (gethash id store)))
            (dispatch-check
             (string= "pending" (jsown:val pending "status"))
             "Crash did not leave pending acceptance")))
        (setf order nil)
        (let ((resumed
                (star.actors:process-target-dispatch-envelope
                 (star.actors:make-target-dispatch-envelope
                  record :destination destination :trace-id "trace:new")
                 #'ordered-persist update
                 :schedule-once-fn #'good-schedule
                 :schedule-recurring-fn #'no-recurring)))
          (dispatch-check
           (eq :accepted
               (star.actors:target-dispatch-outcome-status resumed))
           "Pending acceptance did not resume")
          (dispatch-check
           (string= "trace:durable"
                    (star.actors:target-dispatch-envelope-trace-id
                     (star.actors:target-dispatch-outcome-envelope resumed)))
           "Durable trace was not restored"))))
    (clrhash star.actors:*active-target-schedules*)))

(defun test-canonical-remote-route-is-bound-by-consumer ()
  (let* ((expected
           (star.actors:canonical-target-routing-key "remote-scanner"))
         (consumer
           (star.actors:make-remote-target-consumer
            "remote-scanner"
            (lambda (document)
              (declare (ignore document))
              (star.consumers:settlement-ack))))
         (stream (star.consumers:consumer-stream consumer)))
    (dispatch-check
     (string= expected
              (star.consumers:rabbit-stream-routing-key stream))
     "Remote binding and publish route differ")
    (dispatch-check
     (string= expected "documents.target.dispatch.remote-scanner")
     "Canonical target route drifted")
    (dispatch-check
     (member "actors.remote-scanner.new.target"
             (star.actors:compatibility-target-routing-keys
              "remote-scanner")
             :test #'string=)
     "Compatibility routing alias missing")))

(defun test-overloaded-stopped-and-invalid-ingress_settlement ()
  (dolist (entry
           (list
            (cons
             (star.actors::make-target-dispatch-outcome
              :overloaded :reason "mailbox full" :retryable-p t)
             :retry)
            (cons
             (star.actors::make-target-dispatch-outcome
              :unavailable :reason "actor stopped" :retryable-p t)
             :retry)
            (cons
             (star.actors::make-target-dispatch-outcome
              :invalid :reason "invalid target")
             :dead-letter)))
    (dispatch-check
     (eq (cdr entry)
         (star.consumers:consumer-settlement-action
          (star.rabbit::target-outcome-settlement (car entry))))
     "Outcome ~s mapped to wrong settlement"
     (star.actors:target-dispatch-outcome-status (car entry)))))

(defun test-acceptance-record_has_execution_provenance ()
  (let* ((record
           (star.actors:parse-target-record
            (dispatch-document "target:metadata" :delay 12)))
         (envelope
           (star.actors:make-target-dispatch-envelope
            record
            :destination (dispatch-remote-destination "scanner")
            :attempt 3
            :trace-id "trace:metadata"
            :lease-id "lease:metadata"
            :fencing-token 9))
         (document (star.actors:target-acceptance-document envelope)))
    (dispatch-check
     (search "target-execution:" (jsown:val document "execution_id"))
     "Execution id missing")
    (dispatch-check
     (string= "trace:metadata" (jsown:val document "trace_id"))
     "Trace id missing")
    (dispatch-check
     (string= "lease:metadata" (jsown:val document "lease_id"))
     "Lease id missing")
    (dispatch-check (= 9 (jsown:val document "fencing_token"))
                    "Fencing token missing")
    (dispatch-check (= 3 (jsown:val document "attempt"))
                    "Attempt missing")))

(defun run-target-dispatch-acceptance-tests ()
  (format t "~&Running durable target dispatch acceptance tests~%")
  (test-local-and-remote-recurrence-share-one-scheduler-contract)
  (test-invalid-zero-negative-delays-and-missing-id)
  (test-duplicate-and-conflicting-schedule-identities)
  (test-durable-pending-state-resumes-after-scheduler-crash)
  (test-canonical-remote-route-is-bound-by-consumer)
  (test-overloaded-stopped-and-invalid-ingress_settlement)
  (test-acceptance-record_has_execution_provenance)
  (format t "~&Durable target dispatch acceptance tests passed~%")
  t)

(run-target-dispatch-acceptance-tests)
