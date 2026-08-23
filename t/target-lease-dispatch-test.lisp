(in-package :star-server-tests)

(in-suite target-routing-tests)

(defun make-fenced-dispatch-fixture ()
  (let* ((document
           (jsown:new-js
             ("_id" "target-lease-fixture")
             ("type" "target")
             ("actor" "nmap")
             ("target" "target-lease-fixture")
             ("delay" 1)
             ("recurring" :false)))
         (record
           (star.actors::%make-target-record
            "target-lease-fixture" "nmap" "target-lease-fixture"
            1 nil #() document nil nil nil))
         (identity
           (star.leases:make-lease-identity
            :tenant-id "tenant-a"
            :program-id "program-a"
            :target-namespace "targets"
            :target-id "target-lease-fixture"
            :actor-name "nmap"))
         (lease
           (star.leases:make-lease-record
            :lock-key (star.leases:canonical-target-lock-key identity)
            :identity identity
            :lease-id "lease-fixture"
            :owner-principal-id "principal-a"
            :owner-client-id "client-a"
            :owner-credential-id "credential-a"
            :service-instance-id "server-a"
            :fencing-token 7
            :acquired-at 1
            :renewed-at 1
            :expires-at 9999999999999
            :ttl-ms 30000
            :maximum-lifetime-ms 300000
            :execution-id "execution-a"
            :job-id "job-a"
            :trace-id "trace-a"
            :request-id "request-a"
            :metadata (jsown:empty-object)
            :state :active)))
    (values record lease)))

(test unfenced-target-acceptance-does-not-fabricate-authority
  "Compatibility ingress must fail closed instead of inventing lease authority."
  (multiple-value-bind (record lease) (make-fenced-dispatch-fixture)
    (declare (ignore lease))
    (let ((persist-count 0))
      (let ((outcome
              (star.actors::accept-target-record
               record
               :destination
               (star.actors::make-target-destination-handle
                :rabbit "nmap" :routing-key "documents.target.dispatch.nmap")
               :persist-fn
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (incf persist-count)
                 (error "unfenced target reached persistence"))
               :update-fn (lambda (&rest arguments) (declare (ignore arguments)))
               :schedule-once-fn
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (error "unfenced target reached scheduling")))))
        (is (eq :invalid (star.actors:target-dispatch-outcome-status outcome)))
        (is (= 0 persist-count))
        (is (search "authoritative lease"
                    (star.actors:target-dispatch-outcome-reason outcome)))))))

(test stale-lease-cannot-persist-target-acceptance
  "The fencing decision must happen before CouchDB acceptance or scheduling."
  (multiple-value-bind (record lease) (make-fenced-dispatch-fixture)
    (let ((persist-count 0)
          (schedule-count 0))
      (let ((outcome
              (star.actors::accept-target-record-with-lease
               record nil lease
               :destination
               (star.actors::make-target-destination-handle
                :rabbit "nmap" :routing-key "documents.target.dispatch.nmap")
               :commit-fn
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 :stale-token)
               :persist-fn
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (incf persist-count)
                 (error "stale lease reached persistence"))
               :update-fn (lambda (&rest arguments) (declare (ignore arguments)))
               :schedule-once-fn
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (incf schedule-count)))))
        (is (eq :invalid (star.actors:target-dispatch-outcome-status outcome)))
        (is (= 0 persist-count))
        (is (= 0 schedule-count))))))

(test current-lease-commits-before-target-acceptance
  "A current fenced lease may cross the acceptance boundary with its real token."
  (multiple-value-bind (record lease) (make-fenced-dispatch-fixture)
    (let ((star.actors::*active-target-schedules* (make-hash-table :test #'equal))
          (persist-count 0)
          (schedule-count 0))
      (let* ((outcome
               (star.actors::accept-target-record-with-lease
                record nil lease
                :destination
                (star.actors::make-target-destination-handle
                 :rabbit "nmap" :routing-key "documents.target.dispatch.nmap")
                :commit-fn
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  :committed)
                :persist-fn
                (lambda (desired duplicate-predicate)
                  (declare (ignore duplicate-predicate))
                  (incf persist-count)
                  (values desired :created))
                :update-fn
                (lambda (acceptance-id updater)
                  (declare (ignore acceptance-id updater)))
                :schedule-once-fn
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (incf schedule-count))))
             (envelope (star.actors:target-dispatch-outcome-envelope outcome)))
        (is (eq :accepted (star.actors:target-dispatch-outcome-status outcome)))
        (is (= 1 persist-count))
        (is (= 1 schedule-count))
        (is (string= "lease-fixture"
                     (star.actors:target-dispatch-envelope-lease-id envelope)))
        (is (= 7 (star.actors:target-dispatch-envelope-fencing-token envelope)))))))

(test stale-locator-is-rejected-before-fenced-commit
  "Caller lease locators must be resolved through trusted server authority first."
  (multiple-value-bind (record lease) (make-fenced-dispatch-fixture)
    (declare (ignore lease))
    (let ((commit-count 0)
          (persist-count 0)
          (service (star.authorization::%make-target-lease-service nil nil "server-a")))
      (let ((outcome
              (star.actors::accept-target-record-with-authority
               record service nil "lease-stale" 6
               :authority-fn
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (star.authorization::target-lease-service-result :stale-token))
               :commit-fn
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (incf commit-count)
                 :committed)
               :persist-fn
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (incf persist-count)
                 (error "stale locator reached persistence")))))
        (is (eq :invalid (star.actors:target-dispatch-outcome-status outcome)))
        (is (= 0 commit-count))
        (is (= 0 persist-count))
        (is (search "STALE-TOKEN"
                    (string-upcase
                     (star.actors:target-dispatch-outcome-reason outcome))))))))

(test trusted-authority-record-drives-fenced-target-acceptance
  "A valid locator is replaced by the server-owned lease record before commit."
  (multiple-value-bind (record lease) (make-fenced-dispatch-fixture)
    (let ((star.actors::*active-target-schedules* (make-hash-table :test #'equal))
          (resolved-lease-id nil)
          (resolved-token nil)
          (persist-count 0)
          (service (star.authorization::%make-target-lease-service nil nil "server-a")))
      (let* ((outcome
               (star.actors::accept-target-record-with-authority
                record service nil "caller-lease-locator" 7
                :destination
                (star.actors::make-target-destination-handle
                 :rabbit "nmap" :routing-key "documents.target.dispatch.nmap")
                :authority-fn
                (lambda (resolved-service context lease-id fencing-token)
                  (declare (ignore context))
                  (is (eq service resolved-service))
                  (is (string= "caller-lease-locator" lease-id))
                  (is (= 7 fencing-token))
                  (star.authorization::target-lease-service-result
                   :found :lease lease))
                :commit-fn
                (lambda (store identity committed-lease intent-id intent-value
                         &key deadline request-id)
                  (declare (ignore store identity intent-id intent-value deadline request-id))
                  (setf resolved-lease-id
                        (star.leases:lease-record-lease-id committed-lease)
                        resolved-token
                        (star.leases:lease-record-fencing-token committed-lease))
                  :committed)
                :persist-fn
                (lambda (desired duplicate-predicate)
                  (declare (ignore duplicate-predicate))
                  (incf persist-count)
                  (values desired :created))
                :update-fn
                (lambda (acceptance-id updater)
                  (declare (ignore acceptance-id updater)))
                :schedule-once-fn
                (lambda (&rest arguments)
                  (declare (ignore arguments)))))
             (envelope (star.actors:target-dispatch-outcome-envelope outcome)))
        (is (eq :accepted (star.actors:target-dispatch-outcome-status outcome)))
        (is (= 1 persist-count))
        (is (string= "lease-fixture" resolved-lease-id))
        (is (= 7 resolved-token))
        (is (string= "lease-fixture"
                     (star.actors:target-dispatch-envelope-lease-id envelope)))
        (is (= 7 (star.actors:target-dispatch-envelope-fencing-token envelope)))))))