(in-package :star-server-tests)

(in-suite target-routing-tests)

(test stale-lease-cannot-cross-scheduled-target-side-effect-boundary
  "A lease that becomes stale after acceptance must be rejected again before
scheduled local/Rabbit dispatch. The original accepted lease cannot publish a
side effect after successor authority exists."
  (multiple-value-bind (record lease) (make-fenced-dispatch-fixture)
    (let ((star.actors::*active-target-schedules* (make-hash-table :test #'equal))
          (commit-count 0)
          (side-effect-count 0)
          (scheduled-callback nil))
      (let ((outcome
              (star.actors::accept-target-record-with-lease
               record nil lease
               :destination
               (star.actors::make-target-destination-handle
                :local "nmap" :component :fixture-component)
               :commit-fn
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (incf commit-count)
                 (if (= commit-count 1) :committed :stale-token))
               :persist-fn
               (lambda (desired duplicate-predicate)
                 (declare (ignore duplicate-predicate))
                 (values desired :created))
               :update-fn
               (lambda (&rest arguments)
                 (declare (ignore arguments)))
               :dispatch-fn
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (incf side-effect-count)
                 t)
               :schedule-once-fn
               (lambda (schedule-id delay callback)
                 (declare (ignore schedule-id delay))
                 (setf scheduled-callback callback)))))
        (is (eq :accepted (star.actors:target-dispatch-outcome-status outcome)))
        (is (= 1 commit-count))
        (is (functionp scheduled-callback))
        (funcall scheduled-callback)
        (is (= 2 commit-count))
        (is (= 0 side-effect-count))))))
