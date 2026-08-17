(in-package :star-server-tests)

(def-suite runtime-lifecycle-tests
  :description "Owned runtime lifecycle and dependency-aware readiness")

(in-suite runtime-lifecycle-tests)

(defun make-running-test-consumer (&optional (name "runtime-test-consumer"))
  (let ((consumer
          (make-instance
           'star.consumers:consumer
           :name name
           :workers 1
           :stream nil)))
    (setf (star.consumers:consumer-state consumer) :running
          (star.consumers:consumer-running-p consumer) t)
    consumer))

(defun make-ready-test-runtime ()
  (let ((consumer (make-running-test-consumer)))
    (star.runtime::%make-star-runtime
     :state :running
     :actor-system t
     :consumers (list consumer)
     :event-consumer consumer
     :http-server t
     :kernel t
     :started-at (get-universal-time))))

(test consumer-stop-joins-only-owned-threads
  (let* ((consumer
           (make-running-test-consumer "owned-thread-test"))
         (owned-thread
           (bt:make-thread
            (lambda ()
              (loop until
                    (eq :stopping
                        (star.consumers:consumer-state consumer))
                    do (sleep 0.01)))
            :name "starintel-owned-test-thread"))
         (unrelated-thread
           (bt:make-thread
            (lambda () (sleep 0.5))
            :name "unrelated-test-thread")))
    (setf (star.consumers:consumer-threads consumer)
          (list owned-thread))
    (unwind-protect
         (progn
           (is (star.consumers::stop-consumer-and-wait
                consumer
                :timeout-seconds 1))
           (is (eq :stopped
                   (star.consumers:consumer-state consumer)))
           (is (not (star.consumers:consumer-running-p consumer)))
           (is (bt:thread-alive-p unrelated-thread)))
      (when (bt:thread-alive-p unrelated-thread)
        (bt:join-thread unrelated-thread)))))

(test runtime-readiness-fails-closed
  (let ((runtime (make-ready-test-runtime)))
    (let ((star.runtime::*couchdb-readiness-probe* (constantly t))
          (star.runtime::*rabbit-readiness-probe* (constantly t)))
      (is (star.runtime:runtime-ready-p runtime)))
    (let ((star.runtime::*couchdb-readiness-probe* (constantly nil))
          (star.runtime::*rabbit-readiness-probe* (constantly t)))
      (is (not (star.runtime:runtime-ready-p runtime))))
    (let ((star.runtime::*couchdb-readiness-probe* (constantly t))
          (star.runtime::*rabbit-readiness-probe* (constantly nil)))
      (is (not (star.runtime:runtime-ready-p runtime))))))

(test runtime-liveness-tracks-lifecycle-state
  (let ((runtime
          (star.runtime::%make-star-runtime
           :state :running
           :started-at (get-universal-time))))
    (is (star.runtime:runtime-live-p runtime))
    (setf (star.runtime:star-runtime-state runtime) :stopping)
    (is (star.runtime:runtime-live-p runtime))
    (setf (star.runtime:star-runtime-state runtime) :stopped)
    (is (not (star.runtime:runtime-live-p runtime)))
    (is (not (star.runtime:runtime-live-p nil)))))

(test operational-health-paths-are-public
  (is (member "/health" star:*auth-public-paths* :test #'string=))
  (is (member "/live" star:*auth-public-paths* :test #'string=))
  (is (member "/ready" star:*auth-public-paths* :test #'string=)))
