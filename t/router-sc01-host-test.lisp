(defpackage :star.router.sc01-host.tests (:use :cl :fiveam))
(in-package :star.router.sc01-host.tests)
(def-suite :router-sc01-host)
(in-suite :router-sc01-host)

(test only-completely-rejected-native-receive-is-recoverable
  ;; Inject receive outcomes only. Exercise the real owner's classification,
  ;; including successful progress after a native schema/profile rejection.
  (let ((host (star.router.sc01-host::%make-host)))
    (star.router.sc01-host::poll-inbound
     host :native-port
     (lambda (binding router)
       (declare (ignore binding router))
       (error 'star.router:router-error :operation :sc01-receive :status -5)))
    (star.router.sc01-host::poll-inbound
     host :native-port
     (lambda (binding router)
       (declare (ignore binding router))
       (list :status :committed :durable-p t)))
    (is (= 1 (getf (star.router.sc01-host:snapshot host) :rejected)))
    (is (= 1 (getf (star.router.sc01-host:snapshot host) :committed)))
    (dolist (failure '((:sc01-receive -6) (:sc01-receive -8)
                       (:sc01-receive -2) (:sc01-settle -5) (:sc01-carrier -5)))
      (signals star.router:router-error
        (star.router.sc01-host::poll-inbound
         host :native-port
         (lambda (binding router)
           (declare (ignore binding router))
           (error 'star.router:router-error
                  :operation (first failure) :status (second failure))))))))

(test queue-pressure-rejects-before-target-copy-or-callback
  (let* ((existing (star.router.sc01-host::make-work))
         (host (star.router.sc01-host::%make-host
                :state :running :queue-limit 1 :queue (list existing))))
    (signals star.router:host-error
      (star.router.sc01-host:enqueue-dispatch
       host :must-not-be-copied
       (lambda (dispatch) (declare (ignore dispatch)) (error "must not resolve credentials"))))
    (is (= 1 (getf (star.router.sc01-host:snapshot host) :pending)))
    (is (eq existing (first (star.router.sc01-host::host-queue host))))))

(test cancelled-queued-dispatch-never-resolves-credentials-or-sends
  (let* ((host (star.router.sc01-host::%make-host))
         (ticket (star.router::%make-ticket :state :cancelled))
         (work (star.router.sc01-host::make-work
                :ticket ticket :dispatch :unused
                :context-function (lambda (dispatch)
                                    (declare (ignore dispatch))
                                    (error "cancelled dispatch invoked credentials")))))
    (star.router.sc01-host::deliver-work host :unused work)
    (is (eq :cancelled (star.router::send-ticket-state ticket)))
    (is (= 0 (getf (star.router.sc01-host:snapshot host) :queued)))))

(test failed-credentials-complete-ticket-without-transport-success
  (let* ((host (star.router.sc01-host::%make-host))
         (ticket (star.router::%make-ticket))
         (work (star.router.sc01-host::make-work
                :ticket ticket :dispatch :unused
                :context-function (lambda (dispatch)
                                    (declare (ignore dispatch))
                                    (error "revoked credential")))))
    (star.router.sc01-host::deliver-work host :unused work)
    (is (eq :failed (star.router::send-ticket-state ticket)))
    (signals error (star.router:await-send ticket))
    (is (= 0 (getf (star.router.sc01-host:snapshot host) :queued)))))

(test real-owner-handoff-completes-only-queue-acceptance
  ;; Replace the external native+application send port here; real loopback
  ;; integration separately exercises it. This proves queue ownership/wakeup.
  (let* ((symbol 'star.router.sc01-application:send-dispatch)
         (original (symbol-function symbol))
         (caller (bordeaux-threads:current-thread))
         (host (star.router.sc01-host::%make-host))
         (ticket (star.router::%make-ticket))
         (thread nil) (calls 0)
         (work (star.router.sc01-host::make-work
                :ticket ticket :dispatch :fixture
                :context-function
                (lambda (dispatch)
                  (unless (and (eq :fixture dispatch)
                               (not (eq caller (bordeaux-threads:current-thread))))
                    (error "dispatch did not run on its owner"))
                  :trusted-fixture-context))))
    (unwind-protect
         (progn
           (setf (symbol-function symbol)
                 (lambda (binding router dispatch context)
                   (declare (ignore binding router dispatch))
                   (unless (eq :trusted-fixture-context context)
                     (error "wrong trusted context"))
                   (incf calls)
                   :queued))
           (setf thread (bordeaux-threads:make-thread
                         (lambda () (star.router.sc01-host::deliver-work host :fake-port work))))
           (is (eq :queued (star.router:await-send ticket)))
           (bordeaux-threads:join-thread thread)
           (setf thread nil)
           (is (= 1 calls)))
      (when thread (bordeaux-threads:join-thread thread))
      (setf (symbol-function symbol) original))))

(test pending-deadline-cancels-but-active-send-is-unknown
  (let ((pending (star.router::%make-ticket))
        (sending (star.router::%make-ticket :state :sending)))
    (signals star.router:host-error (star.router:await-send pending 0.001))
    (is (eq :cancelled (star.router::send-ticket-state pending)))
    (signals star.router:host-error (star.router:await-send sending 0.001))
    (is (eq :sending (star.router::send-ticket-state sending)))))
