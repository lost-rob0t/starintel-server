(defpackage :star.router.host.tests (:use :cl :fiveam))
(in-package :star.router.host.tests)
(def-suite :router-host)
(in-suite :router-host)

(defun pending (&optional (deadline (+ (get-internal-real-time)
                                       (* 10 internal-time-units-per-second))))
  (star.router::make-pending-reply
   :route (make-array 1 :element-type '(unsigned-byte 8) :initial-element 1)
   :bytes (make-array 1 :element-type '(unsigned-byte 8) :initial-element 42)
   :deadline deadline))

(test rejection-isolated-and-visible-without-sensitive-errors
  (let* ((deny t) (calls 0)
         (host (star.router::%make-host
                :state :running
                :handler (lambda (&rest ignored)
                           (declare (ignore ignored)) (incf calls)
                           (when deny (error "private credential text")) nil))))
    (star.router::dispatch-host-message host nil nil nil)
    (setf deny nil)
    (star.router::dispatch-host-message host nil nil nil)
    (let ((snapshot (star.router:host-snapshot host)))
      (is (eq :running (getf snapshot :state)))
      (is (eq :handler-rejected (getf snapshot :last-failure)))
      (is (= 1 (getf (getf snapshot :counters) :handler-rejections)))
      (is (= 2 calls))
      (is (null (search "private" (princ-to-string snapshot)))))))

(test reply-backpressure-retains-fifo-until-queued
  ;; Only the external native send port is substituted; test host queue semantics.
  (let* ((first (pending)) (second (pending))
         (host (star.router::%make-host :replies (list first second)))
         (calls 0))
    (star.router::drain-host-replies
     host nil (lambda (&rest ignored) (declare (ignore ignored)) (incf calls) :would-block))
    (is (= 1 calls))
    (is (equal (list first second) (star.router::host-replies host)))
    (star.router::drain-host-replies
     host nil (lambda (&rest ignored) (declare (ignore ignored)) :queued))
    (is (null (star.router::host-replies host)))
    (is (= 2 (getf (getf (star.router:host-snapshot host) :counters) :replies-queued)))))

(test expired-unroutable-and-fatal-send-remain-distinct
  (let ((host (star.router::%make-host :replies (list (pending 0) (pending)))))
    (star.router::drain-host-replies
     host nil (lambda (&rest ignored) (declare (ignore ignored))
                (error 'star.router:router-error :status -7 :operation :send)))
    (let ((counters (getf (star.router:host-snapshot host) :counters)))
      (is (= 1 (getf counters :reply-expired)))
      (is (= 1 (getf counters :reply-no-route)))
      (is (null (getf counters :replies-queued))))
    (setf (star.router::host-replies host) (list (pending)))
    (signals star.router:router-error
      (star.router::drain-host-replies
       host nil (lambda (&rest ignored) (declare (ignore ignored))
                  (error 'star.router:router-error :status -8 :operation :send))))
    (is (= 1 (getf (getf (star.router:host-snapshot host) :counters) :reply-fatal)))))

(test insecure-application-target-binding-denied
  (signals error
    (star.router.server:bind-target-route
     "fixture" (star.router::%make-host :state :running)
     (make-array 1 :element-type '(unsigned-byte 8) :initial-element 1) #'identity)))

(test reply-overflow-is-bounded-and-observable
  (let ((host (star.router::%make-host :queue-limit 1)))
    ;; Reject the batch before encoding/retaining any payload.
    (star.router::retain-host-replies host #(1) '((:kind :reply) (:kind :reply)))
    (is (null (star.router::host-replies host)))
    (is (= 1 (getf (getf (star.router:host-snapshot host) :counters) :reply-overflow)))))

(test authenticator-pins-prevent-close-and-context-substitution
  ;; Exercise ownership guards before any foreign operation, no native mock.
  (let* ((context (gensym "context"))
         (key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 7))
         (auth (star.router::%make-authenticator
                :token 1 :context context :keys (list key)
                :owner (bordeaux-threads:current-thread))))
    (signals star.router:router-error
      (star.router::pin-authenticator auth (gensym "other-context")))
    (star.router::pin-authenticator auth context)
    (signals star.router:router-error (star.router:close-authenticator auth))
    (is (not (star.router::authenticator-closed auth)))
    (is (= 1 (star.router::authenticator-pins auth)))
    (let ((copy (star.router:authenticator-single-key auth)))
      (setf (aref copy 0) 99)
      (is (= 7 (aref (star.router:authenticator-single-key auth) 0))))
    (star.router::unpin-authenticator auth)
    (is (zerop (star.router::authenticator-pins auth)))
    (setf (star.router::authenticator-closed auth) t)
    (signals star.router:router-error (star.router::pin-authenticator auth context))))

(test outbound-target-requires-singleton-active-authenticator
  (let* ((context (gensym "context"))
         (keys (loop for n from 1 to 2 collect
                 (make-array 32 :element-type '(unsigned-byte 8) :initial-element n)))
         (auth (star.router::%make-authenticator
                :token 1 :context context :keys keys :pins 1))
         (host (star.router::%make-host :context context :secure-p t :state :running
                                       :authenticator auth :auth-pinned-p t)))
    (is (star.router:secure-host-p host))
    (is (not (star.router:secure-host-p host :singleton-peer-p t)))
    (signals error
      (star.router.server:bind-target-route
       "fixture" host (make-array 1 :element-type '(unsigned-byte 8) :initial-element 1) #'identity))
    (setf (star.router::authenticator-closed auth) t)
    (is (not (star.router:secure-host-p host)))))
