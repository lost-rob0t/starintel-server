(in-package :star-server-tests)

(def-suite target-admission-tests
  :description "Hermetic target admission policy tests")

(in-suite target-admission-tests)

(defmacro with-clean-target-admission (&body body)
  `(unwind-protect
       (progn
         (star::configure-target-admission)
         ,@body)
     (star::configure-target-admission)))

(defun release-target-admission-ticket (ticket)
  (when ticket
    (star.actors::target-admission-release ticket)))

(test target-admission-max-concurrent
  (with-clean-target-admission
    (star::configure-target-admission :max-concurrent 1)
    (let ((first
            (star.actors::target-admission-acquire
             nil :actor-name "actor-a" :now 0d0)))
      (unwind-protect
           (signals star.actors:target-ingress-overloaded
             (star.actors::target-admission-acquire
              nil :actor-name "actor-b" :now 0d0))
        (release-target-admission-ticket first)))
    (let ((next
            (star.actors::target-admission-acquire
             nil :actor-name "actor-b" :now 0d0)))
      (is (not (null next)))
      (release-target-admission-ticket next))))

(test target-admission-sliding-window-rate-limit
  (with-clean-target-admission
    (star::configure-target-admission
     :rate-limit '(:count 2 :per 10))
    (star.actors::reset-target-admission-state :now 0d0)
    (dotimes (index 2)
      (let ((ticket
              (star.actors::target-admission-acquire
               nil :actor-name "rate-actor" :now (coerce index 'double-float))))
        (release-target-admission-ticket ticket)))
    (signals star.actors:target-ingress-overloaded
      (star.actors::target-admission-acquire
       nil :actor-name "rate-actor" :now 2d0))
    (let ((ticket
            (star.actors::target-admission-acquire
             nil :actor-name "rate-actor" :now 11d0)))
      (is (not (null ticket)))
      (release-target-admission-ticket ticket))))

(test target-admission-token-bucket
  (with-clean-target-admission
    (star::configure-target-admission
     :token-bucket '(:capacity 2 :refill-rate 1 :initial-tokens 2))
    (star.actors::reset-target-admission-state :now 0d0)
    (dotimes (index 2)
      (declare (ignore index))
      (let ((ticket
              (star.actors::target-admission-acquire
               nil :actor-name "bucket-actor" :now 0d0)))
        (release-target-admission-ticket ticket)))
    (signals star.actors:target-ingress-overloaded
      (star.actors::target-admission-acquire
       nil :actor-name "bucket-actor" :now 0d0))
    (let ((ticket
            (star.actors::target-admission-acquire
             nil :actor-name "bucket-actor" :now 1d0)))
      (is (not (null ticket)))
      (release-target-admission-ticket ticket))))

(test target-admission-custom-condition
  (with-clean-target-admission
    (star::configure-target-admission
     :condition
     (lambda (envelope snapshot)
       (declare (ignore envelope))
       (values (zerop (getf snapshot :actor-active))
               "only one dispatch for this actor")))
    (let ((first
            (star.actors::target-admission-acquire
             nil :actor-name "custom-actor" :now 0d0)))
      (unwind-protect
           (signals star.actors:target-ingress-overloaded
             (star.actors::target-admission-acquire
              nil :actor-name "custom-actor" :now 0d0))
        (release-target-admission-ticket first)))
    (let ((other
            (star.actors::target-admission-acquire
             nil :actor-name "other-actor" :now 0d0)))
      (is (not (null other)))
      (release-target-admission-ticket other))))

(test target-admission-composes-policies
  (with-clean-target-admission
    (star::configure-target-admission
     :max-concurrent 2
     :rate-limit '(:count 3 :per 60)
     :token-bucket '(:capacity 3 :refill-rate 1))
    (star.actors::reset-target-admission-state :now 0d0)
    (let ((first
            (star.actors::target-admission-acquire
             nil :actor-name "a" :now 0d0))
          (second
            (star.actors::target-admission-acquire
             nil :actor-name "b" :now 0d0)))
      (unwind-protect
           (progn
             (signals star.actors:target-ingress-overloaded
               (star.actors::target-admission-acquire
                nil :actor-name "c" :now 0d0))
             (is (= 2 (getf (star::target-admission-state) :active))))
        (release-target-admission-ticket first)
        (release-target-admission-ticket second)))))
