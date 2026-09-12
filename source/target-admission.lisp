;;;; Target admission control

(in-package :star)

(defparameter *target-max-concurrent-dispatches* nil
  "Maximum target dispatches active inside the server at one time.

NIL disables the concurrency gate.  This limits concurrent server-side dispatch
operations; a RabbitMQ target may continue executing remotely after publication
returns.")

(defparameter *target-rate-limit* nil
  "Optional sliding-window target dispatch limit.

Use a plist of the form =(:count N :per SECONDS)=.  NIL disables this gate.")

(defparameter *target-token-bucket* nil
  "Optional token-bucket target dispatch limit.

Use a plist of the form =(:capacity N :refill-rate TOKENS-PER-SECOND)=.
=:initial-tokens= is optional and defaults to capacity.")

(defparameter *target-admission-condition* nil
  "Optional trusted operator predicate for target dispatch admission.

The function receives =(ENVELOPE SNAPSHOT)= and returns a generalized boolean
plus an optional rejection reason.  It runs while the admission lock is held and
therefore MUST be fast and non-blocking.")

(in-package :star.actors)

(defvar *target-admission-lock* (bt:make-lock "target-admission"))
(defvar *target-admission-active* 0)
(defvar *target-admission-active-by-actor* (make-hash-table :test #'equal))
(defvar *target-admission-rate-events* nil)
(defvar *target-admission-tokens* nil)
(defvar *target-admission-last-refill* nil)

(defstruct (target-admission-ticket
             (:constructor make-target-admission-ticket (actor-name)))
  actor-name)

(defun target-admission-now ()
  (coerce (/ (get-internal-real-time)
             internal-time-units-per-second)
          'double-float))

(defun target-admission-positive-real-p (value)
  (and (realp value) (plusp value)))

(defun validate-target-rate-limit (spec)
  (when spec
    (let ((count (getf spec :count))
          (period (getf spec :per)))
      (unless (and (integerp count) (plusp count))
        (error "Target rate limit :count must be a positive integer, got ~s"
               count))
      (unless (target-admission-positive-real-p period)
        (error "Target rate limit :per must be positive seconds, got ~s"
               period))))
  spec)

(defun validate-target-token-bucket (spec)
  (when spec
    (let* ((capacity (getf spec :capacity))
           (refill-rate (getf spec :refill-rate))
           (initial (getf spec :initial-tokens capacity)))
      (unless (target-admission-positive-real-p capacity)
        (error "Target token bucket :capacity must be positive, got ~s"
               capacity))
      (unless (target-admission-positive-real-p refill-rate)
        (error "Target token bucket :refill-rate must be positive, got ~s"
               refill-rate))
      (unless (and (realp initial)
                   (<= 0 initial capacity))
        (error "Target token bucket :initial-tokens must be between 0 and ~a, got ~s"
               capacity initial))))
  spec)

(defun validate-target-admission-config
    (&key max-concurrent rate-limit token-bucket condition)
  (when max-concurrent
    (unless (and (integerp max-concurrent) (plusp max-concurrent))
      (error "Target :max-concurrent must be a positive integer, got ~s"
             max-concurrent)))
  (validate-target-rate-limit rate-limit)
  (validate-target-token-bucket token-bucket)
  (when condition
    (unless (or (functionp condition)
                (and (symbolp condition) (fboundp condition)))
      (error "Target admission :condition must be a function designator, got ~s"
             condition)))
  t)

(defun target-admission-enabled-p ()
  (or star::*target-max-concurrent-dispatches*
      star::*target-rate-limit*
      star::*target-token-bucket*
      star::*target-admission-condition*))

(defun reset-target-admission-state (&key (now (target-admission-now)))
  (bt:with-lock-held (*target-admission-lock*)
    (setf *target-admission-active* 0
          *target-admission-active-by-actor* (make-hash-table :test #'equal)
          *target-admission-rate-events* nil
          *target-admission-last-refill* now
          *target-admission-tokens*
          (when star::*target-token-bucket*
            (coerce (getf star::*target-token-bucket*
                          :initial-tokens
                          (getf star::*target-token-bucket* :capacity))
                    'double-float))))
  t)

(defun prune-target-rate-events (now)
  (when star::*target-rate-limit*
    (let* ((period (coerce (getf star::*target-rate-limit* :per)
                           'double-float))
           (cutoff (- now period)))
      (setf *target-admission-rate-events*
            (delete-if (lambda (stamp) (<= stamp cutoff))
                       *target-admission-rate-events*)))))

(defun refill-target-token-bucket (now)
  (when star::*target-token-bucket*
    (let* ((capacity (coerce (getf star::*target-token-bucket* :capacity)
                             'double-float))
           (refill-rate (coerce (getf star::*target-token-bucket* :refill-rate)
                                'double-float))
           (last (or *target-admission-last-refill* now))
           (elapsed (max 0d0 (- now last))))
      (setf *target-admission-tokens*
            (min capacity
                 (+ (or *target-admission-tokens* capacity)
                    (* elapsed refill-rate)))
            *target-admission-last-refill* now))))

(defun target-admission-actor-name (envelope actor-name)
  (or actor-name
      (and envelope
           (target-record-actor
            (target-dispatch-envelope-record envelope)))
      "unknown"))

(defun target-admission-snapshot (actor-name now)
  (list :now now
        :actor actor-name
        :active *target-admission-active*
        :actor-active (gethash actor-name *target-admission-active-by-actor* 0)
        :max-concurrent star::*target-max-concurrent-dispatches*
        :rate-count (length *target-admission-rate-events*)
        :rate-limit star::*target-rate-limit*
        :tokens *target-admission-tokens*
        :token-bucket star::*target-token-bucket*))

(defun call-target-admission-condition (envelope snapshot)
  (let ((condition star::*target-admission-condition*))
    (if condition
        (multiple-value-bind (allowed reason)
            (funcall condition envelope snapshot)
          (values (not (null allowed)) reason))
        (values t nil))))

(defun target-admission-reject (format-control &rest arguments)
  (error 'target-ingress-overloaded
         :reason (apply #'format nil format-control arguments)))

(defun target-admission-acquire
    (envelope &key actor-name (now (target-admission-now)))
  "Reserve one dispatch slot or signal =target-ingress-overloaded=.

The reservation covers only the server-side dispatch operation.  Rate and token
charges are consumed on admission even when the destination later fails, because
the dispatch attempt itself consumed capacity."
  (unless (target-admission-enabled-p)
    (return-from target-admission-acquire nil))
  (let ((actor (target-admission-actor-name envelope actor-name)))
    (bt:with-lock-held (*target-admission-lock*)
      (prune-target-rate-events now)
      (refill-target-token-bucket now)
      (multiple-value-bind (allowed reason)
          (call-target-admission-condition
           envelope (target-admission-snapshot actor now))
        (unless allowed
          (target-admission-reject
           "custom admission condition rejected actor ~a~@[ (~a)~]"
           actor reason)))
      (let ((maximum star::*target-max-concurrent-dispatches*))
        (when (and maximum
                   (>= *target-admission-active* maximum))
          (target-admission-reject
           "target concurrency limit reached (~d/~d)"
           *target-admission-active* maximum)))
      (when star::*target-rate-limit*
        (let ((maximum (getf star::*target-rate-limit* :count))
              (period (getf star::*target-rate-limit* :per)))
          (when (>= (length *target-admission-rate-events*) maximum)
            (target-admission-reject
             "target rate limit reached (~d dispatches per ~a seconds)"
             maximum period))))
      (when star::*target-token-bucket*
        (when (< *target-admission-tokens* 1d0)
          (target-admission-reject
           "target token bucket empty (~,3f tokens available)"
           *target-admission-tokens*)))
      (incf *target-admission-active*)
      (incf (gethash actor *target-admission-active-by-actor* 0))
      (when star::*target-rate-limit*
        (push now *target-admission-rate-events*))
      (when star::*target-token-bucket*
        (decf *target-admission-tokens* 1d0))
      (make-target-admission-ticket actor))))

(defun target-admission-release (ticket)
  (when ticket
    (bt:with-lock-held (*target-admission-lock*)
      (setf *target-admission-active* (max 0 (1- *target-admission-active*)))
      (let* ((actor (target-admission-ticket-actor-name ticket))
             (active (gethash actor *target-admission-active-by-actor* 0)))
        (if (> active 1)
            (setf (gethash actor *target-admission-active-by-actor*) (1- active))
            (remhash actor *target-admission-active-by-actor*)))))
  t)

(defun current-target-admission-state (&key (now (target-admission-now)))
  (bt:with-lock-held (*target-admission-lock*)
    (prune-target-rate-events now)
    (refill-target-token-bucket now)
    (list :enabled (not (null (target-admission-enabled-p)))
          :active *target-admission-active*
          :active-by-actor
          (loop for actor being the hash-keys of *target-admission-active-by-actor*
                  using (hash-value count)
                collect (cons actor count))
          :max-concurrent star::*target-max-concurrent-dispatches*
          :rate-count (length *target-admission-rate-events*)
          :rate-limit star::*target-rate-limit*
          :tokens *target-admission-tokens*
          :token-bucket star::*target-token-bucket*
          :condition (not (null star::*target-admission-condition*)))))

(in-package :star)

(defun configure-target-admission
    (&key max-concurrent rate-limit token-bucket condition)
  "Configure target dispatch admission from =init.lisp=.

All configured gates are ANDed: a target must pass every enabled gate.

Examples:
  (configure-target-admission :max-concurrent 8)
  (configure-target-admission :rate-limit '(:count 60 :per 60))
  (configure-target-admission
   :token-bucket '(:capacity 20 :refill-rate 2))
  (configure-target-admission
   :condition (lambda (envelope snapshot) ...))"
  (star.actors::validate-target-admission-config
   :max-concurrent max-concurrent
   :rate-limit rate-limit
   :token-bucket token-bucket
   :condition condition)
  (setf *target-max-concurrent-dispatches* max-concurrent
        *target-rate-limit* rate-limit
        *target-token-bucket* token-bucket
        *target-admission-condition* condition)
  (star.actors::reset-target-admission-state)
  (star.actors::current-target-admission-state))

(defun target-admission-state ()
  "Return a snapshot of target admission state for operator introspection."
  (star.actors::current-target-admission-state))

(in-package :star.actors)

(defun dispatch-target-envelope-now
    (envelope &key
                (local-send-fn
                  (lambda (component payload)
                    (tell component payload)))
                (remote-send-fn
                  (lambda (routing-key document)
                    (star.rabbit:emit-document
                     "documents" routing-key document))))
  "Dispatch one occurrence through admission control and its destination handle."
  (let ((ticket (target-admission-acquire envelope)))
    (unwind-protect
         (let ((destination (target-dispatch-envelope-destination envelope)))
           (handler-case
               (ecase (target-destination-handle-kind destination)
                 (:local
                  (unless (target-destination-handle-component destination)
                    (error 'target-destination-unavailable
                           :reason "local component handle is missing"))
                  (funcall local-send-fn
                           (target-destination-handle-component destination)
                           envelope))
                 (:rabbit
                  (funcall remote-send-fn
                           (target-destination-handle-routing-key destination)
                           (target-dispatch-document envelope))))
             (error (condition)
               (error (classify-target-dispatch-condition condition)))))
      (target-admission-release ticket)))
  t)
