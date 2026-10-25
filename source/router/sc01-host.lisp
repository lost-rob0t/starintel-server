(defpackage :star.router.sc01-host
  (:use :cl)
  (:export #:start-host #:stop-host #:enqueue-dispatch #:secure-host-p #:snapshot
           #:startup-error #:startup-error-host))
(in-package :star.router.sc01-host)

(define-condition startup-error (star.router:host-error)
  ((host :initarg :host :reader startup-error-host)))

(defstruct (host (:constructor %make-host))
  context api pub binding authenticator secret thread auth-pinned-p
  (queue nil) (queue-limit 64) (timeout-ms 25) (max-payload 1048576)
  (state :starting) stop error
  (rejected 0) (committed 0) (controls 0) (queued 0)
  (lock (bordeaux-threads:make-lock "SC01 owner")))
(defstruct work ticket dispatch context-function)

(defun fail (reason) (error 'star.router:host-error :reason reason))

(defun secure-host-p (host)
  (and (typep host 'host)
       (bordeaux-threads:with-lock-held ((host-lock host))
         (and (eq :running (host-state host)) (not (host-stop host))
              (host-auth-pinned-p host)
              (star.router::authenticator-valid-p
               (host-authenticator host) (host-context host) t)))))

(defun snapshot (host)
  "Safe bounded counters, no document, identity, key, endpoint or exception text."
  (bordeaux-threads:with-lock-held ((host-lock host))
    (list :state (host-state host) :pending (length (host-queue host))
          :rejected (host-rejected host) :committed (host-committed host)
          :controls (host-controls host) :queued (host-queued host))))

(defun copy-dispatch (dispatch)
  (check-type dispatch star.actors::target-dispatch-envelope)
  (let* ((copy (star.actors::copy-target-dispatch-envelope dispatch))
         (document (star.databases.couchdb::clone-outbox-json
                    (star.actors::target-record-document
                     (star.actors::target-dispatch-envelope-record dispatch))))
         (destination (star.actors::copy-target-destination-handle
                       (star.actors::target-dispatch-envelope-destination dispatch))))
    (setf (star.actors::target-dispatch-envelope-record copy)
          (star.actors::parse-target-record document)
          (star.actors::target-destination-handle-name destination)
          (copy-seq (star.actors::target-destination-handle-name destination))
          (star.actors::target-dispatch-envelope-destination copy) destination)
    ;; Snapshot mutable identity strings crossing into the owner queue.
    (macrolet ((copy-identity (accessor)
                 `(when (stringp (,accessor copy))
                    (setf (,accessor copy) (copy-seq (,accessor copy))))))
      (copy-identity star.actors::target-dispatch-envelope-schedule-id)
      (copy-identity star.actors::target-dispatch-envelope-execution-id)
      (copy-identity star.actors::target-dispatch-envelope-trace-id)
      (copy-identity star.actors::target-dispatch-envelope-lease-id)
      (copy-identity star.actors::target-dispatch-envelope-deadline))
    copy))

(defun enqueue-dispatch (host dispatch context-function)
  "Bounded owner handoff. Await with star.router:await-send; :QUEUED is volatile.
CONTEXT-FUNCTION receives the copied existing dispatch on the owner thread and
resolves current trusted dispatch credentials. No retry occurs after timeout."
  (check-type host host)
  (check-type context-function function)
  (when (eq (host-thread host) (bordeaux-threads:current-thread))
    (fail "cannot enqueue blocking dispatch from SC01 owner"))
  ;; Reject known pressure before copying/serializing a potentially large target;
  ;; the second check below closes the admission race between callers.
  (bordeaux-threads:with-lock-held ((host-lock host))
    (unless (and (eq :running (host-state host)) (not (host-stop host)))
      (fail "SC01 host unavailable"))
    (when (>= (length (host-queue host)) (host-queue-limit host))
      (fail "SC01 dispatch queue full")))
  (let* ((copy (copy-dispatch dispatch))
         (bytes (babel:string-to-octets
                 (star.documents:document-json (star.actors::target-dispatch-document copy))
                 :encoding :utf-8))
         (ticket (star.router::%make-ticket)))
    (when (> (length bytes) (host-max-payload host)) (fail "SC01 target exceeds payload limit"))
    (bordeaux-threads:with-lock-held ((host-lock host))
      (unless (and (eq :running (host-state host)) (not (host-stop host)))
        (fail "SC01 host unavailable"))
      (when (>= (length (host-queue host)) (host-queue-limit host))
        (fail "SC01 dispatch queue full"))
      (setf (host-queue host)
            (nconc (host-queue host)
                   (list (make-work :ticket ticket :dispatch copy
                                    :context-function context-function)))))
    ticket))

(defun deliver-work (host router work)
  (let ((ticket (work-ticket work)) (send-p nil))
    (bordeaux-threads:with-lock-held ((star.router::send-ticket-lock ticket))
      (when (eq :pending (star.router::send-ticket-state ticket))
        (setf (star.router::send-ticket-state ticket) :sending send-p t)))
    (when send-p
      (handler-case
          (let ((result (star.router.sc01-application:send-dispatch
                         (host-binding host) router (work-dispatch work)
                         (funcall (work-context-function work) (work-dispatch work)))))
            (star.router::finish-ticket ticket result)
            (bordeaux-threads:with-lock-held ((host-lock host)) (incf (host-queued host))))
        (star.router:router-error (condition)
          (star.router::finish-ticket ticket :failed condition)
          ;; Missing actor/backpressure are request failures; native corruption
          ;; or uncertain multipart send terminates this host, never replays.
          (unless (member (star.router::router-error-status condition) '(1 -7))
            (error condition)))
        (error (condition) (star.router::finish-ticket ticket :failed condition))))))

(defun poll-inbound (host router &optional (process-function #'star.router.sc01-application:process-once))
  (handler-case
      (let ((result (funcall process-function (host-binding host) router)))
        (unless (eq :timeout result)
          (bordeaux-threads:with-lock-held ((host-lock host))
            (if (eq :committed (getf result :status))
                (incf (host-committed host)) (incf (host-controls host))))))
    (star.router:router-error (condition)
      ;; Native receive_message consumed the complete bounded multipart and the
      ;; shared Nim validator rejected it before setting pending. It already
      ;; sent NACK. Only this exact status/operation leaves the broker healthy;
      ;; framing truncation, transport failure and uncertain NACK remain fatal.
      (if (and (= -5 (star.router:router-error-status condition))
               (eq :sc01-receive (star.router:router-error-operation condition)))
          (bordeaux-threads:with-lock-held ((host-lock host))
            (incf (host-rejected host)))
          (error condition)))
    (error ()
      ;; process-once already attempted NACK; never count a successful commit.
      (bordeaux-threads:with-lock-held ((host-lock host))
        (incf (host-rejected host))))))

(defun owner-loop (host)
  (let ((router nil) (closed-p nil))
    (unwind-protect
         (handler-case
             (progn
               (setf router (star.router:open-sc01
                             (host-context host) (host-api host) (host-pub host)
                             :max-payload (host-max-payload host)
                             :timeout-ms (host-timeout-ms host)
                             :server-secret-key (host-secret host)
                             :authenticator (host-authenticator host)))
               (fill (host-secret host) 0)
               (bordeaux-threads:with-lock-held ((host-lock host))
                 (setf (host-state host) :running))
               (loop
                 (when (bordeaux-threads:with-lock-held ((host-lock host)) (host-stop host))
                   (return))
                 (loop repeat 8
                       for work = (bordeaux-threads:with-lock-held ((host-lock host))
                                    (pop (host-queue host)))
                       while work do (deliver-work host router work))
                 (poll-inbound host router)))
           (error (condition) (setf (host-error host) condition)))
      (bordeaux-threads:with-lock-held ((host-lock host)) (setf (host-state host) :stopping))
      (setf closed-p (null router))
      (when router
        (handler-case (progn (star.router:close-sc01 router) (setf closed-p t))
          (error (condition) (setf (host-error host) condition))))
      (when (and closed-p (host-auth-pinned-p host))
        (star.router::unpin-authenticator (host-authenticator host))
        (setf (host-auth-pinned-p host) nil))
      (fill (host-secret host) 0)
      (bordeaux-threads:with-lock-held ((host-lock host))
        (dolist (work (host-queue host))
          (star.router::finish-ticket
           (work-ticket work) :failed
           (make-condition 'star.router:host-error :reason "SC01 host stopped before dispatch")))
        (setf (host-queue host) nil (host-state host) :stopped)))))

(defun start-host (context api-endpoint pub-endpoint binding
                   &key server-secret-key authenticator (queue-limit 64)
                        (timeout-ms 25) (max-payload 1048576))
  "Explicit singleton-CURVE SC01 owner; no actor2actor profile negotiation.
Only this Lisp thread calls the native handle. The native worker never calls Lisp.
On startup timeout STARTUP-ERROR-HOST retains the handle for bounded cleanup."
  (check-type binding star.router.sc01-application::binding)
  (check-type server-secret-key (simple-array (unsigned-byte 8) (32)))
  (star.router::bounded-integer queue-limit 1 4096)
  (star.router::bounded-integer timeout-ms 1 1000)
  (star.router::bounded-integer max-payload 1 1048576)
  (unless (and (star.router::authenticator-valid-p authenticator context t)
               (equalp (star.router:authenticator-single-key authenticator)
                       (star.router.sc01-application::binding-public-key binding)))
    (fail "SC01 host requires this binding's sole authenticated peer"))
  (let ((host (%make-host :context context :api api-endpoint :pub pub-endpoint
                          :binding binding :authenticator authenticator
                          :secret (copy-seq server-secret-key) :queue-limit queue-limit
                          :timeout-ms timeout-ms :max-payload max-payload)))
    (handler-case
        (progn
          (star.router::pin-authenticator authenticator context)
          (setf (host-auth-pinned-p host) t)
          (setf (host-thread host)
                (bordeaux-threads:make-thread (lambda () (owner-loop host))
                                              :name "SC01 application owner")))
      (error (condition)
        (when (host-auth-pinned-p host) (star.router::unpin-authenticator authenticator))
        (fill (host-secret host) 0)
        (error condition)))
    (let ((deadline (+ (get-internal-real-time) (* 5 internal-time-units-per-second))))
      (loop
        (bordeaux-threads:with-lock-held ((host-lock host))
          (when (eq :running (host-state host)) (return-from start-host host))
          (when (eq :stopped (host-state host))
            (error 'startup-error :host host :reason "SC01 startup failed")))
        (when (> (get-internal-real-time) deadline)
          (bordeaux-threads:with-lock-held ((host-lock host)) (setf (host-stop host) t))
          (error 'startup-error :host host :reason "SC01 startup timeout; cleanup pending"))
        (sleep 0.005)))))

(defun stop-host (host &optional (timeout-seconds 2))
  "Bounded graceful stop; timeout leaves owned cleanup pending, not successful."
  (when (eq (host-thread host) (bordeaux-threads:current-thread))
    (fail "cannot join SC01 owner from its handler"))
  (unless (and (realp timeout-seconds) (< 0 timeout-seconds 60)) (fail "invalid shutdown wait"))
  (bordeaux-threads:with-lock-held ((host-lock host)) (setf (host-stop host) t))
  (let ((deadline (+ (get-internal-real-time) (* timeout-seconds internal-time-units-per-second))))
    (loop while (bordeaux-threads:thread-alive-p (host-thread host)) do
      (when (> (get-internal-real-time) deadline) (fail "SC01 cleanup still pending"))
      (sleep 0.005)))
  (bordeaux-threads:join-thread (host-thread host))
  (when (host-error host) (error (host-error host)))
  nil)
