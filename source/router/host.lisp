(in-package :star.router)

(export '(start-host stop-host enqueue-envelope await-send host-error host-snapshot secure-host-p
          host-startup-error host-startup-error-host host-startup-error-cause))

(define-condition host-error (error)
  ((reason :initarg :reason :reader host-error-reason))
  (:report (lambda (condition stream)
             (format stream "StarRouter host: ~A" (host-error-reason condition)))))

(define-condition host-startup-error (host-error)
  ((host :initarg :host :reader host-startup-error-host)
   (cause :initarg :cause :initform nil :reader host-startup-error-cause)))

(defstruct (send-ticket (:constructor %make-ticket))
  route bytes (state :pending) error
  (lock (bordeaux-threads:make-lock "router send ticket"))
  (changed (bordeaux-threads:make-condition-variable)))

(defstruct (host (:constructor %make-host))
  context endpoint manifest profiles handler thread secret-key secure-p authenticator auth-pinned-p
  (queue nil) (queue-limit 64) (timeout-ms 25)
  (replies nil) (reply-timeout-seconds 2)
  (counters (make-hash-table :test #'eq)) last-failure
  (state :starting) error stop
  (lock (bordeaux-threads:make-lock "router host")))

(defstruct pending-reply route bytes deadline)

(defun host-count (host category &optional failure)
  (bordeaux-threads:with-lock-held ((host-lock host))
    (incf (gethash category (host-counters host) 0))
    (when failure (setf (host-last-failure host) failure))))

(defun host-snapshot (host)
  "Bounded operational counters; no payload, route, principal, key or error text."
  (bordeaux-threads:with-lock-held ((host-lock host))
    (list :state (host-state host) :secure-p (host-secure-p host)
          :pending-sends (length (host-queue host)) :pending-replies (length (host-replies host))
          :last-failure (host-last-failure host)
          :counters (loop for key being the hash-keys of (host-counters host)
                          using (hash-value count) append (list key count)))))

(defun secure-host-p (host &key singleton-peer-p)
  (and (typep host 'host)
       (bordeaux-threads:with-lock-held ((host-lock host))
         (and (host-secure-p host) (host-auth-pinned-p host)
              (eq :running (host-state host)) (not (host-stop host))
              (authenticator-valid-p (host-authenticator host) (host-context host) singleton-peer-p)))))

(defun retain-host-replies (host route replies)
  "Bounded FIFO keeps lifecycle ordering; expiry is observable, never success."
  (when replies
    (let* ((envelopes (if (keywordp (first replies)) (list replies) replies))
           (count (list-length envelopes)))
      (unless (and count (<= count (host-queue-limit host)))
        (host-count host :reply-overflow :reply-overflow)
        (return-from retain-host-replies nil))
      (let ((pending
              (loop for envelope in envelopes collect
                (make-pending-reply
                 :route (copy-seq route)
                 :bytes (star-actor-wire:encode-binding-message
                         :zmq (host-profiles host) (host-manifest host) envelope)
                 :deadline (+ (get-internal-real-time)
                              (* (host-reply-timeout-seconds host) internal-time-units-per-second))))))
        (bordeaux-threads:with-lock-held ((host-lock host))
          (if (> (+ count (length (host-replies host))) (host-queue-limit host))
              (progn (incf (gethash :reply-overflow (host-counters host) 0))
                     (setf (host-last-failure host) :reply-overflow))
              (setf (host-replies host) (nconc (host-replies host) pending))))))))

(defun dispatch-host-message (host route envelope peer-key)
  ;; Application denials and semantic errors affect this message only. Native
  ;; transport exceptions retain their fatal classification in the owner loop.
  (handler-case
      (retain-host-replies host route (funcall (host-handler host) route envelope peer-key))
    (router-error (condition) (error condition))
    (error () (host-count host :handler-rejections :handler-rejected))))

(defun drain-host-replies (host router &optional (send-function #'send-validated-bytes))
  (loop repeat 8
        for reply = (bordeaux-threads:with-lock-held ((host-lock host))
                      (first (host-replies host)))
        while reply do
    (let ((remove-p t))
      (if (>= (get-internal-real-time) (pending-reply-deadline reply))
          (host-count host :reply-expired :reply-expired)
          (handler-case
              (if (eq :would-block
                      (funcall send-function router (pending-reply-route reply) (pending-reply-bytes reply)))
                  (progn (setf remove-p nil) (host-count host :reply-backpressure))
                  (host-count host :replies-queued))
            (router-error (condition)
              (if (= -7 (router-error-status condition))
                  (host-count host :reply-no-route :reply-no-route)
                  (progn (host-count host :reply-fatal :transport-failed) (error condition))))))
      (unless remove-p (return))
      (bordeaux-threads:with-lock-held ((host-lock host)) (pop (host-replies host))))))

(defun finish-ticket (ticket state &optional error)
  (bordeaux-threads:with-lock-held ((send-ticket-lock ticket))
    (setf (send-ticket-state ticket) state (send-ticket-error ticket) error)
    (bordeaux-threads:condition-notify (send-ticket-changed ticket))))

(defun take-ticket (host)
  (bordeaux-threads:with-lock-held ((host-lock host))
    (pop (host-queue host))))

(defun send-validated-bytes (router route bytes)
  (cffi:with-foreign-objects ((r :uint8 (length route)) (b :uint8 (length bytes)))
    (copy-to-foreign route r)
    (copy-to-foreign bytes b)
    (let ((status (%send (checked-owner router) r (length route) b (length bytes))))
      (if (= status 1) :would-block
          (progn (checked status :send) :queued)))))

(defun host-loop (host)
  (let ((router nil))
    (unwind-protect
         (handler-case
             (progn
               (setf router (open-router (host-context host) (host-endpoint host)
                                         :timeout-ms (host-timeout-ms host)
                                         :server-secret-key (host-secret-key host)))
               (when (host-secret-key host) (fill (host-secret-key host) 0))
               (bordeaux-threads:with-lock-held ((host-lock host))
                 (setf (host-state host) :running))
               (loop
                 (when (bordeaux-threads:with-lock-held ((host-lock host)) (host-stop host))
                   (return))
                 ;; A finite send batch preserves receive/cancel progress under load.
                 (loop repeat 8 for ticket = (take-ticket host) while ticket do
                   (bordeaux-threads:with-lock-held ((send-ticket-lock ticket))
                     (when (eq (send-ticket-state ticket) :pending)
                       (setf (send-ticket-state ticket) :sending)))
                   (when (eq (send-ticket-state ticket) :sending)
                     (handler-case
                         (finish-ticket ticket
                           (send-validated-bytes router (send-ticket-route ticket)
                                                 (send-ticket-bytes ticket)))
                       (router-error (condition)
                         (finish-ticket ticket :failed condition)
                         (host-count host :send-failed :send-failed)
                         (unless (= -7 (router-error-status condition)) (error condition)))
                       (error (condition) (finish-ticket ticket :failed condition)
                         (host-count host :send-failed :send-failed)))))
                 (drain-host-replies host router)
                 (handler-case
                     (multiple-value-bind (envelope route status peer-key)
                         (receive-envelope router (host-profiles host) (host-manifest host))
                       (when (eq status :received)
                         ;; The caller-owned host handler is the application boundary.
                         ;; Route identity is never converted to an auth principal here.
                         (dispatch-host-message host route envelope peer-key)))
                   ;; A malformed canonical body has been completely consumed. It
                   ;; cannot invoke the handler, and does not kill healthy peers.
                   (star-actor-wire:wire-error ()
                     (host-count host :invalid-envelopes :invalid-envelope)))))
           (error (condition)
             (bordeaux-threads:with-lock-held ((host-lock host))
               (setf (host-error host) condition))))
      (bordeaux-threads:with-lock-held ((host-lock host)) (setf (host-state host) :stopping))
      (let ((closed-p (null router)))
        (when router
          (handler-case (progn (close-router router) (setf closed-p t))
          (error (condition)
            (bordeaux-threads:with-lock-held ((host-lock host))
                (setf (host-error host) condition)))))
        ;; Never release the authenticator while a native listener may remain.
        (when (and closed-p (host-auth-pinned-p host))
          (unpin-authenticator (host-authenticator host))
          (setf (host-auth-pinned-p host) nil)))
      (when (host-secret-key host) (fill (host-secret-key host) 0))
      (bordeaux-threads:with-lock-held ((host-lock host))
        (dolist (ticket (host-queue host))
          (finish-ticket ticket :failed
                         (make-condition 'host-error :reason "host stopped before send")))
        (incf (gethash :reply-abandoned (host-counters host) 0) (length (host-replies host)))
        (when (host-replies host) (setf (host-last-failure host) :shutdown-with-pending-replies))
        (setf (host-queue host) nil (host-replies host) nil (host-state host) :stopped)))))

(defun start-host (context endpoint manifest handler
                   &key (peer-profiles '("star.actor2actor/1"))
                        (queue-limit 64) (timeout-ms 25) (reply-timeout-seconds 2)
                        server-secret-key authenticator)
  "Start one Lisp socket owner. HANDLER must enforce application authentication.
No foreign thread invokes Lisp. This function does not grant authority to peers.
Secure hosts require an active AUTHENTICATOR for the same context; it remains
pinned until the native listener closes. Outbound application target bindings
require a singleton allowlist; multi-key hosts support inbound commands only.
Construct owner-thread runtime bindings lazily inside HANDLER, on its first call.
Replies retain FIFO ordering in a bounded queue under backpressure. Expired,
unroutable and overflow replies increment HOST-SNAPSHOT counters; none implies
delivery or completion. Callers reconcile canonical lifecycle/document identity."
  (check-type handler function)
  (bounded-integer queue-limit 1 4096)
  (bounded-integer timeout-ms 1 1000)
  (unless (and (realp reply-timeout-seconds) (< 0 reply-timeout-seconds 60))
    (error 'host-error :reason "invalid reply deadline"))
  (when server-secret-key
    (check-type server-secret-key (simple-array (unsigned-byte 8) (32)))
    (unless (authenticator-valid-p authenticator context)
      (error 'host-error :reason "secure host requires an active same-context authenticator")))
  (when (and authenticator (null server-secret-key))
    (error 'host-error :reason "authenticator requires a CURVE server key"))
  (star-actor-wire:negotiate-actor2actor-profile peer-profiles)
  (let ((host (%make-host :context context :endpoint endpoint :manifest manifest
                          :profiles (copy-list peer-profiles) :handler handler
                          :queue-limit queue-limit :timeout-ms timeout-ms
                          :reply-timeout-seconds reply-timeout-seconds
                          :secure-p (not (null server-secret-key))
                          :authenticator authenticator
                          :secret-key (and server-secret-key (copy-seq server-secret-key)))))
    (handler-case
        (progn
          (when authenticator
            (pin-authenticator authenticator context)
            (setf (host-auth-pinned-p host) t))
          (setf (host-thread host)
                (bordeaux-threads:make-thread (lambda () (host-loop host)) :name "embedded StarRouter")))
      (error (condition)
        (when (host-auth-pinned-p host)
          (unpin-authenticator authenticator)
          (setf (host-auth-pinned-p host) nil))
        (when (host-secret-key host) (fill (host-secret-key host) 0))
        (error condition)))
    (let ((deadline (+ (get-internal-real-time) (* 5 internal-time-units-per-second))))
      (loop
        (bordeaux-threads:with-lock-held ((host-lock host))
          (when (eq (host-state host) :running) (return-from start-host host))
          (when (eq (host-state host) :stopped)
            (error 'host-startup-error :host host :cause (host-error host)
                   :reason "startup failed")))
        (when (> (get-internal-real-time) deadline)
          (bordeaux-threads:with-lock-held ((host-lock host)) (setf (host-stop host) t))
          (error 'host-startup-error :host host
                 :reason "startup timeout; cleanup still pending"))
        (sleep 0.005)))))

(defun enqueue-envelope (host route envelope)
  "Validate before bounded admission; returned ticket reports transport queueing only."
  (check-type host host)
  (check-type route (simple-array (unsigned-byte 8) (*)))
  (bounded-integer (length route) 1 255)
  (let ((ticket (%make-ticket
                 :route (copy-seq route)
                 :bytes (star-actor-wire:encode-binding-message
                         :zmq (host-profiles host) (host-manifest host) envelope))))
    (bordeaux-threads:with-lock-held ((host-lock host))
      (unless (and (eq (host-state host) :running) (not (host-stop host)))
        (error 'host-error :reason "host unavailable"))
      (when (>= (length (host-queue host)) (host-queue-limit host))
        (error 'host-error :reason "outgoing queue full"))
      (setf (host-queue host) (nconc (host-queue host) (list ticket))))
    ticket))

(defun await-send (ticket &optional (timeout-seconds 2))
  "Wait for queue acceptance. Timeout cancels pending work; an in-flight send is unknown."
  (unless (and (realp timeout-seconds) (< 0 timeout-seconds 60))
    (error 'host-error :reason "invalid send wait"))
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout-seconds internal-time-units-per-second))))
    (bordeaux-threads:with-lock-held ((send-ticket-lock ticket))
      (loop while (member (send-ticket-state ticket) '(:pending :sending)) do
        (let ((remaining (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
          (when (<= remaining 0)
            (if (eq (send-ticket-state ticket) :pending)
                (progn (setf (send-ticket-state ticket) :cancelled)
                       (error 'host-error :reason "deadline before send"))
                (error 'host-error :reason "send outcome unknown")))
          (bordeaux-threads:condition-wait (send-ticket-changed ticket)
                                          (send-ticket-lock ticket) :timeout remaining)))
      (when (send-ticket-error ticket) (error (send-ticket-error ticket)))
      (send-ticket-state ticket))))

(defun stop-host (host &optional (timeout-seconds 2))
  "Request owner-thread cleanup. A timeout does not claim successful shutdown."
  (when (eq (host-thread host) (bordeaux-threads:current-thread))
    (error 'host-error :reason "cannot join host from its handler"))
  (unless (and (realp timeout-seconds) (< 0 timeout-seconds 60))
    (error 'host-error :reason "invalid shutdown wait"))
  (bordeaux-threads:with-lock-held ((host-lock host)) (setf (host-stop host) t))
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout-seconds internal-time-units-per-second))))
    (loop while (bordeaux-threads:thread-alive-p (host-thread host)) do
      (when (> (get-internal-real-time) deadline)
        (error 'host-error :reason "shutdown pending; owner has not returned"))
      (sleep 0.005)))
  (bordeaux-threads:join-thread (host-thread host))
  (when (host-error host) (error (host-error host)))
  nil)
