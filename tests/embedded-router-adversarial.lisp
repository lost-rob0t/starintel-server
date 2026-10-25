;;; Real fixture controls. Markers coordinate threads, never substitute for I/O.
(in-package :embedded-router-application-test)

(defvar *actor-authenticate* nil)
(defvar *actor-revoke* nil)

(defun wait-marker (name &optional (seconds 10))
  (let ((deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop until (probe-file (state-path name))
          do (when (or (probe-file (state-path "stop"))
                       (> (get-internal-real-time) deadline))
               (error "Fixture coordination deadline: ~A" name))
             (sleep 0.01))))

(defun provision-actor-credential ()
  ;; Keep HTTP readback authority independent of the credential being revoked.
  (multiple-value-bind (record key)
      (star.auth:create-api-key
       "embedded-live-actor" "service"
       (list "documents:write" "targets:read" "actor:*" (concatenate 'string "dataset:" *dataset*)
             (concatenate 'string "tenant:" *dataset*)))
    (setf *actor-authenticate*
          (lambda ()
            (let ((star.auth:*request-security-context*
                    (star.auth:authenticate-api-key key "live-actor" (+ (get-universal-time) 60))))
              (star.auth:current-service-call-context)))
          *actor-revoke* (lambda () (star.auth:revoke-api-key (star.auth:api-key-record-id record))))))

(defun pressure-event (suffix)
  (staractorprotocol:make-event-envelope
   :message-id (concatenate 'string *run* ":pressure:" suffix)
   :message-type "star.router.fixture/ready@1" :actor *actor* :sender *actor*
   :dataset *dataset* :payload nil))

(defun actor-pressure (socket manifest)
  (star-zmq:send-frames socket
    (list (star-actor-wire:encode-binding-message :zmq *profiles* manifest (pressure-event "hold"))))
  ;; Exactly eight admitted events must arrive over the actual socket. No
  ;; receipt is inferred from enqueue returning a ticket.
  (dotimes (index 8)
    (let ((reply (star-actor-wire:decode-binding-message
                  :zmq *profiles* manifest (first (star-zmq:receive-frames socket)))))
      (fixture-assert (equal (getf reply :message-id)
                             (getf (pressure-event (write-to-string index)) :message-id))
                      "bounded pressure queue retained FIFO delivery")))
  (wait-marker "pressure-passed"))

(defun prove-pressure (host)
  ;; Owner is held in a bounded fixture callback after real inbound I/O. This
  ;; makes overload deterministic without relying on OS socket buffer sizing.
  (let ((tickets nil) (rejected 0)
        (started (get-internal-real-time)))
    (unwind-protect
         (progn
           (dotimes (index 512)
             (handler-case
                 (push (star.router:enqueue-envelope host *route* (pressure-event (write-to-string index))) tickets)
               (star.router:host-error (condition)
                 (unless (equal "outgoing queue full" (star.router::host-error-reason condition))
                   (error condition))
                 (incf rejected)))
             (fixture-assert (<= (getf (star.router:host-snapshot host) :pending-sends) 8)
                             "queue capacity held under repeated overload"))
           (fixture-assert (= 8 (length tickets)) "exact bounded admissions")
           (fixture-assert (= 504 rejected) "every excess admission rejected")
           (fixture-assert (< (/ (- (get-internal-real-time) started) internal-time-units-per-second) 5)
                           "pressure admissions returned within bound"))
      (marker "pressure-release"))
    (dolist (ticket (nreverse tickets))
      (fixture-assert (eq :queued (star.router:await-send ticket 5)) "pressure transport settlement"))
    (marker "pressure-passed")))

(defun actor-revocation (socket manifest target document command)
  (marker "revoke-requested")
  (wait-marker "credential-revoked")
  (let ((denied (copy-tree document)))
    (setf (cdr (assoc "id" denied :test #'equal)) (concatenate 'string *run* ":revoked"))
    (fixture-assert (eq :error (getf (roundtrip socket manifest
                                              (document-command target denied :suffix "revoked")) :kind))
                    "live revoked credential denied new effect")
    (fixture-assert (eq :error (getf (roundtrip socket manifest command) :kind))
                    "revoked credential denied cached lifecycle replay"))
  (marker "revocation-passed"))

(defun service-adversarial-controls (host)
  (when (and (probe-file (state-path "pressure-entered"))
             (not (probe-file (state-path "pressure-passed"))))
    (prove-pressure host))
  (when (and (probe-file (state-path "revoke-requested"))
             (not (probe-file (state-path "credential-revoked"))))
    (funcall *actor-revoke*)
    (marker "credential-revoked")))
