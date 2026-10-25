;;; Legacy protocol conformance fixture, loaded after application fixture helpers.
;;; Frame layout follows starRouter/tests/embedded_sc01.py and routercore.nim.
;;; No Actor2Actor lifecycle, cancellation or durable job admission is fabricated.
(in-package :embedded-router-application-test)

(defparameter *sc01-instance* "synthetic-sc01")

(defun sc01-subscriber (context public secret server-public endpoint)
  ;; STAR-ZMQ's public API intentionally exposes ROUTER/DEALER only. This
  ;; test-only SUB uses that binding's checked C ABI and ownership accounting.
  (let ((socket nil))
    (bordeaux-threads:with-lock-held (star-zmq::*context-lock*)
      (let ((handle (star-zmq::%socket (star-zmq::context-handle context) 2)))
        (when (cffi:null-pointer-p handle) (star-zmq::native-error :fixture-sub))
        (setf socket (star-zmq::%make-socket
                      :handle handle :context context :owner (bordeaux-threads:current-thread)
                      :kind :sub :payload-limit 1048576))
        (incf (star-zmq::context-sockets context))))
    (handler-case
        (progn
          (dolist (option '((17 . 0) (23 . 8) (24 . 8) (27 . 15000) (28 . 1000)))
            (star-zmq::set-integer-option socket (car option) (cdr option)))
          (star.router:configure-curve-client socket public secret server-public)
          (star-zmq::with-octets (octets "")
            (lambda (pointer length)
              (star-zmq::checked (star-zmq::%setopt (star-zmq::owned-handle socket) 6 pointer length)
                                 :fixture-subscribe)))
          (star-zmq:connect-endpoint socket endpoint)
          socket)
      (error (condition)
        (star-zmq:close-socket socket)
        (error condition)))))

(defun sc01-frames (socket)
  (let ((buffer (cffi:foreign-alloc :uint8 :count 1048576)) (frames nil))
    (unwind-protect
         (loop repeat 9
               for count = (star-zmq::checked
                            (star-zmq::%recv (star-zmq::owned-handle socket) buffer 1048576 0)
                            :fixture-sc01-receive)
               do (fixture-assert (<= count 1048576) "SC01 fixture frame byte limit")
                  (push (star.router::copy-from-foreign buffer count) frames)
                  (unless (star-zmq::receive-more-p (star-zmq::owned-handle socket))
                    (return-from sc01-frames (nreverse frames)))
               finally (error "SC01 fixture multipart limit exceeded"))
      (cffi:foreign-free buffer))))

(defun sc01-send-event (socket event message-id topic body &key (profile "SC01"))
  (let ((frames (list "" profile *sc01-instance* message-id
                      (write-to-string (- (get-universal-time) 2208988800))
                      (write-to-string event) topic body)))
    (loop for remaining on frames
          do (star-zmq::with-octets (octets (car remaining))
               (lambda (pointer length)
                 (fixture-assert
                  (= length (star-zmq::checked
                             (star-zmq::%send (star-zmq::owned-handle socket) pointer length
                                              (if (cdr remaining) 2 0)) :fixture-sc01-send))
                  "complete SC01 fixture frame sent"))))))

(defun sc01-expect-settlement (socket value)
  (fixture-assert (equalp (sc01-frames socket) (list (octets value)))
                  "SC01 settlement frame"))

(defun sc01-actor (context api pub public secret server-public)
  (let ((dealer nil) (subscriber nil))
    (unwind-protect
         (handler-case
             (progn
               (setf subscriber (sc01-subscriber context public secret server-public pub)
                     dealer (star-zmq:open-socket context :dealer
                                                 :identity (octets *sc01-instance*) :timeout-ms 5000))
               (star.router:configure-curve-client dealer public secret server-public)
               (star-zmq:connect-endpoint dealer api)
               (sc01-send-event dealer 7 (concatenate 'string *run* ":register") *actor* "")
               (sc01-expect-settlement dealer "1")
               ;; Observe a real PUB heartbeat to prove subscription readiness,
               ;; rather than assuming that connect or registration drained it.
               (let ((heartbeat (sc01-frames subscriber)))
                 (fixture-assert (= 6 (length heartbeat)) "SC01 PUB frame count")
                 (fixture-assert (equalp (first heartbeat) (octets "broker")) "SC01 broker heartbeat"))
               (sc01-send-event dealer 0 (concatenate 'string *run* ":heartbeat") *actor* "")
               (sc01-expect-settlement dealer "1")
               (bordeaux-threads:with-lock-held (*lock*) (setf *peer-ready* t))
               (let* ((frames
                        (loop repeat 4 for received = (sc01-frames subscriber)
                              when (equalp (first received) (octets *sc01-instance*)) return received
                              finally (error "SC01 target was not published to the registered instance")))
                      (target (jsown:parse (babel:octets-to-string (sixth frames) :encoding :utf-8)))
                      (document-text (uiop:read-file-string (state-path "document.json")))
                      (document (jsown:parse document-text))
                      (message-id (concatenate 'string *run* ":sc01-document")))
                 (fixture-assert (= 6 (length frames)) "SC01 Target frame count")
                 (fixture-assert (equalp (fifth frames) (octets "3")) "legacy Target is newDocument projection")
                 (fixture-assert (equal (jsown:val target "actor") *actor*) "SC01 Target actor")
                 (fixture-assert (equal (jsown:val target "dataset") *dataset*) "SC01 Target dataset")
                 (fixture-assert (equal (babel:octets-to-string (third frames) :encoding :utf-8)
                                       (jsown:val (jsown:val target "extensions") "target_execution_id"))
                                 "SC01 Target preserves execution identity")
                 (dotimes (attempt 2)
                   (declare (ignorable attempt))
                   (sc01-send-event dealer 3 message-id "documents" document-text)
                   ;; Host emits this ACK only after the shared Server mutation
                   ;; authority returns; Python independently reads stored data.
                   (sc01-expect-settlement dealer "1"))
                 (setf (jsown:val document "dataset") "forbidden"
                       (jsown:val document "id") (concatenate 'string *run* ":denied"))
                 (sc01-send-event dealer 3 (concatenate 'string *run* ":denied") "documents" (jsown:to-json document))
                 (sc01-expect-settlement dealer "2")
                 (sc01-send-event dealer 3 (concatenate 'string *run* ":malformed") "documents" "{}")
                 (sc01-expect-settlement dealer "2")
                 (sc01-send-event dealer 0 (concatenate 'string *run* ":wrong-profile") *actor* ""
                                  :profile "star.actor2actor/1")
                 (sc01-expect-settlement dealer "2")
                 (marker "actor-done"))
               (loop until (probe-file (state-path "stop")) do (sleep 0.05)))
           (error (condition)
             (format *error-output* "~&SC01 actor fixture failed: ~A~%" (type-of condition))
             (marker "actor-error")))
      (when dealer (star-zmq:close-socket dealer))
      (when subscriber (star-zmq:close-socket subscriber)))))

(defun exercise-sc01 (context)
  (let ((api (format nil "tcp://127.0.0.1:~A" (uiop:getenv "STAR_ROUTER_E2E_PORT")))
        (pub (format nil "tcp://127.0.0.1:~A" (uiop:getenv "STAR_ROUTER_E2E_PUB_PORT")))
        (trusted (provision-context)) (authenticator nil) (host nil) (actor-thread nil))
    (multiple-value-bind (server-public server-secret) (star.router:keypair)
      (multiple-value-bind (client-public client-secret) (star.router:keypair)
        (unwind-protect
             (progn
               (setf authenticator (star.router:start-authenticator context (list client-public)))
               (let ((binding (star.router.sc01-application:make-binding
                               client-public *actor* *sc01-instance*
                               (lambda (key) (when (equalp key client-public) trusted)))))
                 (setf host (star.router.sc01-host:start-host
                             context api pub binding :server-secret-key server-secret
                             :authenticator authenticator)))
               (star.router.server:bind-sc01-target-route
                *actor* host (lambda (dispatch) (declare (ignore dispatch)) trusted)
                :lease-policy (fixture-lease-policy trusted))
               (setf actor-thread
                     (bordeaux-threads:make-thread
                      (lambda () (sc01-actor context api pub client-public client-secret server-public))
                      :name "synthetic SC01 actor"))
               (let ((deadline (+ (get-internal-real-time) (* 30 internal-time-units-per-second))))
                 (loop until (bordeaux-threads:with-lock-held (*lock*) *peer-ready*)
                       do (when (or (probe-file (state-path "actor-error"))
                                    (> (get-internal-real-time) deadline))
                            (error "SC01 actor registration/subscription readiness failed"))
                          (sleep 0.01)))
               (marker "ready")
               (loop until (probe-file (state-path "stop")) do (sleep 0.05)))
          (star.router.server:unbind-target-route *actor*)
          (marker "stop")
          (when actor-thread (bordeaux-threads:join-thread actor-thread))
          (when host (star.router.sc01-host:stop-host host))
          (when authenticator (star.router:close-authenticator authenticator)))))))
