;;; Native transport evidence only, not Server persistence/Rabbit E2E.
(require :asdf)
(let ((root (uiop:getenv "STAR_ROUTER_TEST_ROOT"))
      (language (uiop:getenv "STAR_LANG_SOURCE")))
  (unless (and root language) (error "Run scripts/test-embedded-router.sh"))
  (asdf:initialize-source-registry
   `(:source-registry (:tree ,(uiop:ensure-directory-pathname root))
                      (:tree ,(uiop:ensure-directory-pathname language))
                      :inherit-configuration)))
(asdf:load-system "starintel-router")
(asdf:load-system "star-actor-wire/tests")
(asdf:test-system "star-actor-wire")

(defpackage :embedded-router-native-test (:use :cl))
(in-package :embedded-router-native-test)
(cffi:load-foreign-library (uiop:getenv "STAR_ROUTER_LIBRARY"))
(cffi:defcfun ("sr_abi_version" abi-version) :uint32)
(cffi:defcfun ("sr_open" native-open) :int
  (context :pointer) (endpoint :string) (profile :string)
  (limit :uint32) (hwm :uint32) (timeout-ms :uint32) (token :pointer))
(cffi:defcfun ("sr_receive" native-receive) :int
  (token :uint64) (route :pointer) (route-cap :size) (route-len :pointer)
  (body :pointer) (body-cap :size) (body-len :pointer))
(cffi:defcfun ("sr_send" native-send) :int
  (token :uint64) (route :pointer) (route-len :size) (body :pointer) (body-len :size))
(cffi:defcfun ("sr_close" native-close) :int (token :pointer))

(defvar *assertions* 0)
(defun check (value)
  (incf *assertions*)
  (unless value (error "Embedded native assertion ~D failed" *assertions*)))

(defun rejects (type thunk)
  (let ((rejected nil))
    (handler-case (funcall thunk)
      (error (condition)
        (unless (typep condition type) (error condition))
        (setf rejected t)))
    (check rejected)))

(defun wrapper-cycle (context)
  (let* ((endpoint (format nil "tcp://127.0.0.1:~D" (+ 20000 (random 40000))))
         (router (star.router:open-router context endpoint :timeout-ms 20))
         (dealer nil)
         (manifest (star-actor-wire-tests::manifest))
         (command (star-actor-wire-tests::command "wrapper"))
         (profiles '("star.actor2actor/1")))
    (unwind-protect
         (progn
           (rejects 'error (lambda () (star-zmq:close-context context)))
           (rejects 'star-actor-wire:unsupported-peer-protocol
                    (lambda () (star.router:receive-envelope router '("SC01") manifest)))
           (let ((rejected nil))
             (bordeaux-threads:join-thread
              (bordeaux-threads:make-thread
               (lambda ()
                 (handler-case (star.router:close-router router)
                   (star.router:router-error (condition)
                     (setf rejected (= -3 (star.router:router-error-status condition))))))))
             (check rejected))
           (setf dealer (star-zmq:open-socket context :dealer :timeout-ms 1000))
           (star-zmq:connect-endpoint dealer endpoint)
           (star-zmq:send-frames dealer
             (list (star-actor-wire:encode-binding-message :zmq profiles manifest command)))
           (let ((deadline (+ (get-internal-real-time) (* 5 internal-time-units-per-second))))
             (loop
               (multiple-value-bind (received route status)
                   (star.router:receive-envelope router profiles manifest)
                 (when (eq status :received)
                   (check (equal (getf received :message-id) "wrapper"))
                   (check (equal (getf received :correlation-id) (getf command :correlation-id)))
                   (check (eq :queued (star.router:send-envelope router route profiles manifest received)))
                   (check (equalp (first (star-zmq:receive-frames dealer))
                                  (star-actor-wire:encode-envelope manifest command)))
                   (return)))
               (when (> (get-internal-real-time) deadline)
                 (error "Wrapper roundtrip deadline exceeded"))))
           (star-zmq:send-frames dealer (list (babel:string-to-octets "{}" :encoding :utf-8)))
           (let ((deadline (+ (get-internal-real-time) (* 5 internal-time-units-per-second)))
                 (rejected nil))
             (loop until rejected
                   do (handler-case (star.router:receive-envelope router profiles manifest)
                        (star-actor-wire:wire-error () (setf rejected t)))
                      (when (> (get-internal-real-time) deadline)
                        (error "Malformed canonical envelope was not rejected")))
             (check rejected)))
      (when dealer (star-zmq:close-socket dealer))
      (star.router:close-router router)
      (star.router:close-router router))))

(defun receive-bounded (handle route route-cap route-len body body-cap body-len)
  (let ((deadline (+ (get-internal-real-time) (* 5 internal-time-units-per-second))))
    (loop for status = (native-receive handle route route-cap route-len body body-cap body-len)
          unless (= status 1) return status
          do (when (> (get-internal-real-time) deadline)
               (error "Native receive exceeded fixture deadline")))))

(defun run-cycle (context)
  (let ((dealer nil)
        (payload (star-actor-wire:encode-binding-message
                  :zmq '("star.actor2actor/1")
                  (star-actor-wire-tests::manifest)
                  (star-actor-wire-tests::command "embedded-native")))
        (identity (babel:string-to-octets "synthetic-actor" :encoding :utf-8)))
    (cffi:with-foreign-objects ((token :uint64) (route :uint8 255)
                              (body :uint8 1048576) (route-len :size) (body-len :size))
      (setf (cffi:mem-ref token :uint64) 0)
      (unwind-protect
           (progn
             (check (= -4 (native-open (star-zmq::context-handle context)
                                      "tcp://127.0.0.1:1" "SC01" 1048576 4 20 token)))
             (check (zerop (cffi:mem-ref token :uint64)))
             (let ((endpoint nil))
               ;; Retry only bind collisions; no public or alternative transport.
               (loop repeat 8
                     for candidate = (format nil "tcp://127.0.0.1:~D" (+ 20000 (random 40000)))
                     for status = (native-open (star-zmq::context-handle context)
                                               candidate "star.actor2actor/1" 1048576 4 20 token)
                     when (zerop status) do (setf endpoint candidate) (return)
                     unless (= status -2) do (error "Native open failed: ~D" status))
               (unless endpoint (error "Could not bind private native test endpoint"))
               (setf dealer (star-zmq:open-socket context :dealer :identity identity :timeout-ms 1000))
               (star-zmq:connect-endpoint dealer endpoint))
             (let ((handle (cffi:mem-ref token :uint64)))
               (check (= 1 (native-receive handle route 255 route-len body 1048576 body-len)))
               (check (eq :queued (star-zmq:send-frames dealer (list payload))))
               ;; The ABI reports required sizes and retains the message.
               (check (= 2 (receive-bounded handle route 1 route-len body 1 body-len)))
               (check (= (length identity) (cffi:mem-ref route-len :size)))
               (check (= (length payload) (cffi:mem-ref body-len :size)))
               (check (= 0 (native-receive handle route 255 route-len body 1048576 body-len)))
               (check (equalp identity
                              (coerce (loop for i below (length identity)
                                            collect (cffi:mem-aref route :uint8 i))
                                      '(vector (unsigned-byte 8)))))
               (check (= 0 (native-send handle route (length identity) body (length payload))))
               (check (equalp (list payload) (star-zmq:receive-frames dealer)))
               ;; Raw malformed fixture deliberately bypasses the safe sender API.
               (star-zmq::with-octets payload
                 (lambda (pointer length)
                   (check (= length (star-zmq::%send (star-zmq::owned-handle dealer) pointer length 2)))
                   (check (= length (star-zmq::%send (star-zmq::owned-handle dealer) pointer length 0)))))
               (check (= -5 (receive-bounded handle route 255 route-len body 1048576 body-len)))
               (check (= 0 (native-close token)))
               (check (zerop (cffi:mem-ref token :uint64)))
               (check (= 0 (native-close token)))
               (check (= -6 (native-receive handle route 255 route-len body 1048576 body-len)))))
        (when dealer (star-zmq:close-socket dealer))
        (unless (zerop (cffi:mem-ref token :uint64)) (native-close token))))))

(check (= 1 (abi-version)))
(let ((context (star-zmq:make-context)))
  (unwind-protect
       (progn
         (wrapper-cycle context)
         (dotimes (iteration 3) (declare (ignorable iteration)) (run-cycle context)))
    (star-zmq:close-context context)))
(format t "~&NATIVE TRANSPORT ONLY: ~D assertions passed; Server/Rabbit E2E remains separate.~%" *assertions*)
