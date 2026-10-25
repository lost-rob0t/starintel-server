(defpackage :star.router
  (:use :cl)
  (:export #:open-router #:close-router #:with-router
           #:send-envelope #:receive-envelope
           #:router-error #:router-error-status #:router-error-operation))
(in-package :star.router)

;; Optional system: loading this file neither loads native code nor binds a socket.
;; All operations, including close, belong to the thread that called OPEN-ROUTER.
;; The embedding ABI does not call Lisp, start a collector, or acknowledge storage.
(cffi:define-foreign-library libstarrouter
  (:unix (:or "libstarrouter.so"))
  (t (:default "libstarrouter")))
(cffi:defcfun ("sr_abi_version" %abi-version) :uint32)
(cffi:defcfun ("sr_open" %open) :int
  (context :pointer) (endpoint :string) (profile :string)
  (max-payload :uint32) (high-water-mark :uint32) (timeout-ms :uint32)
  (handle :pointer))
(cffi:defcfun ("sr_open_secure" %open-secure) :int
  (context :pointer) (endpoint :string) (profile :string)
  (max-payload :uint32) (high-water-mark :uint32) (timeout-ms :uint32)
  (secret :pointer) (handle :pointer))
(cffi:defcfun ("sr_peer_key" %peer-key) :int (handle :uint64) (key :pointer))
(cffi:defcfun ("sr_receive" %receive) :int
  (handle :uint64) (route :pointer) (route-capacity :size) (route-length :pointer)
  (body :pointer) (body-capacity :size) (body-length :pointer))
(cffi:defcfun ("sr_send" %send) :int
  (handle :uint64) (route :pointer) (route-length :size)
  (body :pointer) (body-length :size))
(cffi:defcfun ("sr_close" %close) :int (handle :pointer))

(define-condition router-error (error)
  ((status :initarg :status :reader router-error-status)
   (operation :initarg :operation :reader router-error-operation))
  (:report (lambda (condition stream)
             (format stream "StarRouter ~A failed (status ~D)."
                     (router-error-operation condition)
                     (router-error-status condition)))))

(defvar *library-lock* (bordeaux-threads:make-lock "StarRouter library"))
(defvar *library* nil)
(defstruct (router (:constructor %make-router))
  (handle 0) context owner (max-payload 1048576) closed secure)

(defun checked (status operation)
  (unless (zerop status)
    (error 'router-error :status status :operation operation)))

(defun ensure-library ()
  (bordeaux-threads:with-lock-held (*library-lock*)
    (unless *library*
      (let ((library (cffi:load-foreign-library
                      (or (uiop:getenv "STAR_ROUTER_LIBRARY") 'libstarrouter))))
        (unless (= 1 (%abi-version))
          (cffi:close-foreign-library library)
          (error 'router-error :operation :abi-version :status -4))
        (setf *library* library)))))

(defun checked-owner (router)
  (check-type router router)
  (unless (eq (router-owner router) (bordeaux-threads:current-thread))
    (error 'router-error :operation :owner :status -3))
  (when (router-closed router)
    (error 'router-error :operation :closed :status -6))
  (router-handle router))

(defun bounded-integer (value minimum maximum)
  (unless (and (integerp value) (<= minimum value maximum))
    (error 'router-error :operation :configuration :status -1))
  value)

(defun open-router (context endpoint &key (profile "star.actor2actor/1")
                                          (max-payload 1048576)
                                          (high-water-mark 64) (timeout-ms 50)
                                          server-secret-key)
  "Borrow an existing STAR-ZMQ context; caller closes it after this router.
Only the native ABI's explicit supported profile is accepted. This is a transport
boundary, not an authenticated Server listener or durable ingestion endpoint."
  (unless (equal profile "star.actor2actor/1")
    (error 'star-actor-wire:unsupported-peer-protocol))
  (unless (and (stringp endpoint) (<= 1 (length endpoint) 512)
               (not (find (code-char 0) endpoint)))
    (error 'router-error :operation :endpoint :status -1))
  (bounded-integer max-payload 1 1048576)
  (bounded-integer high-water-mark 1 65536)
  (bounded-integer timeout-ms 1 1000)
  (when server-secret-key
    (check-type server-secret-key (simple-array (unsigned-byte 8) (32))))
  (ensure-library)
  ;; Pinned StarLang context resource accounting: prevent termination while a
  ;; borrowed native socket is live. No second context or libzmq binding is made.
  ;; This private seam must be replaced by a public borrow API before promotion.
  (bordeaux-threads:with-lock-held (star-zmq::*context-lock*)
    (unless (and (typep context 'star-zmq::context)
                 (eq context star-zmq::*process-context*)
                 (star-zmq::context-handle context))
      (error 'router-error :operation :context :status -1))
    (cffi:with-foreign-object (handle :uint64)
      (setf (cffi:mem-ref handle :uint64) 0)
      (if server-secret-key
          (cffi:with-foreign-object (secret :uint8 32)
            (unwind-protect
                 (progn
                   (copy-to-foreign server-secret-key secret)
                   (checked (%open-secure (star-zmq::context-handle context)
                                          endpoint profile max-payload high-water-mark
                                          timeout-ms secret handle) :open-secure))
              (dotimes (i 32) (setf (cffi:mem-aref secret :uint8 i) 0))))
          (checked (%open (star-zmq::context-handle context) endpoint profile
                          max-payload high-water-mark timeout-ms handle) :open))
      (incf (star-zmq::context-sockets context))
      (%make-router :handle (cffi:mem-ref handle :uint64) :context context
                    :owner (bordeaux-threads:current-thread)
                    :max-payload max-payload :secure (not (null server-secret-key))))))

(defun close-router (router)
  "Close on the owner thread. Repeated close is harmless; context remains owned by caller."
  (check-type router router)
  (unless (eq (router-owner router) (bordeaux-threads:current-thread))
    (error 'router-error :operation :owner :status -3))
  (unless (router-closed router)
    (cffi:with-foreign-object (handle :uint64)
      (setf (cffi:mem-ref handle :uint64) (router-handle router))
      (checked (%close handle) :close))
    (bordeaux-threads:with-lock-held (star-zmq::*context-lock*)
      (decf (star-zmq::context-sockets (router-context router))))
    (setf (router-closed router) t (router-handle router) 0))
  nil)

(defmacro with-router ((name context endpoint &rest options) &body body)
  `(let ((,name (open-router ,context ,endpoint ,@options)))
     (unwind-protect (progn ,@body) (close-router ,name))))

(defun copy-to-foreign (bytes pointer)
  (dotimes (index (length bytes))
    (setf (cffi:mem-aref pointer :uint8 index) (aref bytes index))))

(defun copy-from-foreign (pointer count)
  (let ((bytes (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (index count bytes)
      (setf (aref bytes index) (cffi:mem-aref pointer :uint8 index)))))

(defun send-envelope (router route peer-profiles manifest envelope)
  "Send canonical lifecycle bytes. Return :QUEUED or :WOULD-BLOCK, never :DURABLE.
ROUTE is opaque transport metadata, never a principal or authorization decision."
  (let* ((handle (checked-owner router))
         (body (star-actor-wire:encode-binding-message
                :zmq peer-profiles manifest envelope)))
    (check-type route (simple-array (unsigned-byte 8) (*)))
    (bounded-integer (length route) 1 255)
    (bounded-integer (length body) 1 (router-max-payload router))
    (cffi:with-foreign-objects ((route-buffer :uint8 (length route))
                               (body-buffer :uint8 (length body)))
      (copy-to-foreign route route-buffer)
      (copy-to-foreign body body-buffer)
      (let ((status (%send handle route-buffer (length route)
                           body-buffer (length body))))
        (if (= status 1) :would-block
            (progn (checked status :send) :queued))))))

(defun receive-envelope (router peer-profiles manifest)
  "Return canonical ENVELOPE, opaque ROUTE, and :RECEIVED; or NIL,NIL,:TIMEOUT.
No application handler is invoked. Caller must authenticate/authorize before
dispatch and execute lifecycle semantics in the canonical runtime."
  (let ((handle (checked-owner router)) (limit (router-max-payload router)))
    ;; Capability mismatch is rejected before reading a native message.
    (star-actor-wire:negotiate-actor2actor-profile peer-profiles)
    (cffi:with-foreign-objects ((route :uint8 255) (body :uint8 limit)
                               (route-length :size) (body-length :size))
      (setf (cffi:mem-ref route-length :size) 0
            (cffi:mem-ref body-length :size) 0)
      (let ((status (%receive handle route 255 route-length body limit body-length)))
        (when (= status 1)
          (return-from receive-envelope (values nil nil :timeout)))
        (checked status :receive)
        (let ((rn (cffi:mem-ref route-length :size))
              (bn (cffi:mem-ref body-length :size)))
          (bounded-integer rn 1 255)
          (bounded-integer bn 1 limit)
          (values
           (star-actor-wire:decode-binding-message
            :zmq peer-profiles manifest (copy-from-foreign body bn))
           (copy-from-foreign route rn) :received
           (when (router-secure router)
             (cffi:with-foreign-object (key :uint8 32)
               (checked (%peer-key handle key) :peer-key)
               (copy-from-foreign key 32)))))))))
