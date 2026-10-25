(in-package :star.router)
(export '(open-sc01 close-sc01 receive-sc01 settle-sc01 send-sc01-target))

(cffi:defcfun ("sr_sc01_open" %sc01-open) :int
  (context :pointer) (api :string) (pub :string) (limit :uint32)
  (hwm :uint32) (timeout :uint32) (handle :pointer))
(cffi:defcfun ("sr_sc01_open_secure" %sc01-open-secure) :int
  (context :pointer) (api :string) (pub :string) (limit :uint32)
  (hwm :uint32) (timeout :uint32) (secret :pointer) (handle :pointer))
(cffi:defcfun ("sr_sc01_receive" %sc01-receive) :int
  (handle :uint64) (buffer :pointer) (capacity :size) (length :pointer))
(cffi:defcfun ("sr_sc01_settle" %sc01-settle) :int (handle :uint64) (accept :int))
(cffi:defcfun ("sr_sc01_target" %sc01-target) :int
  (handle :uint64) (actor :string) (id :string) (body :pointer) (length :size))
(cffi:defcfun ("sr_sc01_peer_key" %sc01-peer-key) :int (handle :uint64) (key :pointer))
(cffi:defcfun ("sr_sc01_close" %sc01-close) :int (handle :pointer))

(defstruct (sc01-router (:constructor %make-sc01-router))
  handle context owner limit secure pending closed authenticator)

(defun sc01-owner (router)
  (check-type router sc01-router)
  (unless (eq (sc01-router-owner router) (bordeaux-threads:current-thread))
    (error 'router-error :operation :sc01-owner :status -3))
  (when (sc01-router-closed router)
    (error 'router-error :operation :sc01-closed :status -6))
  (sc01-router-handle router))

(defun open-sc01 (context api-endpoint pub-endpoint
                  &key (max-payload 1048576) (high-water-mark 64) (timeout-ms 25)
                       server-secret-key authenticator)
  "Embed the existing Nim SC01 broker with separate explicit profile and host settlement.
SC01 has no actor2actor lifecycle semantics; this function never negotiates fallback.
Secure SC01 requires a singleton AUTHENTICATOR on this context because a PUB
subscriber can select arbitrary topics. The authenticator stays pinned until
the native broker is closed; multi-key authenticated PUB is not supported."
  (bounded-integer max-payload 1 1048576)
  (bounded-integer high-water-mark 1 65536)
  (bounded-integer timeout-ms 1 1000)
  (dolist (endpoint (list api-endpoint pub-endpoint))
    (unless (and (stringp endpoint) (<= 1 (length endpoint) 512)
                 (not (find (code-char 0) endpoint)))
      (error 'router-error :operation :endpoint :status -1)))
  (when server-secret-key
    (check-type server-secret-key (simple-array (unsigned-byte 8) (32)))
    (unless (authenticator-valid-p authenticator context t)
      (error 'router-error :operation :sc01-single-peer-auth :status -1)))
  (when (and authenticator (null server-secret-key))
    (error 'router-error :operation :sc01-auth-without-curve :status -1))
  (ensure-library)
  (when authenticator (pin-authenticator authenticator context))
  (let ((opened nil))
    (unwind-protect
         (prog1
             (bordeaux-threads:with-lock-held (star-zmq::*context-lock*)
    (unless (and (eq context star-zmq::*process-context*) (star-zmq::context-handle context))
      (error 'router-error :operation :context :status -1))
    (cffi:with-foreign-object (handle :uint64)
      (setf (cffi:mem-ref handle :uint64) 0)
      (if server-secret-key
          (cffi:with-foreign-object (secret :uint8 32)
            (unwind-protect
                 (progn
                   (copy-to-foreign server-secret-key secret)
                   (checked (%sc01-open-secure (star-zmq::context-handle context)
                                               api-endpoint pub-endpoint max-payload
                                               high-water-mark timeout-ms secret handle)
                            :sc01-open-secure))
              (dotimes (i 32) (setf (cffi:mem-aref secret :uint8 i) 0))))
          (checked (%sc01-open (star-zmq::context-handle context) api-endpoint pub-endpoint
                               max-payload high-water-mark timeout-ms handle) :sc01-open))
      (incf (star-zmq::context-sockets context))
      (%make-sc01-router :handle (cffi:mem-ref handle :uint64) :context context
                         :owner (bordeaux-threads:current-thread) :limit max-payload
                              :secure (not (null server-secret-key)) :authenticator authenticator)))
           (setf opened t))
      (unless opened (when authenticator (unpin-authenticator authenticator))))))

(defun unpack-sc01-carrier (bytes)
  "Unpack bounded FFI frame lengths; the shared Nim core remains the SC01 codec."
  (let ((offset 0) (frames nil) (size (length bytes)))
    (dotimes (frame 9)
      (declare (ignore frame))
      (when (> (+ offset 4) size)
        (error 'router-error :operation :sc01-carrier :status -5))
      (let ((length (+ (ash (aref bytes offset) 24) (ash (aref bytes (+ offset 1)) 16)
                       (ash (aref bytes (+ offset 2)) 8) (aref bytes (+ offset 3)))))
        (incf offset 4)
        (when (> (+ offset length) size)
          (error 'router-error :operation :sc01-carrier :status -5))
        (push (subseq bytes offset (+ offset length)) frames)
        (incf offset length)))
    (unless (= offset size) (error 'router-error :operation :sc01-carrier :status -5))
    (nreverse frames)))

(defun receive-sc01 (router)
  "Return nine raw frames, authenticated key (or NIL), :RECEIVED; no ACK is sent.
Caller must settle every returned event after authorization and durable effects.
Timeout returns NIL,NIL,:TIMEOUT. No new event is admitted while one is pending."
  (let* ((handle (sc01-owner router)) (capacity (+ (* 9 (sc01-router-limit router)) 36)))
    (when (sc01-router-pending router)
      (error 'router-error :operation :sc01-unsettled :status -1))
    ;; A full nine-frame carrier can exceed the Lisp control-stack size.
    (let ((buffer (cffi:foreign-alloc :uint8 :count capacity)))
      (unwind-protect
           (cffi:with-foreign-object (length :size)
      (setf (cffi:mem-ref length :size) 0)
      (let ((status (%sc01-receive handle buffer capacity length)))
        (when (= status 1) (return-from receive-sc01 (values nil nil :timeout)))
        (checked status :sc01-receive)
        (setf (sc01-router-pending router) t)
        (handler-case
            (let ((n (cffi:mem-ref length :size)))
              (bounded-integer n 36 capacity)
              (values (unpack-sc01-carrier (copy-from-foreign buffer n))
                      (when (sc01-router-secure router)
                        (cffi:with-foreign-object (key :uint8 32)
                          (checked (%sc01-peer-key handle key) :sc01-peer-key)
                          (copy-from-foreign key 32)))
                      :received))
          (error (condition)
            (settle-sc01 router nil)
                 (error condition)))))
        (cffi:foreign-free buffer)))))

(defun settle-sc01 (router accept)
  "ACK only when ACCEPT is literal T; NIL NACKs. Receipt alone never authorizes T."
  (unless (member accept '(nil t)) (error "SC01 settlement requires a Boolean"))
  (let ((handle (sc01-owner router)))
    (unless (sc01-router-pending router)
      (error 'router-error :operation :sc01-no-pending :status -1))
    (checked (%sc01-settle handle (if accept 1 0)) :sc01-settle)
    (setf (sc01-router-pending router) nil))
  nil)

(defun send-sc01-target (router actor id body)
  "Route a trusted, validated Target through the shared Nim registered-actor core.
The returned success is PUB queue acceptance, never durable actor admission."
  (dolist (value (list actor id))
    (unless (and (stringp value) (<= 1 (length value) 255)
                 (not (find (code-char 0) value)))
      (error 'router-error :operation :sc01-target :status -1)))
  (check-type body (simple-array (unsigned-byte 8) (*)))
  (bounded-integer (length body) 1 (sc01-router-limit router))
  (cffi:with-foreign-object (bytes :uint8 (length body))
    (copy-to-foreign body bytes)
    (checked (%sc01-target (sc01-owner router) actor id bytes (length body)) :sc01-target))
  :queued)

(defun close-sc01 (router)
  "Stop/join the native owner before releasing the borrowed context count."
  (check-type router sc01-router)
  (unless (eq (sc01-router-owner router) (bordeaux-threads:current-thread))
    (error 'router-error :operation :sc01-owner :status -3))
  (unless (sc01-router-closed router)
    (let ((handle (sc01-owner router)))
      (cffi:with-foreign-object (token :uint64)
        (setf (cffi:mem-ref token :uint64) handle)
        (checked (%sc01-close token) :sc01-close)))
    (bordeaux-threads:with-lock-held (star-zmq::*context-lock*)
      (decf (star-zmq::context-sockets (sc01-router-context router))))
    (setf (sc01-router-closed router) t (sc01-router-pending router) nil))
    (when (and (sc01-router-closed router) (sc01-router-authenticator router))
      (unpin-authenticator (sc01-router-authenticator router))
      (setf (sc01-router-authenticator router) nil))
  nil)
