(in-package :star.router)
(export '(keypair configure-curve-client start-authenticator close-authenticator authenticator-single-key))

(cffi:defcfun ("sr_auth_start" %auth-start) :int
  (context :pointer) (keys :pointer) (count :size) (token :pointer))
(cffi:defcfun ("sr_auth_close" %auth-close) :int (token :pointer))
(cffi:defcfun ("zmq_curve_keypair" %curve-keypair) :int
  (public :pointer) (secret :pointer))
(cffi:defcfun ("zmq_z85_decode" %z85-decode) :pointer
  (destination :pointer) (text :pointer))

(defstruct (authenticator (:constructor %make-authenticator))
  token (context nil :read-only t) owner closed
  ;; Owned snapshot, no exported reader or mutator; never reuse caller vectors.
  (keys nil :read-only t) (pins 0)
  (lock (bordeaux-threads:make-lock "router authenticator")))

(defun authenticator-valid-p (auth context &optional singleton-p)
  (and (typep auth 'authenticator)
       (bordeaux-threads:with-lock-held ((authenticator-lock auth))
         (and (not (authenticator-closed auth))
              (integerp (authenticator-token auth)) (plusp (authenticator-token auth))
              (eq context (authenticator-context auth))
              (or (not singleton-p) (= 1 (length (authenticator-keys auth))))))))

(defun pin-authenticator (auth context)
  (check-type auth authenticator)
  (bordeaux-threads:with-lock-held ((authenticator-lock auth))
    (unless (and (not (authenticator-closed auth))
                 (integerp (authenticator-token auth)) (plusp (authenticator-token auth))
                 (eq context (authenticator-context auth)))
      (error 'router-error :operation :auth-pin :status -1))
    (incf (authenticator-pins auth)))
  auth)

(defun unpin-authenticator (auth)
  (bordeaux-threads:with-lock-held ((authenticator-lock auth))
    (unless (plusp (authenticator-pins auth))
      (error 'router-error :operation :auth-pin :status -1))
    (decf (authenticator-pins auth)))
  nil)

(defun authenticator-single-key (auth)
  "Return a defensive copy of the only allowed public key, or fail closed."
  (check-type auth authenticator)
  (bordeaux-threads:with-lock-held ((authenticator-lock auth))
    (unless (and (not (authenticator-closed auth))
                 (= 1 (length (authenticator-keys auth))))
      (error 'router-error :operation :auth-single-key :status -1))
    (copy-seq (first (authenticator-keys auth)))))

(defun keypair ()
  "Generate CURVE keys using the loaded libzmq; returns public and secret raw32 bytes."
  (star-zmq:library-version)
  (cffi:with-foreign-objects ((public :uint8 41) (secret :uint8 41)
                             (public-raw :uint8 32) (secret-raw :uint8 32))
    (unwind-protect
         (progn
           (checked (%curve-keypair public secret) :keypair)
           (when (or (cffi:null-pointer-p (%z85-decode public-raw public))
                     (cffi:null-pointer-p (%z85-decode secret-raw secret)))
             (error 'router-error :operation :keypair :status -2))
           (values (copy-from-foreign public-raw 32) (copy-from-foreign secret-raw 32)))
      (dotimes (i 41) (setf (cffi:mem-aref secret :uint8 i) 0))
      (dotimes (i 32) (setf (cffi:mem-aref secret-raw :uint8 i) 0)))))

(defun configure-curve-client (socket public secret server-public)
  "Set a DEALER/SUB's CURVE identity before connect; routing IDs grant no authority."
  (dolist (pair (list (cons 48 public) (cons 49 secret) (cons 50 server-public)))
    (check-type (cdr pair) (simple-array (unsigned-byte 8) (32)))
    (cffi:with-foreign-object (bytes :uint8 32)
      (unwind-protect
           (progn
             (copy-to-foreign (cdr pair) bytes)
             (checked (star-zmq::%setopt (star-zmq::owned-handle socket)
                                         (car pair) bytes 32) :curve-client))
        (dotimes (i 32) (setf (cffi:mem-aref bytes :uint8 i) 0)))))
  socket)

(defun start-authenticator (context keys)
  "Start one native ZAP owner for this context with an explicit public-key allowlist.
No private credentials are received; key admission is not application authorization.
The allowlist is an immutable owned snapshot. Stop listeners before closing or
replacing it. Multi-key authenticators support inbound peers; application target
bindings require a singleton key until native recipient-key routing exists."
  (unless (and (listp keys) (<= 1 (length keys) 1024))
    (error 'router-error :operation :auth-keys :status -1))
  (dolist (key keys) (check-type key (simple-array (unsigned-byte 8) (32))))
  (when (/= (length keys) (length (remove-duplicates keys :test #'equalp)))
    (error 'router-error :operation :auth-duplicate-keys :status -1))
  (setf keys (mapcar #'copy-seq keys))
  (ensure-library)
  (bordeaux-threads:with-lock-held (star-zmq::*context-lock*)
    (unless (and (eq context star-zmq::*process-context*) (star-zmq::context-handle context))
      (error 'router-error :operation :context :status -1))
    (cffi:with-foreign-objects ((bytes :uint8 (* 32 (length keys))) (token :uint64))
      (setf (cffi:mem-ref token :uint64) 0)
      (loop for key in keys for index from 0 do
        (copy-to-foreign key (cffi:inc-pointer bytes (* 32 index))))
      (checked (%auth-start (star-zmq::context-handle context) bytes (length keys) token)
               :auth-start)
      (incf (star-zmq::context-sockets context))
      (%make-authenticator :token (cffi:mem-ref token :uint64) :context context
                           :keys keys
                           :owner (bordeaux-threads:current-thread)))))

(defun close-authenticator (auth)
  "Close the native ZAP owner after all secure listeners; repeated close is harmless."
  (unless (eq (authenticator-owner auth) (bordeaux-threads:current-thread))
    (error 'router-error :operation :auth-owner :status -3))
  (bordeaux-threads:with-lock-held ((authenticator-lock auth))
    (when (plusp (authenticator-pins auth))
      (error 'router-error :operation :auth-listeners-active :status -1))
    (unless (authenticator-closed auth)
      (cffi:with-foreign-object (token :uint64)
        (setf (cffi:mem-ref token :uint64) (authenticator-token auth))
        (checked (%auth-close token) :auth-close))
      (bordeaux-threads:with-lock-held (star-zmq::*context-lock*)
        (decf (star-zmq::context-sockets (authenticator-context auth))))
      (setf (authenticator-closed auth) t (authenticator-token auth) 0)))
  nil)
