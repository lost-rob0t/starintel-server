(in-package :star.starfs)

;;;; Local content-addressed filesystem backend.
;;;;
;;;; Immutable blocks live as one file per content id under a two-hex-digit
;;;; fan-out directory below ROOT. Writes go to a unique temp sibling and are
;;;; renamed into place, so a reader can never observe a partial block and a
;;;; crashed or interrupted put leaves no gettable address behind. The
;;;; backend is dumb-bytes CAS: it owns no namespace, no keys, and no
;;;; policy. It is also the conformance reference for every future backend
;;;; (object stores, IPFS transport, peer replication).

(defclass local-block-store (block-store)
  ((root :initarg :root
         :reader local-block-store-root
         :documentation "Directory pathname holding all block files."))
  (:documentation
   "Content-addressed block store rooted at a filesystem directory."))

(setf (documentation 'local-block-store-root 'function)
      "The =root= directory pathname of a =local-block-store=.")

(defun make-local-block-store (&key root)
  "Create a local block store under directory ROOT, creating it when missing."
  (unless (or (stringp root) (pathnamep root))
    (error 'starfs-error
           :format-control "ROOT must be a directory designator, got ~s"
           :format-arguments (list root)))
  (let ((root-path
          (uiop:ensure-directory-pathname
           (if (stringp root) (pathname root) root))))
    (ensure-directories-exist root-path)
    (make-instance 'local-block-store :root root-path)))

(defun block-relative-directory (hex)
  "Two-hex-digit fan-out directory name for HEX digest body."
  (subseq hex 0 2))

(defun block-path (store content-id)
  "Filesystem path of the complete block addressed by CONTENT-ID. Partial temp files never live at this path. The block file carries an explicit type so the atomic rename target's type is never unspecified (NIL pathname components are merged from the rename source by rename-file, which would rename the temp file onto itself)."
  (let* ((hex (content-id-hex content-id))
         (root (local-block-store-root store)))
    (merge-pathnames
     (make-pathname
      :directory (append (pathname-directory root)
                         (list (block-relative-directory hex)))
      :name hex
      :type "blk")
     root)))

(defun temporary-block-path (path)
  "Return a unique temp sibling of PATH for one atomic write attempt."
  (make-pathname
   :name (format nil "~a-~a"
                 (pathname-name path)
                 (string-downcase (star.ids:ulid)))
   :type "blk-tmp"
   :defaults path))

(defun read-block-file-bytes (path)
  "Read PATH into a fresh (unsigned-byte 8) vector."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream)
                             :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun block-file-matches-content-id-p (path content-id)
  "Return true only when PATH exists and its exact bytes hash to CONTENT-ID."
  (when (uiop:file-exists-p path)
    (handler-case
        (string= content-id
                 (content-id-from-bytes (read-block-file-bytes path)))
      (file-error () nil))))

(defun atomic-write-block-file (path bytes content-id)
  "Write BYTES to a unique temp sibling of PATH, then atomically publish it. Concurrent writers of the same CONTENT-ID may race to publish identical bytes; all successful final states must still verify against CONTENT-ID."
  (ensure-directories-exist (uiop:pathname-directory-pathname path))
  (let ((temp-path (temporary-block-path path)))
    (unwind-protect
         (progn
           (with-open-file (stream temp-path
                                   :direction :output
                                   :if-exists :error
                                   :if-does-not-exist :create
                                   :element-type '(unsigned-byte 8))
             (write-sequence bytes stream))
           ;; A concurrent writer may have published the same immutable block
           ;; while this attempt was writing. Avoid replacing a verified final
           ;; file when it is already correct; otherwise publish our complete
           ;; temp file. On platforms where rename-file rejects an existing
           ;; target, accept that race only if the winner verifies.
           (unless (block-file-matches-content-id-p path content-id)
             (handler-case
                 (rename-file temp-path path)
               (file-error (condition)
                 (unless (block-file-matches-content-id-p path content-id)
                   (error condition))))))
           (unless (block-file-matches-content-id-p path content-id)
             (error 'block-integrity-error :content-id content-id)))
      (when (uiop:file-exists-p temp-path)
        (ignore-errors (delete-file temp-path))))))

(defmethod put-block ((store local-block-store) bytes)
  (let* ((content-id (content-id-from-bytes bytes))
         (path (block-path store content-id)))
    ;; Idempotency is content-addressed, not path-addressed: an existing path
    ;; is reusable only when its bytes still hash to the requested id. Repair
    ;; an out-of-band-corrupted file atomically from the caller's verified
    ;; bytes instead of returning a false success that get-block cannot read.
    (unless (block-file-matches-content-id-p path content-id)
      (atomic-write-block-file path bytes content-id))
    content-id))

(defmethod get-block ((store local-block-store) content-id)
  (let ((path (block-path store content-id)))
    (unless (uiop:file-exists-p path)
      (error 'block-not-found :content-id content-id))
    (let ((bytes (read-block-file-bytes path)))
      (unless (string= content-id (content-id-from-bytes bytes))
        (error 'block-integrity-error :content-id content-id))
      bytes)))

(defmethod block-exists-p ((store local-block-store) content-id)
  (and (content-id-p content-id)
       (block-file-matches-content-id-p
        (block-path store content-id)
        content-id)))

(defmethod delete-block ((store local-block-store) content-id)
  (when (content-id-p content-id)
    (let ((path (block-path store content-id)))
      (when (uiop:file-exists-p path)
        (delete-file path)
        t))))