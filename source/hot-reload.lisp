(in-package :star.hot-reload)

(define-condition hot-reload-error (error)
  ((code
    :initarg :code
    :reader hot-reload-error-code)
   (message
    :initarg :message
    :reader hot-reload-error-message)
   (cause
    :initarg :cause
    :initform nil
    :reader hot-reload-error-cause))
  (:report
   (lambda (condition stream)
     (format stream "Hot reload failed (~a): ~a"
             (hot-reload-error-code condition)
             (hot-reload-error-message condition)))))

(defun environment-boolean (name &optional default)
  (let ((value (uiop:getenv name)))
    (if value
        (not (null
              (member (string-downcase value)
                      '("1" "true" "yes" "on")
                      :test #'string=)))
        default)))

(defun environment-number (name default)
  (let ((value (uiop:getenv name)))
    (if value
        (let ((parsed (read-from-string value)))
          (unless (and (realp parsed) (plusp parsed))
            (error "~a must be a positive number" name))
          parsed)
        default)))

(defparameter *enabled-p*
  (environment-boolean "STAR_HOT_RELOAD" nil)
  "When true, StarIntel may compile and load trusted patch files at runtime.")

(defparameter *directory*
  (or (uiop:getenv "STAR_HOT_RELOAD_DIRECTORY")
      ".star-hot-reload/")
  "Directory containing trusted runtime patch files.")

(defparameter *poll-seconds*
  (environment-number "STAR_HOT_RELOAD_POLL_SECONDS" 0.5d0)
  "Polling interval used by the live patch watcher.")

(defparameter *max-patch-bytes*
  1048576
  "Maximum accepted patch size. Hot patches should stay small and reviewable.")

(defvar *reload-lock* (bt:make-lock "star-hot-reload"))
(defvar *watcher-lock* (bt:make-lock "star-hot-reload-watcher"))
(defvar *watcher-thread* nil)
(defvar *watcher-stop-p* nil)
(defvar *image-id* (cms-ulid:ulid))
(defvar *image-generation* 0)
(defvar *last-patch* nil)
(defvar *last-success-at* nil)
(defvar *last-error* nil)
(defvar *actors-hook-installed-p* nil)

(defun image-id ()
  "Stable identifier for this Lisp image. It survives hot reloads, not restarts."
  *image-id*)

(defun image-generation ()
  "Number of successful live patches applied to this Lisp image."
  *image-generation*)

(defun image-marker ()
  "Diagnostic function intentionally suitable for hot-reload integration tests."
  "baseline")

(defun signal-hot-reload-error (code message &optional cause)
  (error 'hot-reload-error
         :code code
         :message message
         :cause cause))

(defun ensure-enabled ()
  (unless *enabled-p*
    (signal-hot-reload-error
     "disabled"
     "STAR_HOT_RELOAD is not enabled")))

(defun ensure-hot-reload-directory ()
  (let* ((directory (uiop:ensure-directory-pathname (pathname *directory*)))
         (sentinel (merge-pathnames ".starintel-directory-sentinel" directory)))
    (ensure-directories-exist sentinel)
    (truename directory)))

(defun pathname-prefix-p (directory pathname)
  (let ((prefix (namestring (uiop:ensure-directory-pathname directory)))
        (candidate (namestring pathname)))
    (and (<= (length prefix) (length candidate))
         (string= prefix candidate :end2 (length prefix)))))

(defun canonical-patch-path (relative-path)
  (ensure-enabled)
  (let* ((requested (pathname relative-path))
         (root (ensure-hot-reload-directory)))
    (when (uiop:absolute-pathname-p requested)
      (signal-hot-reload-error
       "absolute_path"
       "Patch paths must be relative to STAR_HOT_RELOAD_DIRECTORY"))
    (let ((candidate
            (handler-case
                (truename (merge-pathnames requested root))
              (file-error (condition)
                (signal-hot-reload-error
                 "missing_patch"
                 (format nil "Patch does not exist: ~a" relative-path)
                 condition)))))
      (unless (pathname-prefix-p root candidate)
        (signal-hot-reload-error
         "path_escape"
         "Patch resolves outside STAR_HOT_RELOAD_DIRECTORY"))
      (unless (string-equal "lisp" (pathname-type candidate))
        (signal-hot-reload-error
         "invalid_extension"
         "Only .lisp patch files may be hot loaded"))
      candidate)))

(defun patch-size (pathname)
  (with-open-file (stream pathname :direction :input)
    (file-length stream)))

(defun validate-patch-file (pathname)
  (let ((size (patch-size pathname)))
    (when (> size *max-patch-bytes*)
      (signal-hot-reload-error
       "patch_too_large"
       (format nil "Patch is ~d bytes; maximum is ~d"
               size
               *max-patch-bytes*))))
  pathname)

(defun temporary-fasl-path (source)
  (make-pathname
   :name (format nil "star-hot-reload-~a" (cms-ulid:ulid))
   :type (or (pathname-type (compile-file-pathname source)) "fasl")
   :defaults (uiop:temporary-directory)))

(defun record-failure (pathname condition)
  (setf *last-error*
        (list :path (and pathname (namestring pathname))
              :at (get-universal-time)
              :message (princ-to-string condition))))

(defun compile-and-load-patch (pathname)
  (validate-patch-file pathname)
  (let ((fasl (temporary-fasl-path pathname)))
    (unwind-protect
         (multiple-value-bind (output warnings-p failure-p)
             (compile-file pathname :output-file fasl)
           (declare (ignore warnings-p))
           (when (or failure-p (null output))
             (signal-hot-reload-error
              "compile_failed"
              (format nil "Compilation failed for ~a" pathname)))
           (load output :verbose nil :print nil))
      (when (probe-file fasl)
        (ignore-errors (delete-file fasl))))))

(defun apply-canonical-patch (pathname)
  (bt:with-lock-held (*reload-lock*)
    (handler-case
        (progn
          (compile-and-load-patch pathname)
          (incf *image-generation*)
          (setf *last-patch* (namestring pathname)
                *last-success-at* (get-universal-time)
                *last-error* nil)
          (log:info
           "Hot-reloaded patch ~a into image ~a generation ~d"
           pathname
           *image-id*
           *image-generation*)
          *image-generation*)
      (hot-reload-error (condition)
        (record-failure pathname condition)
        (log:error "Hot reload rejected ~a: ~a" pathname condition)
        (error condition))
      (condition (condition)
        (record-failure pathname condition)
        (log:error "Hot reload failed for ~a: ~a" pathname condition)
        (signal-hot-reload-error
         "load_failed"
         (format nil "Could not load patch ~a" pathname)
         condition)))))

(defun apply-patch-file (relative-path)
  "Compile and load RELATIVE-PATH into the current Lisp image.

The path is resolved beneath STAR_HOT_RELOAD_DIRECTORY. Compilation happens
before LOAD, and successful loads increment IMAGE-GENERATION. Existing server,
actor, RabbitMQ, CouchDB and HTTP objects are not restarted."
  (apply-canonical-patch (canonical-patch-path relative-path)))

(defun lisp-patch-file-p (pathname)
  (string-equal "lisp" (pathname-type pathname)))

(defun patch-files (root)
  (remove-if-not #'lisp-patch-file-p (uiop:directory-files root)))

(defun snapshot-patches (root)
  (let ((snapshot (make-hash-table :test #'equal)))
    (dolist (pathname (patch-files root))
      (setf (gethash (namestring (truename pathname)) snapshot)
            (file-write-date pathname)))
    snapshot))

(defun runtime-live-p-if-available ()
  (let* ((package (find-package :star.runtime))
         (symbol (and package (find-symbol "RUNTIME-LIVE-P" package))))
    (if (and symbol (fboundp symbol))
        (funcall symbol)
        t)))

(defun watch-loop (root seen follow-runtime)
  (unwind-protect
       (loop until *watcher-stop-p*
             while (or (not follow-runtime)
                       (runtime-live-p-if-available))
             do (dolist (pathname (patch-files root))
                  (let* ((canonical (truename pathname))
                         (key (namestring canonical))
                         (stamp (file-write-date canonical))
                         (previous (gethash key seen)))
                    (unless (eql stamp previous)
                      ;; Mark before applying so a bad patch is not retried in a
                      ;; tight loop. Editing the file gives it another chance.
                      (setf (gethash key seen) stamp)
                      (handler-case
                          (apply-canonical-patch canonical)
                        (hot-reload-error (condition)
                          (log:error "Watcher rejected ~a: ~a"
                                     canonical
                                     condition)))))
                (sleep *poll-seconds*))
    (bt:with-lock-held (*watcher-lock*)
      (when (eq *watcher-thread* (bt:current-thread))
        (setf *watcher-thread* nil
              *watcher-stop-p* nil)))))

(defun watcher-running-p ()
  (and *watcher-thread*
       (bt:thread-alive-p *watcher-thread*)
       t))

(defun start-watcher (&key (follow-runtime t))
  "Start the singleton patch-directory watcher when hot reload is enabled.

Existing .lisp files are snapshotted and are not loaded automatically. New or
modified .lisp files are compiled and loaded once. Use atomic rename/copy when
publishing a patch so the watcher never observes a partially written file."
  (ensure-enabled)
  (bt:with-lock-held (*watcher-lock*)
    (when (watcher-running-p)
      (return-from start-watcher *watcher-thread*))
    (let* ((root (ensure-hot-reload-directory))
           (seen (snapshot-patches root)))
      (setf *watcher-stop-p* nil
            *watcher-thread*
            (bt:make-thread
             (lambda () (watch-loop root seen follow-runtime))
             :name "star-hot-reload-watcher"))
      (log:info "Hot reload watcher active for ~a (image ~a)"
                root
                *image-id*)
      *watcher-thread*)))

(defun stop-watcher ()
  "Stop the live patch watcher. Idempotent."
  (let ((thread nil))
    (bt:with-lock-held (*watcher-lock*)
      (setf *watcher-stop-p* t
            thread *watcher-thread*))
    (when (and thread
               (bt:thread-alive-p thread)
               (not (eq thread (bt:current-thread))))
      (handler-case
          (bt:with-timeout (5)
            (bt:join-thread thread))
        (bt:timeout ()
          (log:warn "Timed out waiting for hot reload watcher to stop"))))
    (bt:with-lock-held (*watcher-lock*)
      (unless (and *watcher-thread*
                   (bt:thread-alive-p *watcher-thread*))
        (setf *watcher-thread* nil
              *watcher-stop-p* nil)))
    t))

(defun maybe-start-watcher ()
  (when *enabled-p*
    (start-watcher :follow-runtime t)))

(unless *actors-hook-installed-p*
  (nhooks:add-hook star:*actors-start-hook* #'maybe-start-watcher)
  (setf *actors-hook-installed-p* t))

(defun status-json ()
  "Return operator-visible hot reload state as JSON."
  (jsown:to-json
   (jsown:new-js
    ("enabled" (if *enabled-p* :true :false))
    ("watcher_running" (if (watcher-running-p) :true :false))
    ("image_id" *image-id*)
    ("generation" *image-generation*)
    ("image_marker" (image-marker))
    ("directory" *directory*)
    ("last_patch" (or *last-patch* :null))
    ("last_success_at" (or *last-success-at* :null))
    ("last_error"
     (if *last-error*
         (jsown:new-js
          ("path" (or (getf *last-error* :path) :null))
          ("at" (getf *last-error* :at))
          ("message" (getf *last-error* :message)))
         :null)))))
