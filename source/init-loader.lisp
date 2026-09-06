(in-package :starintel-gserver)

(defun ensure-init-file-exists (init-path)
  "Ensure INIT-PATH exists, copying =example_configs/init.lisp= if absent.

Falls back to a minimal template when the example config itself is
missing.  Call before =load-init-file= on first boot."
  (let ((example-config (uiop:merge-pathnames*
                         "example_configs/init.lisp"
                         (asdf:system-source-directory :starintel-gserver))))
    (cond
      ((probe-file example-config)
       (log:info (format nil "Creating default init file from ~a" example-config))
       (uiop:copy-file example-config init-path))
      (t
       (log:warn "No example config found, creating minimal init file")
       (with-open-file (stream init-path
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
         (format stream ";; Starintel Server Init File~%")
         (format stream ";; Generated at ~a~%~%" (get-universal-time))
         (format stream "(in-package :star)~%~%")
         (format stream ";; Configure your settings here~%")
         (format stream ";; See example_configs/init.lisp for examples~%"))))))

(defun load-init-file (init-path)
  "Load the init file INIT-PATH with error handling.

The file is evaluated in whatever package it declares; the stock
template uses =(in-package :star)= so settings like
=star:*http-api-port*= are directly assignable.  Signals on failure
after logging."
  (handler-case
      (progn
        (log:info (format nil "Loading init file: ~a" init-path))
        (load init-path :verbose nil :print nil)
        t)
    (error (e)
      (log:error (format nil "Failed to load ~a: ~a" init-path e))
      (error "Init file loading failed: ~a" e))))



(defun safe-load-init (init-path)
  "Load INIT-PATH, creating it first if it does not exist yet.

Returns T on success, signals an error on failure.  This is the
idempotent entry point used by the runtime startup."
  (let ((resolved-path (uiop:ensure-pathname init-path)))
    (cond
      ((probe-file resolved-path)
       (load-init-file resolved-path))

      (t
       (log:info (format nil "Init file ~a not found, creating default" resolved-path))
       (ensure-init-file-exists resolved-path)
       (load-init-file resolved-path)))))

