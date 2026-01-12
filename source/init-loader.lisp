(in-package :starintel-gserver)

(defun ensure-init-file-exists (init-path)
  "Ensure init file exists, copy from example_configs if not."
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
  "Load a single init file with error handling."
  (handler-case
      (progn
        (log:info (format nil "Loading init file: ~a" init-path))
        (load init-path :verbose nil :print nil)
        t)
    (error (e)
      (log:error (format nil "Failed to load ~a: ~a" init-path e))
      (error "Init file loading failed: ~a" e))))



(defun safe-load-init (init-path)
  "Safely load initialization configuration.

  Returns T on success, signals an error on failure."
  (let ((resolved-path (uiop:ensure-pathname init-path)))
    (cond
      ((probe-file resolved-path)
       (load-init-file resolved-path))

      (t
       (log:info (format nil "Init file ~a not found, creating default" resolved-path))
       (ensure-init-file-exists resolved-path)
       (load-init-file resolved-path)))))

