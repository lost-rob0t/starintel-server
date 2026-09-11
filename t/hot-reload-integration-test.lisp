(in-package :star-server-tests)

(in-suite http-api-tests)

(defun hot-reload-test-directory ()
  (uiop:ensure-directory-pathname
   (merge-pathnames
    (format nil "star-hot-reload-ci-~a/" (cms-ulid:ulid))
    (uiop:temporary-directory))))

(defun write-test-patch (directory name content)
  (let ((pathname (merge-pathnames name directory)))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string content stream))
    pathname))

(defun wait-for-image-marker (expected &key (attempts 100) (sleep-seconds 0.05d0))
  (loop repeat attempts
        when (string= expected (star.hot-reload:image-marker))
          do (return t)
        do (sleep sleep-seconds)
        finally (return nil)))

(defun server-root-responds-p ()
  (handler-case
      (multiple-value-bind (body status)
          (dex:get (make-test-url "/"))
        (declare (ignore body))
        (= status 200))
    (condition () nil)))

(test live-hot-reload-changes-current-image-without-restart
  "A live patch must change executable code while the OS PID, image identity,
HTTP server handle and listening service stay unchanged."
  (let* ((directory (hot-reload-test-directory))
         (original-enabled star.hot-reload:*enabled-p*)
         (original-directory star.hot-reload:*directory*)
         (original-poll star.hot-reload:*poll-seconds*)
         (original-marker
           (symbol-function 'star.hot-reload:image-marker))
         (server-before *test-server*)
         (pid-before (star.hot-reload:process-id))
         (image-before (star.hot-reload:image-id))
         (generation-before (star.hot-reload:image-generation)))
    (unwind-protect
         (progn
           (ensure-directories-exist (merge-pathnames "sentinel" directory))
           (setf star.hot-reload:*enabled-p* t
                 star.hot-reload:*directory* (namestring directory)
                 star.hot-reload:*poll-seconds* 0.05d0)

           (is (integerp pid-before)
               "CI must observe a real OS PID before applying the patch")
           (is (string= "baseline" (star.hot-reload:image-marker)))
           (is (eq server-before *test-server*))
           (is (server-root-responds-p))

           ;; Start the same watcher production uses, then publish the patch
           ;; after it snapshots the directory. No process or HTTP restart is
           ;; allowed anywhere in this test.
           (star.hot-reload:start-watcher :follow-runtime nil)
           (is (star.hot-reload:watcher-running-p))
           (write-test-patch
            directory
            "live-image-change.lisp"
            (format nil
                    "(in-package :star.hot-reload)~%~%(defun image-marker () \"patched-ci\")~%"))

           (is (wait-for-image-marker "patched-ci")
               "Watcher did not replace IMAGE-MARKER in the running image")
           (is (= pid-before (star.hot-reload:process-id))
               "Hot reload must not replace the OS process")
           (is (string= image-before (star.hot-reload:image-id))
               "Hot reload must preserve the Lisp image identity")
           (is (> (star.hot-reload:image-generation) generation-before)
               "Successful hot reload must advance image generation")
           (is (eq server-before *test-server*)
               "Clack server handle changed, indicating a restart")
           (is (server-root-responds-p)
               "HTTP service stopped responding after live patch")

           ;; A syntactically broken patch must fail before LOAD and leave the
           ;; already-patched image and server alive.
           (star.hot-reload:stop-watcher)
           (let ((good-generation (star.hot-reload:image-generation)))
             (write-test-patch
              directory
              "broken-patch.lisp"
              "(in-package :star.hot-reload)\n(defun image-marker () \"broken\"\n")
             (is
              (handler-case
                  (progn
                    (star.hot-reload:apply-patch-file "broken-patch.lisp")
                    nil)
                (star.hot-reload:hot-reload-error () t))
              "Broken patch must be rejected")
             (is (= good-generation (star.hot-reload:image-generation))
                 "Rejected patch must not advance image generation")
             (is (string= "patched-ci" (star.hot-reload:image-marker))
                 "Rejected patch must not replace executable code")
             (is (= pid-before (star.hot-reload:process-id)))
             (is (string= image-before (star.hot-reload:image-id)))
             (is (eq server-before *test-server*))
             (is (server-root-responds-p))))
      (ignore-errors (star.hot-reload:stop-watcher))
      (setf (symbol-function 'star.hot-reload:image-marker) original-marker
            star.hot-reload:*enabled-p* original-enabled
            star.hot-reload:*directory* original-directory
            star.hot-reload:*poll-seconds* original-poll)
      (ignore-errors
        (uiop:delete-directory-tree
         directory
         :validate t
         :if-does-not-exist :ignore)))))
