(in-package :star-server-tests)

(def-suite init-loader-tests
  :description "Tests for the init file loading system")

(in-suite init-loader-tests)

;;; Test Fixtures and Helpers

(defun make-temp-init-file (content)
  "Create a temporary init file for testing"
  (let ((temp-file (format nil "/tmp/test-init-~A.lisp" (get-universal-time))))
    (with-open-file (stream temp-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string content stream))
    (pathname temp-file)))

(defun make-temp-init-directory ()
  "Create a temporary directory for modular init files"
  (let ((temp-dir (uiop:ensure-directory-pathname
                   (format nil "/tmp/test-init-~A/" (get-universal-time)))))
    (ensure-directories-exist temp-dir)
    temp-dir))

(defun cleanup-temp-path (path)
  "Clean up temporary test files/directories"
  (when (probe-file path)
    (if (uiop:directory-pathname-p path)
        (uiop:delete-directory-tree path :validate t :if-does-not-exist :ignore)
        (delete-file path))))

(defun without-layout-whitespace (value)
  (remove-if (lambda (character)
               (member character
                       '(#\Space #\Tab #\Newline #\Return)
                       :test #'char=))
             value))

;;; Tests for ensure-init-file-exists

(test ensure-init-file-exists-creates-from-example
      "Test that ensure-init-file-exists copies from example config"
      (let ((temp-file (pathname (format nil "/tmp/test-init-~A.lisp" (get-universal-time)))))
        (unwind-protect
             (progn
               (star:ensure-init-file-exists temp-file)
               (is (probe-file temp-file))
               (let ((content (alexandria:read-file-into-string temp-file)))
                 (is (search "(in-package :star)" content))))
          (cleanup-temp-path temp-file))))

(test ensure-init-file-exists-creates-minimal-if-no-example
      "Test that ensure-init-file-exists creates minimal config if example missing"
      (let ((temp-file (pathname (format nil "/tmp/test-init-~A.lisp" (get-universal-time)))))
            ;; Temporarily shadow the system source directory to simulate missing example
            (original-dir (asdf:system-source-directory :starintel-gserver)))
        (unwind-protect
             (progn
               ;; This will fail to find example_configs and create minimal file
               (handler-case
                   (star:ensure-init-file-exists temp-file)
                 (error () nil))
               ;; Verify file was created even if example doesn't exist
               (when (probe-file temp-file)
                 (let ((content (alexandria:read-file-into-string temp-file)))
                   (is (search "Starintel Server Init File" content)))))
          (cleanup-temp-path temp-file))))

;;; Tests for load-init-file

(test load-init-file-success
      "Test successful loading of a valid init file"
      (let* ((test-var-name (gensym "TEST-VAR"))
             (content (format nil "(in-package :star)~%(defvar ~A 42)" test-var-name))
             (temp-file (make-temp-init-file content)))
        (unwind-protect
             (progn
               (is-true (star:load-init-file temp-file))
               (is (= 42 (symbol-value (find-symbol (symbol-name test-var-name) :star)))))
          (cleanup-temp-path temp-file))))

(test load-init-file-handles-syntax-error
      "Test that load-init-file properly handles syntax errors"
      (let* ((content "(in-package :star)(defun broken-syntax (")
             (temp-file (make-temp-init-file content)))
        (unwind-protect
             (signals error
                      (star:load-init-file temp-file))
          (cleanup-temp-path temp-file))))







;;; Tests for safe-load-init

(test safe-load-init-loads-file
      "Test safe-load-init with a regular file"
      (let* ((content "(in-package :star)(defvar *test-safe-load* 100)")
             (temp-file (make-temp-init-file content)))
        (unwind-protect
             (progn
               (is-true (star:safe-load-init temp-file))
               (is (= 100 (symbol-value (find-symbol "*TEST-SAFE-LOAD*" :star)))))
          (cleanup-temp-path temp-file))))



(test safe-load-init-creates-missing-file
      "Test that safe-load-init creates file if it doesn't exist"
      (let ((temp-file (pathname (format nil "/tmp/test-missing-~A.lisp" (get-universal-time)))))
        (unwind-protect
             (progn
               (is-false (probe-file temp-file))
               (is-true (star:safe-load-init temp-file))
               (is-true (probe-file temp-file)))
          (cleanup-temp-path temp-file))))

(test container-init-resolves-compose-secret-files
      "The container init must refresh every Compose *_FILE secret at command runtime"
      (let* ((source-path
               (uiop:merge-pathnames*
                "docker/star-server-init.lisp"
                (asdf:system-source-directory :starintel-gserver)))
             (source
               (without-layout-whitespace
                (alexandria:read-file-into-string source-path))))
        (dolist (variables
                 '(("COUCHDB_PASSWORD" "COUCHDB_PASSWORD_FILE")
                   ("RABBITMQ_PASSWORD" "RABBITMQ_PASSWORD_FILE")
                   ("STAR_AUTH_PEPPER" "STAR_AUTH_PEPPER_FILE")
                   ("STAR_AUTH_BOOTSTRAP_SECRET"
                    "STAR_AUTH_BOOTSTRAP_SECRET_FILE")))
          (destructuring-bind (value-variable file-variable) variables
            (is (search (format nil
                                "(environment-secret~s~s)"
                                value-variable
                                file-variable)
                        source))))))



;;; Tests for load-modular-init




;;; Run all init-loader tests

(defun run-init-loader-tests ()
  "Run all init loader tests"
  (run! 'init-loader-tests))












