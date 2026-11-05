(in-package :star-server-tests)

;;; Tests for the STAR_SERVER_PRO_ACTORS init-time plugin gate
;;; (star:maybe-start-pro-actors). Hermetic: a fake plugin package stands in
;;; for the real starintel-pro-actors system, which is only present in the
;;; server image, not the unit-test image.

(def-suite pro-actors-plugin-tests
    :description "Env-gated starintel-pro-actors plugin startup")

(in-suite pro-actors-plugin-tests)

(defmacro with-plugin-env (value &body body)
  "Run BODY with STAR_SERVER_PRO_ACTORS bound to VALUE (nil unsets)."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (uiop:getenv "STAR_SERVER_PRO_ACTORS")))
       (unwind-protect
            (progn
              (if ,value
                  (setf (uiop:getenv "STAR_SERVER_PRO_ACTORS") ,value)
                  (setf (uiop:getenv "STAR_SERVER_PRO_ACTORS") ""))
              ,@body)
         (if ,saved
             (setf (uiop:getenv "STAR_SERVER_PRO_ACTORS") ,saved)
             (setf (uiop:getenv "STAR_SERVER_PRO_ACTORS") ""))))))

(defun ensure-fake-plugin-absent ()
  (let ((package (find-package :starintel-pro-actors)))
    (when package
      (delete-package package))))

(test gate-is-inert-when-env-unset
  (with-plugin-env nil
    (ensure-fake-plugin-absent)
    (is (null (star:maybe-start-pro-actors)))))

(test gate-signals-when-plugin-not-in-image
  (with-plugin-env "1"
    (ensure-fake-plugin-absent)
    (signals error (star:maybe-start-pro-actors))))

(test gate-calls-plugin-start-when-env-set
  (with-plugin-env "1"
    (ensure-fake-plugin-absent)
    (let ((called 0))
      (let ((package (make-package :starintel-pro-actors))
            (start (intern "START" :starintel-pro-actors)))
        (setf (symbol-function start) (lambda () (incf called)))
        (unwind-protect
             (progn
               (is (eq t (star:maybe-start-pro-actors)))
               (is (= 1 called)))
          (ensure-fake-plugin-absent))))))

(defun run-pro-actors-plugin-tests ()
  "Run all pro-actors plugin gate tests"
  (run! 'pro-actors-plugin-tests))
