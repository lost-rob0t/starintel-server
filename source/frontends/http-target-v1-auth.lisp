(in-package :star.frontends.http-api)

(defvar *target-v1-base-route-action* nil)

(unless *target-v1-base-route-action*
  (setf *target-v1-base-route-action* (symbol-function 'route-action)))

(defun route-action (method path)
  (if (string= path +target-v1-path+)
      (case method
        (:post "targets:dispatch")
        (:get "targets:read")
        (otherwise nil))
      (funcall *target-v1-base-route-action* method path)))