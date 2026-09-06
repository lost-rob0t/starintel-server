(in-package :star.frontends.http-api)

(defvar *target-v1-base-route-action* nil)

(unless *target-v1-base-route-action*
  (setf *target-v1-base-route-action* (symbol-function 'route-action)))

(defun route-action (method path)
  (if (and (eq method :post)
           (string= path +target-v1-path+))
      "targets:dispatch"
      (funcall *target-v1-base-route-action* method path)))