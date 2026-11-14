(in-package :star.frontends.http-api)

(defvar *target-v1-base-route-action* nil)

(unless *target-v1-base-route-action*
  (setf *target-v1-base-route-action* (symbol-function 'route-action)))

(defun route-action (method path)
  (cond
    ((string= path +target-v1-path+)
     (case method
       (:post "targets:dispatch")
       (:get "targets:read")
       (otherwise nil)))
    ((and (eq method :patch)
          (path-prefix-p "/api/v1/targets/" path))
     "targets:update")
    (t (funcall *target-v1-base-route-action* method path))))