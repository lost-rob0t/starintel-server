(in-package :star.frontends.http-api)

(defvar *activity-v2-base-route-action* nil
  "Route-action implementation wrapped by the activity v2 authorization layer.")

(unless *activity-v2-base-route-action*
  (setf *activity-v2-base-route-action*
        (symbol-function 'route-action)))

(defun activity-v2-api-read-path-p (path)
  "True for the authenticated activity/timeline read surface."
  (or (string= path "/api/v1/timeline")
      (string= path "/api/v1/activity")
      (path-prefix-p "/api/v1/activity/" path)))

(defun route-action (method path)
  (if (and (eq method :get)
           (activity-v2-api-read-path-p path))
      "views:read"
      (funcall *activity-v2-base-route-action* method path)))
