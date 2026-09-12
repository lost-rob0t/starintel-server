(in-package :star.frontends.http-api)

(defvar *view-v2-base-route-action* nil)

(unless *view-v2-base-route-action*
  (setf *view-v2-base-route-action* (symbol-function 'route-action)))

(defun view-v2-api-read-path-p (path)
  (some
   (lambda (base)
     (or (string= path base)
         (path-prefix-p (concatenate 'string base "/") path)))
   '("/api/v1/views"
     "/api/v1/analytics"
     "/api/v1/research"
     "/api/v1/graph"
     "/api/v1/geo"
     "/api/v1/operations"
     "/api/v1/migrations")))

(defun route-action (method path)
  (if (and (eq method :get)
           (view-v2-api-read-path-p path))
      "views:read"
      (funcall *view-v2-base-route-action* method path)))
