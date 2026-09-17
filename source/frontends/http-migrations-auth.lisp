(in-package :star.frontends.http-api)

(defvar *migration-base-route-action* nil)

(unless *migration-base-route-action*
  (setf *migration-base-route-action*
        (symbol-function 'route-action)))

(defun route-action (method path)
  (cond
    ((and (eq method :get)
          (string= path +migration-preview-path+))
     "views:read")
    ((and (eq method :post)
          (string= path +migration-apply-path+))
     "documents:write")
    (t
     (funcall *migration-base-route-action* method path))))
