(in-package :star.frontends.http-api)

(defun status-msg (msg status &key info traceback code)
  "Return a stable JSON status envelope with an optional machine-readable code."
  (let ((json
          (jsown:new-js
            ("msg" msg)
            ("status" (string-downcase (symbol-name status))))))
    (when code
      (jsown:extend-js json ("code" code)))
    (when info
      (jsown:extend-js json ("info" info)))
    (when traceback
      (jsown:extend-js json ("trace" traceback)))
    (jsown:to-json json)))
