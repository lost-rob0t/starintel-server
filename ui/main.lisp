(in-package :star-ui)

(defun parse-args (args)
  (let ((address "0.0.0.0")
        (port 8080)
        (backend-url "http://localhost:5000"))
    (loop for arg in args
          for i from 0
          do (cond
               ((string= arg "--address")
                (setf address (nth (1+ i) args)))
               ((string= arg "--port")
                (setf port (parse-integer (nth (1+ i) args))))
               ((string= arg "--backend")
                (setf backend-url (nth (1+ i) args)))))
    (values address port backend-url)))

(defun main ()
  (log:config :info)
  (multiple-value-bind (address port backend-url)
      (parse-args (uiop:command-line-arguments))
    (log:info "Star-UI Server starting...")
    (log:info "Address: ~a" address)
    (log:info "Port: ~a" port)
    (log:info "Backend API: ~a" backend-url)
    (start-ui-server :address address :port port :backend-url backend-url)
    (loop (sleep 1))))
