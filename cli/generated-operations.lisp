(in-package :star.api.client)

;;; This layer is generated from the shared Lisp contract at compile/load time.
;;; It intentionally contains no HTTP-library, authentication, retry, or error
;;; policy. Those semantics stay in CLIENT-RUNTIME.LISP.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (operation (star.http.contract:all-http-operations))
    (let* ((name (star.http.contract:operation-request-symbol-name operation))
           (symbol (intern name :star.api.client))
           (operation-id (star.http.contract:http-operation-id operation))
           (summary (star.http.contract:http-operation-summary operation)))
      (eval
       `(defun ,symbol (client &key path-parameters query-parameters headers body
                                    request-options)
          ,summary
          (call-operation client ,operation-id
                          :path-parameters path-parameters
                          :query-parameters query-parameters
                          :headers headers
                          :body body
                          :request-options request-options)))
      (export symbol :star.api.client))))
