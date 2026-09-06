(in-package :star.frontends.http-api)

(defun target-v1-outcome-disposition (outcome)
  (case (star.actors::target-dispatch-outcome-status outcome)
    (:accepted :created)
    (:duplicate :duplicate)
    (otherwise nil)))

(defun handle-v1-target-create-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (let* ((request (require-json-object (parse-json-request)))
           (principal (request-principal))
           (document (target-v1-document-from-request request principal))
           (ledger (target-v1-request-ledger request document principal))
           (record (star.actors::parse-target-record document))
           (outcome (star.actors::accept-target-record record))
           (disposition (target-v1-outcome-disposition outcome)))
      (unless disposition
        (signal-http-input-error
         409
         "target_request_rejected"
         (or (star.actors::target-dispatch-outcome-reason outcome)
             "Target request was rejected")))
      (setf (lack.response:response-status *response*)
            (if (eq disposition :created) 201 200))
      (jsown:to-json (target-v1-receipt ledger disposition)))))

(setf (ningle:route *app* +target-v1-path+ :method :post)
      #'handle-v1-target-create-route)