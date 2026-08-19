(in-package :star.api.client)

;;; Deadline and final protocol normalization layer.
;;;
;;; REQUEST-OPTIONS is intentionally a simple public value. Keep absolute
;;; monotonic deadlines out-of-band so the public structure remains compatible
;;; while repeated/nested calls with the same options cannot restart a timeout.

(defparameter *request-option-deadlines* (make-hash-table :test #'eq))
(defparameter *raw-make-request-options*
  (symbol-function 'make-request-options))

(defun monotonic-ticks ()
  (get-internal-real-time))

(defun milliseconds-to-internal-ticks (milliseconds)
  (round (* milliseconds internal-time-units-per-second) 1000))

(defun internal-ticks-to-milliseconds (ticks)
  (floor (* ticks 1000) internal-time-units-per-second))

(defun set-request-options-deadline (options timeout-ms)
  (when timeout-ms
    (setf (gethash options *request-option-deadlines*)
          (+ (monotonic-ticks)
             (milliseconds-to-internal-ticks timeout-ms))))
  options)

(defun make-request-options (&key timeout-ms correlation-id idempotency-key headers)
  "Create request options with one absolute monotonic deadline when bounded."
  (let ((options
          (funcall *raw-make-request-options*
                   :timeout-ms timeout-ms
                   :correlation-id correlation-id
                   :idempotency-key idempotency-key
                   :headers headers)))
    (set-request-options-deadline options timeout-ms)))

(defun ensure-request-options (options client)
  (let ((result (or options (make-request-options))))
    (unless (request-options-timeout-ms result)
      (setf (request-options-timeout-ms result)
            (star-client-default-timeout-ms client)))
    (unless (gethash result *request-option-deadlines*)
      (set-request-options-deadline
       result (request-options-timeout-ms result)))
    result))

(defun remaining-request-timeout-ms (options)
  (let ((deadline (gethash options *request-option-deadlines*)))
    (if deadline
        (max 0
             (internal-ticks-to-milliseconds
              (- deadline (monotonic-ticks))))
        (request-options-timeout-ms options))))

(defun make-dispatch-options (options remaining-ms)
  (funcall *raw-make-request-options*
           :timeout-ms remaining-ms
           :correlation-id (request-options-correlation-id options)
           :idempotency-key (request-options-idempotency-key options)
           :headers (copy-tree (request-options-headers options))))

(defun error-envelope-fields (body)
  "Decode legacy StarIntel errors and RFC 9457 problem details."
  (when (and (stringp body) (probable-json-body-p body))
    (let ((parsed (ignore-errors (jsown:parse body))))
      (when parsed
        (values
         (or (jsown:val-safe parsed "code")
             (jsown:val-safe parsed "type"))
         (or (jsown:val-safe parsed "detail")
             (jsown:val-safe parsed "msg")
             (jsown:val-safe parsed "message")
             (jsown:val-safe parsed "title"))
         (or (jsown:val-safe parsed "correlation_id")
             (jsown:val-safe parsed "correlationId")))))))

(defun call-operation (client operation-id
                       &key path-parameters query-parameters headers body
                         request-options)
  "Execute exactly one contracted operation inside one absolute deadline.

There are deliberately no hidden retries here. Replaying a mutation requires an
explicit operation-level idempotency contract rather than transport optimism."
  (let* ((operation (star.http.contract:find-http-operation operation-id))
         (options (ensure-request-options request-options client))
         (remaining-ms (remaining-request-timeout-ms options)))
    (when (and (request-options-timeout-ms options)
               (<= remaining-ms 0))
      (error 'client-timeout-error
             :message (format nil "Operation ~a deadline expired before dispatch"
                              operation-id)))
    (let ((dispatch-options (make-dispatch-options options remaining-ms)))
      (when headers
        (setf (request-options-headers dispatch-options)
              (merge-headers (request-options-headers dispatch-options)
                             headers)))
      (let* ((path (expand-operation-path operation path-parameters))
             (body-string (request-body-string body))
             (request
               (make-client-request
                :method (star.http.contract:http-operation-method operation)
                :uri (make-url client path :query query-parameters)
                :headers (effective-request-headers
                          client body-string dispatch-options)
                :body body-string
                :timeout-ms remaining-ms
                :operation-id operation-id))
             (response
               (perform-client-request (star-client-transport client) request)))
        (unless (typep response 'client-response)
          (error 'client-protocol-error
                 :message "Client transport returned a non-response value"
                 :operation-id operation-id))
        (unless (successful-status-p (client-response-status response))
          (signal-response-error response operation-id))
        (let ((value (decoded-response-value response operation-id)))
          (when (and (null (client-response-correlation-id response))
                     (consp value)
                     (eq (first value) :obj))
            (setf (client-response-correlation-id response)
                  (jsown:val-safe value "correlation_id")))
          (values value response))))))
