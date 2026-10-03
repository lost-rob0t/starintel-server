(in-package :star.observability)

;; Lack middleware: the single HTTP observability boundary. Extracts W3C
;; trace context, records one server span plus request metrics per request,
;; and injects traceparent into the response. Never alters app behavior: any
;; internal error is logged and re-signaled for the app's own handler.

(defun env-header-value (env name)
  "Read a request header from a lack env."
  (let ((headers (getf env :headers)))
    (when headers
      (or (gethash name headers)
          (gethash (string-downcase name) headers)
          (gethash (string-upcase name) headers)))))

(defun normalized-route (env)
  "Bounded-cardinality route label. The gserver glue replaces the raw path
with the route template from the HTTP contract table when one matches;
otherwise the sanitized path is used and path parameters remain unbounded."
  (or (getf env :star-route-template) (getf env :path-info "/")))

(defun status-class (status)
  "5xx/4xx/3xx/2xx/1xx classification for metric labels."
  (cond ((null status) "unknown")
        ((>= status 500) "5xx")
        ((>= status 400) "4xx")
        ((>= status 300) "3xx")
        ((>= status 200) "2xx")
        (t "1xx")))

(defun observability-http-middleware (app)
  "Wrap a lack app with trace context extraction, one server span, request
metrics, and response traceparent injection."
  (lambda (env)
    (let* ((inbound (parse-traceparent (env-header-value env "traceparent")))
           (context (or inbound (make-root-context)))
           (start (now-unix-nanos))
           (start-real (get-internal-real-time))
           (status nil)
           (*current-trace-context* context))
      (let ((response
              (handler-case (funcall app env)
                (error (condition)
                  (emit-log-event
                   :error "http handler failed"
                   :attributes
                   (list (cons "error.type"
                               (format nil "~a" (type-of condition)))))
                  (error condition)))))
        (setf status (and response (listp response) (first response)))
        (let ((elapsed-ms
                (/ (- (get-internal-real-time) start-real)
                   internal-time-units-per-second 0.001)))
          (when (signal-enabled-p :traces)
            (queue-span
             context "http.server" 1 start (now-unix-nanos)
             (list (cons "http.request.method"
                         (string-upcase
                          (format nil "~a" (getf env :request-method :get))))
                   (cons "http.route" (normalized-route env))
                   (cons "http.status_class" (status-class status))
                   (cons "duration.ms" elapsed-ms))
             (not (and status (>= status 500)))))
          (record-counter
           "starintel_http_requests_total" 1
           :attributes
           (list (cons "http.route" (normalized-route env))
                 (cons "http.request.method"
                       (string-upcase
                        (format nil "~a" (getf env :request-method :get))))
                 (cons "http.status_class" (status-class status))))
          (record-counter
           "starintel_http_request_duration_ms"
           elapsed-ms
           :attributes
           (list (cons "http.route" (normalized-route env))
                 (cons "http.status_class" (status-class status)))))
        (when response
          ;; Clack responses are plain (status headers body) lists; append the
          ;; response traceparent without touching lack response objects.
          (setf response
                (list (first response)
                      (append (second response)
                              (list :traceparent
                                    (encode-traceparent context)))
                      (third response))))
        response))))