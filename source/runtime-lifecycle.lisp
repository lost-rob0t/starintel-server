(in-package :star.consumers)

(defparameter +rabbit-consume-poll-timeout-us+ 250000
  "Maximum idle Rabbit consume wait before a worker re-checks lifecycle state.")

(define-condition consumer-read-timeout (condition) ())

(defmethod stream-read ((stream rabbit-queue-stream))
  "Read one Rabbit delivery with a finite wait so shutdown can be observed."
  (assert-rabbit-stream-owner stream)
  (handler-case
      (cl-rabbit:consume-message
       (rabbit-stream-connection stream)
       :timeout +rabbit-consume-poll-timeout-us+)
    (cl-rabbit:rabbitmq-library-error (condition)
      (if (eq :amqp-status-timeout
              (cl-rabbit:rabbitmq-library-error/error-code condition))
          (signal 'consumer-read-timeout)
          (error condition)))))

(defun run-consumer (consumer)
  "Run a consumer on its owner thread while periodically observing stop state."
  (unwind-protect
       (progn
         (open-stream (consumer-stream consumer))
         (setf (consumer-running-p consumer) t)
         (consumer-update-state consumer :running)
         (loop until (eq (consumer-state consumer) :stopping)
               do (handler-case
                      (consumer-process-delivery
                       consumer
                       (consumer-read consumer))
                    (consumer-read-timeout () nil)
                    (end-of-file ()
                      (return)))))
    (when (consumer-running-p consumer)
      (handler-case
          (close-stream (consumer-stream consumer))
        (error (condition)
          (log:warn "Consumer ~a stream close failed during shutdown: ~a"
                    (consumer-name consumer)
                    condition))))
    (setf (consumer-running-p consumer) nil)
    (consumer-update-state consumer :stopped))
  consumer)

(defun join-consumer-owned-threads (consumer)
  "Join only threads explicitly retained by CONSUMER."
  (dolist (thread (consumer-threads consumer))
    (when (bt:thread-alive-p thread)
      (bt:join-thread thread)))
  consumer)

(defun stop-consumer-and-wait (consumer &key (timeout-seconds 5))
  "Request stop and wait only for CONSUMER-owned threads."
  (stop-consumer consumer)
  (handler-case
      (bt:with-timeout (timeout-seconds)
        (join-consumer-owned-threads consumer)
        (setf (consumer-running-p consumer) nil)
        (consumer-update-state consumer :stopped)
        t)
    (bt:timeout ()
      (log:error "Timed out stopping consumer ~a" (consumer-name consumer))
      nil)))

(defun consumer-ready-p (consumer)
  "Return true only when the parent and every worker are running."
  (let ((workers (or (consumer-worker-instances consumer)
                     (list consumer))))
    (and (eq :running (consumer-state consumer))
         (consumer-running-p consumer)
         (every (lambda (worker)
                  (and (eq :running (consumer-state worker))
                       (consumer-running-p worker)))
                workers))))

(in-package :star.actors)

(defvar *gc-timer* nil)

(defun start-actors (&key rabbit-user rabbit-host rabbit-password rabbit-vhost rabbit-port)
  "Start the actor runtime and retain every process-owned timer/system handle."
  (start-actor-system)
  (setf *producer-agent*
        (make-producer-agent
         (star.producers:make-producer
          :name "actor-producer"
          :exchange-name "documents"
          :host rabbit-host
          :port rabbit-port
          :user rabbit-user
          :password rabbit-password
          :vhost rabbit-vhost)
         *sys*))
  (setf *gc-timer* (wt:make-wheel-timer))
  (wt:schedule-recurring
   *gc-timer*
   1
   3600
   (lambda ()
     #+sbcl (sb-ext:gc :full t)))
  (start-couchdb-agent *sys*)
  (start-actor-index *sys*)
  (start-couchdb-gets *sys*)
  (start-couchdb-inserts *sys*)
  (start-target-timer)
  (start-target-actor *sys*)
  (nhooks:run-hook star:*actors-start-hook*)
  *sys*)

(defun stop-actors (&key (timeout-seconds 5))
  "Stop process-owned actor resources without touching unrelated Lisp threads."
  (when *producer-agent*
    (handler-case
        (bt:with-timeout (timeout-seconds)
          (agent-get
           *producer-agent*
           (lambda (producer)
             (when (star.producers:producer-open-p producer)
               (star.producers:destroy producer))
             t)))
      (condition (condition)
        (log:warn "Producer shutdown failed: ~a" condition))))
  (dolist (timer (list *target-timer* *gc-timer*))
    (when timer
      (handler-case
          (wt:shutdown-wheel-timer timer)
        (condition (condition)
          (log:warn "Timer shutdown failed: ~a" condition)))))
  (when *sys*
    (handler-case
        (bt:with-timeout (timeout-seconds)
          (sento.actor-context:shutdown *sys* :wait t))
      (condition (condition)
        (log:warn "Actor-system shutdown failed: ~a" condition))))
  (setf *producer-agent* nil
        *targets* nil
        *target-timer* nil
        *gc-timer* nil
        *actor-index-agent* nil
        *couchdb-agent* nil
        *couchdb-gets* nil
        *couchdb-inserts* nil
        *sys* nil)
  t)

(in-package :star.frontends.http-api)

(defvar *http-server* nil)

(defun start-http-api ()
  "Start HTTP and retain the Clack handle. Rabbit publication is actor-owned."
  (when *http-server*
    (error "HTTP API is already running"))
  (log:info "Starting HTTP API server")
  (log:info "Server configuration - address: ~a port: ~a"
            star:*http-api-address*
            star:*http-api-port*)
  (log:info "Hunchentoot threading - max threads: 50, max accept: 100")
  (setf *http-server*
        (clack:clackup
         *server*
         :server :hunchentoot
         :address star:*http-api-address*
         :port star:*http-api-port*
         :max-thread-count 50
         :max-accept-count 100
         :request-timeout 300))
  (log:info "HTTP API server started successfully on ~a:~a"
            star:*http-api-address*
            star:*http-api-port*)
  *http-server*)

(defun stop-http-api (&key (timeout-seconds 5))
  "Stop the HTTP server represented by the retained Clack handle."
  (when *http-server*
    (handler-case
        (bt:with-timeout (timeout-seconds)
          (clack:stop *http-server*))
      (condition (condition)
        (log:warn "HTTP shutdown failed: ~a" condition)))
    (setf *http-server* nil))
  t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (uiop:define-package :star.runtime
    (:use :cl)
    (:export
     #:*runtime*
     #:star-runtime
     #:star-runtime-state
     #:star-runtime-consumers
     #:star-runtime-event-consumer
     #:star-runtime-http-server
     #:star-runtime-actor-system
     #:star-runtime-kernel
     #:star-runtime-started-at
     #:star-runtime-stop-reason
     #:start-runtime
     #:stop-runtime
     #:run-runtime-loop
     #:runtime-live-p
     #:runtime-ready-p
     #:runtime-readiness-snapshot
     #:runtime-live-json
     #:runtime-readiness-json)))

(in-package :star.runtime)

(defparameter *shutdown-timeout-seconds*
  (star::environment-integer "STAR_SHUTDOWN_TIMEOUT_SECONDS" 5))

(defvar *runtime* nil)
(defvar *signal-stop-requested* nil)
(defvar *signal-handlers-installed-p* nil)

(defstruct (star-runtime
             (:constructor %make-star-runtime
                 (&key
                    (state :created)
                    actor-system
                    consumers
                    event-consumer
                    http-server
                    kernel
                    started-at
                    stop-reason
                    (lock (bt:make-lock "star-runtime")))))
  state
  actor-system
  consumers
  event-consumer
  http-server
  kernel
  started-at
  stop-reason
  lock)

(defun default-couchdb-readiness-probe ()
  (handler-case
      (multiple-value-bind (body status)
          (dex:get
           (format nil "~a://~a:~d/_up"
                   star:*couchdb-scheme*
                   star:*couchdb-host*
                   star:*couchdb-port*)
           :connect-timeout 1
           :read-timeout 1
           :use-connection-pool nil)
        (declare (ignore body))
        (= 200 status))
    (condition () nil)))

(defun default-rabbit-readiness-probe ()
  (and star.actors:*producer-agent*
       (handler-case
           (bt:with-timeout (1)
             (sento.agent:agent-get
              star.actors:*producer-agent*
              (lambda (producer)
                (star.producers:producer-open-p producer))))
         (condition () nil))))

(defparameter *couchdb-readiness-probe* #'default-couchdb-readiness-probe)
(defparameter *rabbit-readiness-probe* #'default-rabbit-readiness-probe)

(defun runtime-consumers (runtime)
  (append
   (copy-list (star-runtime-consumers runtime))
   (when (star-runtime-event-consumer runtime)
     (list (star-runtime-event-consumer runtime)))))

(defun runtime-consumers-ready-p (runtime)
  (let ((consumers (runtime-consumers runtime)))
    (and consumers
         (every #'star.consumers::consumer-ready-p consumers))))

(defun runtime-readiness-snapshot (&optional (runtime *runtime*))
  (if (null runtime)
      (list :state :stopped
            :couchdb nil
            :rabbit nil
            :consumers nil
            :actors nil
            :kernel nil
            :http nil
            :ready nil)
      (let* ((state (star-runtime-state runtime))
             (couchdb (and (eq :running state)
                           (funcall *couchdb-readiness-probe*)))
             (rabbit (and (eq :running state)
                          (funcall *rabbit-readiness-probe*)))
             (consumers (and (eq :running state)
                             (runtime-consumers-ready-p runtime)))
             (actors (and (eq :running state)
                          (star-runtime-actor-system runtime)
                          t))
             (kernel (and (eq :running state)
                          (star-runtime-kernel runtime)
                          t))
             (http (and (eq :running state)
                        (star-runtime-http-server runtime)
                        t)))
        (list :state state
              :couchdb couchdb
              :rabbit rabbit
              :consumers consumers
              :actors actors
              :kernel kernel
              :http http
              :ready (and couchdb rabbit consumers actors kernel http)))))

(defun runtime-live-p (&optional (runtime *runtime*))
  (and runtime
       (member (star-runtime-state runtime)
               '(:starting :running :stopping)
               :test #'eq)
       t))

(defun runtime-ready-p (&optional (runtime *runtime*))
  (getf (runtime-readiness-snapshot runtime) :ready))

(defun state-string (state)
  (string-downcase (symbol-name state)))

(defun availability-string (value)
  (if value "ready" "unavailable"))

(defun runtime-live-json (&optional (runtime *runtime*))
  (jsown:to-json
   (jsown:new-js
    ("status" (if (runtime-live-p runtime) "live" "stopped"))
    ("state" (state-string
              (if runtime
                  (star-runtime-state runtime)
                  :stopped))))))

(defun runtime-readiness-json (&optional (runtime *runtime*))
  (let* ((snapshot (runtime-readiness-snapshot runtime))
         (ready (getf snapshot :ready)))
    (jsown:to-json
     (jsown:new-js
      ("status" (if ready "ready" "unready"))
      ("state" (state-string (getf snapshot :state)))
      ("couchdb" (availability-string (getf snapshot :couchdb)))
      ("rabbit" (availability-string (getf snapshot :rabbit)))
      ("consumers" (availability-string (getf snapshot :consumers)))
      ("actors" (availability-string (getf snapshot :actors)))
      ("kernel" (availability-string (getf snapshot :kernel)))
      ("http" (availability-string (getf snapshot :http)))))))

(defun install-runtime-signal-handlers ()
  (unless *signal-handlers-installed-p*
    #+sbcl
    (progn
      (sb-sys:enable-interrupt
       sb-unix:sigterm
       (lambda (&rest ignored)
         (declare (ignore ignored))
         (setf *signal-stop-requested* :sigterm)))
      (sb-sys:enable-interrupt
       sb-unix:sigint
       (lambda (&rest ignored)
         (declare (ignore ignored))
         (setf *signal-stop-requested* :sigint))))
    (setf *signal-handlers-installed-p* t))
  t)

(defun stop-runtime-consumers (runtime)
  (let ((consumers (runtime-consumers runtime)))
    (dolist (consumer consumers)
      (star.consumers:stop-consumer consumer))
    (handler-case
        (bt:with-timeout (*shutdown-timeout-seconds*)
          (dolist (consumer consumers)
            (star.consumers::join-consumer-owned-threads consumer)
            (setf (star.consumers:consumer-running-p consumer) nil)
            (star.consumers:consumer-update-state consumer :stopped))
          t)
      (bt:timeout ()
        (log:error "Timed out waiting for Rabbit consumers to stop")
        nil))))

(defun runtime-mark-stopping (runtime reason)
  (bt:with-lock-held ((star-runtime-lock runtime))
    (case (star-runtime-state runtime)
      (:stopped nil)
      (:stopping nil)
      (otherwise
       (setf (star-runtime-state runtime) :stopping
             (star-runtime-stop-reason runtime) reason)
       t))))

(defun stop-runtime (&optional (runtime *runtime*) &key (reason :explicit-stop))
  "Stop one StarIntel runtime in dependency-safe order. Idempotent."
  (when (null runtime)
    (return-from stop-runtime t))
  (unless (runtime-mark-stopping runtime reason)
    (return-from stop-runtime
      (eq :stopped (star-runtime-state runtime))))
  (log:info "Stopping StarIntel runtime (reason=~a)" reason)
  (ignore-errors
    (star.frontends.http-api::stop-http-api
     :timeout-seconds *shutdown-timeout-seconds*))
  (setf (star-runtime-http-server runtime) nil)
  (stop-runtime-consumers runtime)
  (setf (star-runtime-consumers runtime) nil
        (star-runtime-event-consumer runtime) nil)
  (ignore-errors
    (star.actors::stop-actors
     :timeout-seconds *shutdown-timeout-seconds*))
  (setf (star-runtime-actor-system runtime) nil)
  (ignore-errors
    (star.authorization:close-target-lease-service))
  (when (and (star-runtime-kernel runtime)
             (eq (star-runtime-kernel runtime) lparallel:*kernel*))
    (handler-case
        (bt:with-timeout (*shutdown-timeout-seconds*)
          (lparallel:end-kernel :wait t))
      (condition (condition)
        (log:warn "lparallel shutdown failed: ~a" condition))))
  (setf (star-runtime-kernel runtime) nil)
  (bt:with-lock-held ((star-runtime-lock runtime))
    (setf (star-runtime-state runtime) :stopped))
  (when (eq runtime *runtime*)
    (setf *runtime* nil))
  (log:info "StarIntel runtime stopped")
  t)

(defun start-runtime (init-file)
  "Start the owned StarIntel runtime, rolling back on partial failure."
  (when (and *runtime*
             (not (eq :stopped (star-runtime-state *runtime*))))
    (error "StarIntel runtime is already active"))
  (let ((runtime (%make-star-runtime
                  :state :starting
                  :started-at (get-universal-time))))
    (setf *runtime* runtime
          *signal-stop-requested* nil)
    (install-runtime-signal-handlers)
    (handler-case
        (progn
          (star:safe-load-init init-file)
          (log:info "Creating ~a worker threads" star:*ingest-workers*)
          (setf lparallel:*kernel*
                (lparallel:make-kernel star:*ingest-workers*)
                (star-runtime-kernel runtime)
                lparallel:*kernel*)
          (star.databases.couchdb:init-db)
          (star.auth:initialize-auth-store)
          (star.auth:ensure-initial-user)
          (star.authorization:initialize-target-lease-service)
          (setf (star-runtime-actor-system runtime)
                (star.actors:start-actors
                 :rabbit-host star:*rabbit-address*
                 :rabbit-vhost "/"
                 :rabbit-port star:*rabbit-port*
                 :rabbit-user star:*rabbit-user*
                 :rabbit-password star:*rabbit-password*))
          (setf (star-runtime-consumers runtime)
                (star.rabbit:start-consumers))
          (setf (star-runtime-event-consumer runtime)
                (star.actors:start-event-consumer 2))
          (setf (star-runtime-http-server runtime)
                (star.frontends.http-api::start-http-api))
          (bt:with-lock-held ((star-runtime-lock runtime))
            (setf (star-runtime-state runtime) :running))
          (log:info "StarIntel runtime is ready to serve")
          runtime)
      (condition (condition)
        (log:error "StarIntel startup failed: ~a" condition)
        (ignore-errors
          (stop-runtime runtime :reason :startup-failure))
        (error condition)))))

(defun run-runtime-loop (&optional (runtime *runtime*))
  "Keep the owning main thread alive until an explicit/signal stop."
  (unless runtime
    (error "No StarIntel runtime to run"))
  (loop while (member (star-runtime-state runtime)
                      '(:starting :running)
                      :test #'eq)
        do (when *signal-stop-requested*
             (let ((reason *signal-stop-requested*))
               (setf *signal-stop-requested* nil)
               (stop-runtime runtime :reason reason)))
           (sleep 0.1))
  runtime)

(in-package :star.frontends.http-api)

(pushnew "/live" star:*auth-public-paths* :test #'string=)
(pushnew "/ready" star:*auth-public-paths* :test #'string=)

(setf (ningle:route *app* "/live" :method :get)
      (lambda (params)
        (declare (ignore params))
        (set-default-headers)
        (unless (star.runtime:runtime-live-p)
          (setf (lack.response:response-status *response*) 503))
        (star.runtime:runtime-live-json)))

(setf (ningle:route *app* "/ready" :method :get)
      (lambda (params)
        (declare (ignore params))
        (set-default-headers)
        (unless (star.runtime:runtime-ready-p)
          (setf (lack.response:response-status *response*) 503))
        (star.runtime:runtime-readiness-json)))

(setf (ningle:route *app* "/health" :method :get)
      (lambda (params)
        (declare (ignore params))
        (set-default-headers)
        (unless (star.runtime:runtime-ready-p)
          (setf (lack.response:response-status *response*) 503))
        (star.runtime:runtime-readiness-json)))