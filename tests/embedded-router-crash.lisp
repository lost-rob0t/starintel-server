;;; SIGKILL checkpoints for real Couch-backed lifecycle recovery. No mock store.
(in-package :embedded-router-application-test)

(defun fixture-journal ()
  (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*)
    (star.router.journal:make-couchdb-journal-store client star:*couchdb-default-database*)))

(defun crash-stage () (uiop:getenv "STAR_ROUTER_E2E_CRASH_STAGE"))
(defun crash-resuming-p () (equal "crash-resume" (uiop:getenv "STAR_ROUTER_E2E_MODE")))

(defun crash-checkpoint (stage)
  (when (and (not (crash-resuming-p))
             (equal (string-downcase (symbol-name stage)) (crash-stage)))
    (marker "crash-checkpoint")
    ;; Only Python's SIGKILL completes this path. A graceful stop or timeout is
    ;; a failed fixture, never evidence of process-crash recovery.
    (wait-marker "never-release-crash" 60)
    (error "Crash checkpoint was unexpectedly released")))

(defun persisted-keypair (prefix)
  (let ((public-file (concatenate 'string prefix "-public"))
        (secret-file (concatenate 'string prefix "-secret")))
    (if (plusp (length (uiop:read-file-string (state-path public-file))))
        (values (star.auth::decode-hex (uiop:read-file-string (state-path public-file)))
                (star.auth::decode-hex (uiop:read-file-string (state-path secret-file))))
        (multiple-value-bind (public secret) (star.router:keypair)
          (flet ((hex (bytes) (format nil "~(~{~2,'0X~}~)" (coerce bytes 'list))))
            (write-key public-file (hex public)) (write-key secret-file (hex secret)))
          (values public secret)))))

(defun crash-principal ()
  (let ((star.auth:*request-security-context*
          (star.auth:authenticate-api-key
           (uiop:read-file-string (state-path "api-key")) "crash-session" (+ (get-universal-time) 600))))
    (star.auth:current-service-call-context)))

(defun crash-actor (context endpoint manifest public secret server-public)
  (let ((socket (star-zmq:open-socket context :dealer :identity *route* :timeout-ms 10000)))
    (unwind-protect
         (handler-case
             (progn
               (star.router:configure-curve-client socket public secret server-public)
               (star-zmq:connect-endpoint socket endpoint)
               (if (crash-resuming-p)
                   (progn
                     ;; Exact byte replay, retained before the first attempt.
                     (star-zmq:send-frames socket
                       (list (octets (uiop:read-file-string (state-path "crash-command.json")))))
                     (let ((outputs nil))
                       (loop repeat (if (equal (crash-stage) "terminal") 2 1)
                             do (push (babel:octets-to-string (first (star-zmq:receive-frames socket))
                                                              :encoding :utf-8) outputs))
                       (write-key "crash-observed.json" (jsown:to-json (coerce (nreverse outputs) 'vector))))
                     (marker "actor-done"))
                   (progn
                     (star-zmq:send-frames socket
                       (list (star-actor-wire:encode-binding-message :zmq *profiles* manifest
                                (pressure-event "crash-ready"))))
                     (let* ((target (star-actor-wire:decode-binding-message
                                     :zmq *profiles* manifest (first (star-zmq:receive-frames socket))))
                            (document (star-actor-wire:decode-json-value
                                       (octets (uiop:read-file-string (state-path "document.json")))))
                            (command (document-command target document :suffix (crash-stage)))
                            (bytes (star-actor-wire:encode-binding-message :zmq *profiles* manifest command)))
                       (write-key "crash-command.json" (babel:octets-to-string bytes :encoding :utf-8))
                       (star-zmq:send-frames socket (list bytes)))))
               (loop until (probe-file (state-path "stop")) do (sleep 0.05)))
           (error (condition)
             (format *error-output* "~&Crash actor fixture failed: ~A~%" (type-of condition))
             (marker "actor-error")))
      (star-zmq:close-socket socket))))

(defun exercise-crash (context)
  (let* ((endpoint (format nil "tcp://127.0.0.1:~A" (uiop:getenv "STAR_ROUTER_E2E_PORT")))
         (manifest (fixture-manifest endpoint))
         (binding (star.router.application:make-document-binding manifest "star.router.fixture/document@1" *actor*))
         (host nil) (authenticator nil) (actor-thread nil) (lifecycle nil)
         (journal (fixture-journal)))
    (when (zerop (length (uiop:read-file-string (state-path "api-key")))) (provision-context))
    (multiple-value-bind (server-public server-secret) (persisted-keypair "crash-server")
      (multiple-value-bind (client-public client-secret) (persisted-keypair "crash-client")
        (unwind-protect
             (progn
               (setf authenticator (star.router:start-authenticator context (list client-public))
                     host
                     (star.router:start-host
                      context endpoint manifest
                      (lambda (route envelope key)
                        (declare (ignore route))
                        (unless lifecycle
                          (setf lifecycle
                                (star.router.lifecycle:make-runtime-binding
                                 manifest *actor*
                                 (lambda (principal command)
                                   (when (crash-resuming-p)
                                     (marker "unexpected-effect")
                                     (error "Recovery must not repeat unknown or terminal effects"))
                                   (star.router.application:commit-document-command
                                    binding principal (star-actor-wire:encode-binding-message :zmq *profiles* manifest command))
                                   (crash-checkpoint :effect)
                                   (starlangruntime:complete-dispatch
                                    :message-type "star.router.fixture/result@1"
                                    :payload (list (cons "status" "committed") (cons "durable" t))))
                                 (lambda (peer) (when (equalp peer client-public) (crash-principal)))
                                 :journal journal
                                 :durable-p t
                                 :journal-checkpoint (lambda (stage command)
                                                       (when (eq stage :terminal)
                                                         (let* ((principal (crash-principal))
                                                                (key (star.router.lifecycle:runtime-journal-key
                                                                      lifecycle principal command))
                                                                (row (star.router.journal:journal-read journal key)))
                                                           (write-key "crash-expected.json"
                                                                      (jsown:to-json (jsown:val row "outcomeWires")))))
                                                       (crash-checkpoint stage)))))
                        (if (eq :event (getf envelope :kind))
                            (progn (bordeaux-threads:with-lock-held (*lock*) (setf *peer-ready* t)) nil)
                            (star.router.lifecycle:runtime-handle-envelope lifecycle envelope key)))
                      :server-secret-key server-secret :authenticator authenticator))
               (when (crash-resuming-p) (enroll-fixture-actor))
               (unless (crash-resuming-p)
                 (bind-fixture-target host manifest client-public #'crash-principal))
               (setf actor-thread
                     (bordeaux-threads:make-thread
                      (lambda () (crash-actor context endpoint manifest client-public client-secret server-public))
                      :name "crash recovery actor"))
               (unless (crash-resuming-p)
                 (let ((deadline (+ (get-internal-real-time) (* 15 internal-time-units-per-second))))
                   (loop until (bordeaux-threads:with-lock-held (*lock*) *peer-ready*)
                         do (when (> (get-internal-real-time) deadline) (error "Crash actor readiness"))
                            (sleep 0.01))))
               (marker "ready")
               (loop until (probe-file (state-path "stop")) do (sleep 0.05)))
          (star.router.server:unbind-target-route *actor*)
          (setf *fixture-enrollment* nil)
          (marker "stop")
          (when actor-thread (bordeaux-threads:join-thread actor-thread))
          (when host (star.router:stop-host host))
          (when authenticator (star.router:close-authenticator authenticator)))))))
