;;; Real disposable application fixture. Started only by the Python driver.
(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(uiop:ensure-directory-pathname (uiop:getenv "STAR_ROUTER_TEST_ROOT")))
   (:tree ,(uiop:ensure-directory-pathname (uiop:getenv "STAR_LANG_SOURCE")))
   :inherit-configuration))
(asdf:load-system "starintel-router-server")
(asdf:test-system "starintel-router-server")
(asdf:load-system "starlang-compiler")

(defpackage :embedded-router-application-test (:use :cl))
(in-package :embedded-router-application-test)
(defparameter *state* (uiop:ensure-directory-pathname (uiop:getenv "STAR_ROUTER_E2E_STATE")))
(defparameter *run* (uiop:getenv "STAR_ROUTER_E2E_RUN"))
(defparameter *dataset* (uiop:getenv "STAR_ROUTER_E2E_DATASET"))
(defparameter *actor* "star:v1:collector:embedded-fixture")
(defparameter *profiles* '("star.actor2actor/1"))
(defparameter *route* (babel:string-to-octets "synthetic-actor" :encoding :utf-8))
(defvar *lock* (bordeaux-threads:make-lock "embedded fixture state"))
(defvar *peer-ready* nil)
(defvar *handler-calls* 0)

(defun state-path (name) (merge-pathnames name *state*))
(defun marker (name)
  (with-open-file (stream (state-path name) :direction :output :if-exists :supersede)
    (write-line "complete" stream)))
(defun fixture-assert (value label) (unless value (error "Fixture assertion failed: ~A" label)))
(defun octets (text) (babel:string-to-octets text :encoding :utf-8))

(defun fixture-manifest (endpoint)
  ;; The compiler emits this test-only manifest. Lifecycle/wire parsing is never
  ;; implemented here, and production document types remain generated upstream.
  (starlangcompiler:emit-portable-manifest
   '(:kind :spec-library :name "star.router.fixture@1" :version "1.0.0"
     :digest "sha256:embedded-router-synthetic-fixture" :imports ()
     :declarations
     ((:kind :message :name "target" :qualified-name "star.router.fixture/target@1"
       :fields ((:name "target" :type "any" :required t)))
      (:kind :message :name "document" :qualified-name "star.router.fixture/document@1"
       :fields ((:name "document" :type "any" :required t)))
      (:kind :message :name "ready" :qualified-name "star.router.fixture/ready@1" :fields ())
      (:kind :message :name "result" :qualified-name "star.router.fixture/result@1"
       :fields ((:name "status" :type "string" :required t)
                (:name "durable" :type "boolean" :required t)))))
   (list
    (starlangcompiler:compile-actor-source
     (format nil "(actor ~A (:runtime external :service-uri ~S :protocol star-message-v1 :endpoint ~S :accepts (star.router.fixture/target@1 star.router.fixture/document@1) :produces (star.router.fixture/document@1 star.router.fixture/result@1) :restart permanent :mailbox (bounded 4)))"
             *actor* (concatenate 'string "star://fixture/actor/" *actor*) endpoint)
     :source-id "embedded-router-application-fixture.star"))))

(defun write-key (name value)
  ;; Truncate each checkpoint: later canonical results can be shorter. Keep
  ;; replacement files private as well as their containing directory.
  (with-open-file (stream (state-path name) :direction :output :if-exists :supersede
                                          :if-does-not-exist :error)
    (fixture-assert (zerop (cffi:foreign-funcall "chmod" :string (namestring (state-path name))
                                                       :unsigned-int #o600 :int))
                    "private fixture file permissions")
    (write-string value stream)))

(defun provision-context ()
  (let ((scopes (list "documents:read" "documents:write" "targets:dispatch" "targets:lease" "targets:read"
                      "actor:*" (concatenate 'string "dataset:" *dataset*)
                      (concatenate 'string "tenant:" *dataset*))))
    (multiple-value-bind (record key) (star.auth:create-api-key "embedded-fixture" "service" scopes)
      (declare (ignore record))
      (write-key "api-key" key)
      (multiple-value-bind (denied-record denied-key)
          (star.auth:create-api-key "embedded-denied" "service"
                                   '("documents:read" "dataset:forbidden" "tenant:forbidden"))
        (declare (ignore denied-record))
        (write-key "denied-key" denied-key))
      ;; This authenticated context is bound to the allowed CURVE public key by
      ;; trusted fixture configuration. ROUTER identity supplies no authority.
      (let ((star.auth:*request-security-context*
              (star.auth:authenticate-api-key key "fixture-session" (+ (get-universal-time) 600))))
        (star.auth:current-service-call-context)))))

(defun target-command (dispatch)
  (let* ((document (star.actors::target-dispatch-document dispatch))
         (portable (star-actor-wire:decode-json-value (octets (star.documents:document-json document))))
         (execution (star.actors::target-dispatch-envelope-execution-id dispatch)))
    (staractorprotocol:make-command-envelope
     :message-id execution :message-type "star.router.fixture/target@1"
     :attempt (star.router.leases:canonical-attempt dispatch)
     :actor *actor* :sender "fixture-server" :reply-to "fixture-server"
     :idempotency-key execution :correlation-id (star.actors::target-dispatch-envelope-trace-id dispatch)
     :causation-id (star.actors:target-record-id
                    (star.actors:target-dispatch-envelope-record dispatch))
     :dataset *dataset* :payload (list (cons "target" portable)))))

(defun roundtrip (socket manifest command)
  (star-zmq:send-frames socket
    (list (star-actor-wire:encode-binding-message :zmq *profiles* manifest command)))
  (let ((result nil))
    (loop repeat 8
          for reply = (star-actor-wire:decode-binding-message
                       :zmq *profiles* manifest (first (star-zmq:receive-frames socket)))
          do (fixture-assert (equal (getf reply :correlation-id) (getf command :correlation-id)) "reply correlation")
             (fixture-assert (equal (getf reply :causation-id) (getf command :message-id)) "reply causation")
             (case (getf reply :kind)
               (:error (return-from roundtrip reply))
               (:reply (setf result reply))
               (:ack
                (when (eq :completed (getf (getf reply :payload) :status))
                  (fixture-assert result "completed lifecycle has a result")
                  (return-from roundtrip result)))
               (otherwise (error "Unexpected lifecycle output"))))
    (error "Canonical lifecycle did not terminate within its output bound")))

(defun document-command (target document &key (suffix "document") (dataset *dataset*) deadline)
  (staractorprotocol:make-command-envelope
   :message-id (concatenate 'string *run* ":" suffix)
   :message-type "star.router.fixture/document@1" :actor *actor* :sender *actor*
   :reply-to "fixture-server" :idempotency-key (concatenate 'string *run* ":" suffix)
   :correlation-id (getf target :correlation-id) :causation-id (getf target :message-id)
   :dataset dataset :deadline deadline :payload (list (cons "document" document))))

(defun reject-unlisted-peer (context endpoint manifest server-public)
  (multiple-value-bind (public secret) (star.router:keypair)
    (let ((socket (star-zmq:open-socket context :dealer :identity *route* :timeout-ms 300))
          (denied nil))
      (unwind-protect
           (progn
             (star.router:configure-curve-client socket public secret server-public)
             (star-zmq:connect-endpoint socket endpoint)
             (handler-case
                 (progn
                   (star-zmq:send-frames socket
                     (list (star-actor-wire:encode-binding-message
                            :zmq *profiles* manifest
                            (staractorprotocol:make-event-envelope
                             :message-id (concatenate 'string *run* ":spoofed-ready")
                             :message-type "star.router.fixture/ready@1" :actor *actor* :sender *actor*
                             :dataset *dataset* :payload nil))))
                   (star-zmq:receive-frames socket))
               (star-zmq:zmq-error (condition)
                 (unless (= 11 (star-zmq:zmq-error-code condition)) (error condition))
                 (setf denied t)))
             (fixture-assert denied "unlisted CURVE key could not exchange application messages")
             (fixture-assert (bordeaux-threads:with-lock-held (*lock*) (zerop *handler-calls*))
                             "spoofed ROUTER identity invoked no handler"))
        (star-zmq:close-socket socket)))))

(defun synthetic-actor (context endpoint manifest public secret server-public)
  (let ((socket nil))
    (unwind-protect
         (handler-case
             (progn
               (setf socket (star-zmq:open-socket context :dealer :identity *route* :timeout-ms 5000))
               (star.router:configure-curve-client socket public secret server-public)
               (star-zmq:connect-endpoint socket endpoint)
               (star-zmq:send-frames socket
                 (list (star-actor-wire:encode-binding-message
                        :zmq *profiles* manifest
                        (staractorprotocol:make-event-envelope
                         :message-id (concatenate 'string *run* ":ready")
                         :message-type "star.router.fixture/ready@1" :actor *actor* :sender *actor*
                         :dataset *dataset* :payload nil))))
               (let* ((target (star-actor-wire:decode-binding-message
                               :zmq *profiles* manifest (first (star-zmq:receive-frames socket))))
                      (document (star-actor-wire:decode-json-value
                                 (octets (uiop:read-file-string (state-path "document.json")))))
                      (command (document-command target document)))
                 (fixture-assert (equal (getf target :message-type) "star.router.fixture/target@1") "target command")
                 (fixture-assert (equal (getf target :dataset) *dataset*) "target scope")
                 (dotimes (attempt 2)
                   (declare (ignorable attempt))
                   (let ((reply (roundtrip socket manifest command)))
                     (fixture-assert (eq :reply (getf reply :kind)) "committed reply kind")
                     (fixture-assert (equal "committed" (cdr (assoc "status" (getf reply :payload) :test #'equal))) "commit status")
                     (fixture-assert (eq t (cdr (assoc "durable" (getf reply :payload) :test #'equal))) "durable application acknowledgment")))
                 (let ((changed (copy-tree document)))
                   (setf (cdr (assoc "mission" changed :test #'equal)) "Conflicting synthetic replay")
                   (fixture-assert
                    (eq :error (getf (roundtrip socket manifest (document-command target changed)) :kind))
                    "conflicting native lifecycle replay denied"))
                 (let ((denied (copy-tree document)))
                   (setf (cdr (assoc "dataset" denied :test #'equal)) "forbidden"
                         (cdr (assoc "id" denied :test #'equal)) (concatenate 'string *run* ":denied"))
                   (fixture-assert
                    (eq :error (getf (roundtrip socket manifest
                                      (document-command target denied :suffix "denied" :dataset "forbidden")) :kind))
                    "cross-dataset native command denied"))
                 (fixture-assert
                  (eq :error (getf (roundtrip socket manifest
                                    (document-command target document :suffix "expired"
                                                      :deadline "2000-01-01T00:00:00Z")) :kind))
                  "expired native command denied")
                 (let* ((cancelled-document (copy-tree document))
                        (cancelled nil))
                   (setf (cdr (assoc "id" cancelled-document :test #'equal))
                         (concatenate 'string *run* ":cancelled")
                         cancelled (document-command target cancelled-document :suffix "cancelled"))
                   ;; Canonical cancel-before-command produces no fabricated
                   ;; receipt; the later command must terminate as cancelled.
                   (star-zmq:send-frames socket
                     (list (star-actor-wire:encode-binding-message
                            :zmq *profiles* manifest
                            (staractorprotocol:make-cancel-envelope
                             cancelled :message-id (concatenate 'string *run* ":cancel")
                             :actor *actor* :sender *actor* :reason "synthetic cancellation"))))
                   (fixture-assert
                    (eq :error (getf (roundtrip socket manifest cancelled) :kind))
                    "cancelled command produced no successful effect"))
                 (actor-target-outcomes socket manifest target)
                 (actor-pressure socket manifest)
                 ;; Reconnect with a different routing identity while retaining
                 ;; the same authenticated key and canonical replay identity.
                 (star-zmq:close-socket socket)
                 (setf socket (star-zmq:open-socket context :dealer
                                                   :identity (octets "synthetic-reconnected") :timeout-ms 5000))
                 (star.router:configure-curve-client socket public secret server-public)
                 (star-zmq:connect-endpoint socket endpoint)
                 (fixture-assert (eq :reply (getf (roundtrip socket manifest command) :kind))
                                 "authenticated reconnect replay")
                 (actor-revocation socket manifest target document command)
                 (marker "actor-done"))
               (loop until (probe-file (state-path "stop")) do (sleep 0.05)))
           (error (condition)
             (format *error-output* "~&Synthetic actor failed: ~A~%" (type-of condition))
             (marker "actor-error")))
      (when socket (star-zmq:close-socket socket)))))

(defun exercise (context)
  (let* ((endpoint (format nil "tcp://127.0.0.1:~A" (uiop:getenv "STAR_ROUTER_E2E_PORT")))
         (manifest (fixture-manifest endpoint))
         (_http-context (provision-context))
         (binding (star.router.application:make-document-binding
                   manifest "star.router.fixture/document@1" *actor*))
         (authenticator nil) (host nil) (actor-thread nil) (lifecycle nil))
    (declare (ignore _http-context))
    (multiple-value-bind (server-public server-secret) (star.router:keypair)
      (multiple-value-bind (client-public client-secret) (star.router:keypair)
        (unwind-protect
             (progn
               (provision-actor-credential)
               (setf authenticator (star.router:start-authenticator context (list client-public)))
               (setf host
                     (star.router:start-host
                      context endpoint manifest
                      (lambda (route envelope authenticated-key)
                        (declare (ignore route))
                        (unless lifecycle
                          (setf lifecycle
                                (star.router.lifecycle:make-runtime-binding
                                 manifest *actor*
                                 (lambda (principal command)
                                   (let ((result (star.router.application:commit-document-command
                                                  binding principal
                                                  (star-actor-wire:encode-binding-message
                                                   :zmq *profiles* manifest command))))
                                     (fixture-assert (eq :committed (getf result :status)) "application commit")
                                     (starlangruntime:complete-dispatch
                                      :message-type "star.router.fixture/result@1"
                                      :payload (list (cons "status" "committed") (cons "durable" t)))))
                                 (lambda (key)
                                   (when (equalp key client-public) (funcall *actor-authenticate*)))
                                 :journal (fixture-journal) :durable-p t)))
                        (fixture-assert (equalp authenticated-key client-public) "authenticated CURVE identity")
                        (bordeaux-threads:with-lock-held (*lock*) (incf *handler-calls*))
                        (if (equal (getf envelope :message-type) "star.router.fixture/ready@1")
                            (progn
                              (outbound-barrier-event envelope)
                              (when (equal (getf envelope :message-id)
                                           (getf (pressure-event "hold") :message-id))
                                (marker "pressure-entered")
                                (wait-marker "pressure-release"))
                              (bordeaux-threads:with-lock-held (*lock*) (setf *peer-ready* t)) nil)
                            (handler-case
                                (star.router.lifecycle:runtime-handle-envelope lifecycle envelope authenticated-key)
                              (error ()
                                ;; Test host owns this boundary mapping. The
                                ;; canonical dispatcher remains semantic owner.
                                (list (staractorprotocol:make-error-envelope
                                       envelope :message-id (star.ids:ulid) :actor *actor* :sender "fixture-server"
                                       :code "fixture_boundary_rejected" :message "Synthetic command rejected"
                                       :retryable nil))))))
                      :server-secret-key server-secret :authenticator authenticator :queue-limit 8))
               (reject-unlisted-peer context endpoint manifest server-public)
               (bind-fixture-target host manifest client-public *actor-authenticate*)
               (setf actor-thread (bordeaux-threads:make-thread
                                   (lambda () (synthetic-actor context endpoint manifest client-public client-secret server-public))
                                   :name "synthetic native actor"))
               (let ((deadline (+ (get-internal-real-time) (* 15 internal-time-units-per-second))))
                 (loop until (bordeaux-threads:with-lock-held (*lock*) *peer-ready*)
                       do (when (or (probe-file (state-path "actor-error"))
                                    (> (get-internal-real-time) deadline))
                            (error "Authenticated synthetic actor readiness failed"))
                          (sleep 0.01)))
               (marker "ready")
               (loop until (probe-file (state-path "stop"))
                     do (service-outbound-controls) (service-adversarial-controls host) (sleep 0.01)))
          (star.router.server:unbind-target-route *actor*)
          (setf *fixture-enrollment* nil)
          (marker "stop")
          (when actor-thread (bordeaux-threads:join-thread actor-thread))
          (when host (star.router:stop-host host))
          (when authenticator (star.router:close-authenticator authenticator)))))))

(load (merge-pathnames "tests/embedded-router-adversarial.lisp"
                       (uiop:ensure-directory-pathname (uiop:getenv "STAR_ROUTER_TEST_ROOT")))
      :verbose nil :print nil)

(load (merge-pathnames "tests/embedded-router-crash.lisp"
                       (uiop:ensure-directory-pathname (uiop:getenv "STAR_ROUTER_TEST_ROOT")))
      :verbose nil :print nil)

(load (merge-pathnames "tests/embedded-router-outbound.lisp"
                       (uiop:ensure-directory-pathname (uiop:getenv "STAR_ROUTER_TEST_ROOT")))
      :verbose nil :print nil)

(load (merge-pathnames "tests/embedded-router-sc01.lisp"
                       (uiop:ensure-directory-pathname (uiop:getenv "STAR_ROUTER_TEST_ROOT")))
      :verbose nil :print nil)

(handler-case
    (let ((runtime nil) (context nil))
      (unwind-protect
           (progn
             (setf runtime (star.runtime:start-runtime (state-path "init.lisp")))
             (cond
               ((equal (uiop:getenv "STAR_ROUTER_E2E_MODE") "exercise")
                (setf context (star-zmq:make-context)) (exercise context))
               ((member (uiop:getenv "STAR_ROUTER_E2E_MODE") '("crash-first" "crash-resume") :test #'equal)
                (setf context (star-zmq:make-context)) (exercise-crash context))
               ((equal (uiop:getenv "STAR_ROUTER_E2E_MODE") "sc01")
                (setf context (star-zmq:make-context)) (exercise-sc01 context))
               (t
                   (when (equal (uiop:getenv "STAR_ROUTER_E2E_MODE") "rabbit") (provision-context))
                   (marker "ready")
                   (loop until (probe-file (state-path "stop")) do (sleep 0.05)))))
        (when context (star-zmq:close-context context))
        (when runtime (star.runtime:stop-runtime runtime))))
  (error (condition)
    ;; Avoid debugger backtraces containing authenticated contexts/secret arrays.
    (format *error-output* "~&Application fixture failed: ~A~%" (type-of condition))
    (uiop:quit 1)))
