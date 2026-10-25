(defpackage :star.router.sc01-application.tests (:use :cl :fiveam))
(in-package :star.router.sc01-application.tests)
(def-suite :router-sc01-application)
(in-suite :router-sc01-application)

(defun key (&optional (byte 9))
  (make-array 32 :element-type '(unsigned-byte 8) :initial-element byte))
(defun context (&optional (write t))
  (star.auth::%make-service-call-context
   :principal-id "sc01-fixture" :principal-type "api_client" :credential-id "fixture-key"
   :scopes (append '("tenant:default" "dataset:fixture")
                   (list (if write "documents:write" "documents:read")))))
(defun binding (&optional (context (context)))
  (star.router.sc01-application:make-binding
   (key) "fixture.actor" "fixture-instance"
   (lambda (authenticated-key) (declare (ignore authenticated-key)) context)))
(defun frames (&key (event "7") (source "fixture-instance") (topic "fixture.actor")
                    (body ""))
  (mapcar (lambda (value) (babel:string-to-octets value :encoding :utf-8))
          (list "untrusted-route" "" "SC01" source "fixture-message" "1" event topic body)))
(defun document ()
  "{\"id\":\"fixture:sc01\",\"schemaVersion\":\"0.10.1\",\"dtype\":\"person\",\"dataset\":\"fixture\",\"fname\":\"Synthetic\"}")

(test pinned-key-source-and-actor-before-registration
  (signals star.router.application:application-rejection
    (star.router.sc01-application:process-event (binding) (frames) nil))
  (signals star.router.application:application-rejection
    (star.router.sc01-application:process-event (binding) (frames) (key 8)))
  (signals star.router.application:application-rejection
    (star.router.sc01-application:process-event (binding) (frames :source "spoof") (key)))
  (signals star.router.application:application-rejection
    (star.router.sc01-application:process-event (binding) (frames :topic "admin") (key)))
  (let ((result (star.router.sc01-application:process-event (binding) (frames) (key))))
    (is (eq :control-authorized (getf result :status)))
    (is (null (getf result :durable-p)))))

(test registration-rechecks-expiry-and-unsupported-operations-fail
  (let ((expired (star.auth::%make-service-call-context
                  :principal-id "fixture" :deadline (1- (get-universal-time)))))
    (signals star.router.application:application-rejection
      (star.router.sc01-application:process-event (binding expired) (frames) (key))))
  (dolist (event '("1" "2" "4" "5" "6" "8"))
    (signals star.router.application:application-rejection
      (star.router.sc01-application:process-event (binding) (frames :event event) (key)))))

(test canonical-validation-and-authorization-before-storage
  (signals star.frontends.http-api:http-input-error
    (star.router.sc01-application:process-event
     (binding) (frames :event "3" :body "{\"unexpected\":true}") (key)))
  (signals star.authorization:authorization-error
    (star.router.sc01-application:process-event
     (binding (context nil)) (frames :event "3" :body (document)) (key))))

(test persistence-failure-cannot-return-committed
  ;; Only the external persistence effect is replaced. Auth/schema remain real.
  (let* ((symbol 'star.rabbit::persist-rabbit-document-mutation)
         (original (symbol-function symbol)) (calls 0))
    (unwind-protect
         (progn
           (setf (symbol-function symbol)
                 (lambda (document operation)
                   (declare (ignore document operation))
                   (incf calls)
                   (error "synthetic storage failure")))
           (signals error
             (star.router.sc01-application:process-event
              (binding) (frames :event "3" :body (document)) (key)))
           (is (= 1 calls)))
      (setf (symbol-function symbol) original))))

(test rejected-event-sends-no-acceptance
  ;; External native receive/settle ports only; this is not native E2E evidence.
  (let* ((receive 'star.router:receive-sc01) (settle 'star.router:settle-sc01)
         (old-receive (symbol-function receive)) (old-settle (symbol-function settle))
         (auth (star.router::%make-authenticator
                :token 1 :context :fixture-context :keys (list (key))))
         (router (star.router::%make-sc01-router
                  :secure t :context :fixture-context :authenticator auth))
         (settlements nil))
    (unwind-protect
         (progn
           (setf (symbol-function receive)
                 (lambda (router) (declare (ignore router))
                   (values (frames :source "spoof") (key) :received))
                 (symbol-function settle)
                 (lambda (router accepted) (declare (ignore router))
                   (push accepted settlements)))
           (signals star.router.application:application-rejection
             (star.router.sc01-application:process-once (binding) router))
           (is (equal '(nil) settlements)))
      (setf (symbol-function receive) old-receive (symbol-function settle) old-settle))))

(test sc01-pub-requires-one-matching-enrolled-key
  ;; Inert configuration objects exercise policy without starting native sockets.
  (dolist (keys (list (list (key) (key 8)) (list (key 8)) (list (key))))
    (let* ((auth (star.router::%make-authenticator
                  :token 1 :context :fixture-context :keys keys))
           (router (star.router::%make-sc01-router
                    :secure t :context :fixture-context :authenticator auth)))
      (if (and (= 1 (length keys)) (equalp (key) (first keys)))
          (is (null (star.router.sc01-application::require-private-router (binding) router)))
          (signals star.router.application:application-rejection
            (star.router.sc01-application::require-private-router (binding) router))))))
