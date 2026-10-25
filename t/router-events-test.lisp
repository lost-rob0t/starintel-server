(defpackage :star.router.events.tests (:use :cl :fiveam))
(in-package :star.router.events.tests)
(def-suite :router-events)
(in-suite :router-events)

(defun manifest ()
  '(:messages ((:name "fixture.document-event@1"
                :fields ((:name "document" :type "map" :required t))))))
(defun context (&optional (scopes '("documents:write" "tenant:default" "dataset:fixture")))
  (star.auth::%make-service-call-context
   :principal-id "event-principal" :principal-type "api_client"
   :credential-id "event-credential" :scopes scopes))
(defun peer-key () (make-array 32 :element-type '(unsigned-byte 8) :initial-element 17))
(defun binding (authenticate)
  (star.router.events:make-document-event-binding
   (manifest) "fixture.document-event@1" "server.events" authenticate))
(defun event (&key (actor "server.events") (dataset "fixture") invalid)
  (staractorprotocol:make-event-envelope
   :message-id "event/1" :message-type "fixture.document-event@1" :actor actor
   :sender "untrusted-sender" :correlation-id "trace/1" :causation-id "origin/1"
   :dataset dataset
   :payload `(("document" . (("id" . "fixture:event-document") ("dataset" . "fixture")
                             ("dtype" . "person") ("schemaVersion" . "0.10.1")
                             (,(if invalid "unexpectedField" "fname") . "Synthetic"))))))

(test rejects-control-command-and-wrong-destination-before-authentication
  (let ((calls 0))
    (let ((binding (binding (lambda (key) (declare (ignore key)) (incf calls) (context)))))
      (signals star.router.application:application-rejection
        (star.router.events:commit-document-event binding (event :actor "other") (peer-key)))
      (let ((command (staractorprotocol:make-command-envelope
                      :message-id "command/1" :message-type "fixture.document-event@1"
                      :actor "server.events" :sender "untrusted" :dataset "fixture"
                      :idempotency-key "command/1" :reply-to "reply"
                      :payload (getf (event) :payload))))
        (signals star.router.application:application-rejection
          (star.router.events:commit-document-event binding command (peer-key))))
      (signals star.router.application:application-rejection
        (star.router.events:commit-document-event
         binding (staractorprotocol:make-ack-envelope
                  (event) :message-id "ack/1" :actor "server.events"
                  :sender "untrusted" :status :accepted) (peer-key)))
      (is (= 0 calls)))))

(test stream-frame-is-not-an-ingest-envelope
  ;; The canonical projection is an internal object, not a subscription protocol.
  (signals error
    (star.router.events:commit-document-event
     (binding (lambda (key) (declare (ignore key)) (context)))
     (staractorprotocol:make-actor2actor-stream-frame
      :task-id "task/1" :correlation-id "trace/1" :sequence 1 :envelope (event))
     (peer-key))))

(test authenticates-every-event-and-does-not-retain-revoked-context
  (let ((calls 0))
    (let ((binding
            (binding (lambda (key)
                       (is (equalp key (peer-key)))
                       (incf calls)
                       ;; Transport-key storage is private to this invocation.
                       (fill key 0)
                       (when (= calls 1) (context '("documents:read" "dataset:fixture" "tenant:default")))))))
      (signals star.authorization:authorization-error
        (star.router.events:commit-document-event binding (event) (peer-key)))
      (signals star.router.application:application-rejection
        (star.router.events:commit-document-event binding (event) (peer-key)))
      (is (= 2 calls)))))

(test rejects-invalid-schema-dataset-and-expired-authentication
  (let ((binding (binding (lambda (key) (declare (ignore key)) (context)))))
    (signals star.frontends.http-api:http-input-error
      (star.router.events:commit-document-event binding (event :invalid t) (peer-key)))
    (signals star.router.application:application-rejection
      (star.router.events:commit-document-event binding (event :dataset "other") (peer-key))))
  (signals star.router.application:application-rejection
    (star.router.events:commit-document-event
     (binding (lambda (key)
                (declare (ignore key))
                (star.auth::%make-service-call-context
                 :principal-id "expired" :deadline (1- (get-universal-time)))))
     (event) (peer-key))))

(test denied-tenant-never-reaches-persistence
  (signals star.authorization:authorization-error
    (star.router.events:commit-document-event
     (binding (lambda (key) (declare (ignore key))
                (context '("documents:write" "dataset:fixture" "tenant:other"))))
     (event) (peer-key))))

(test durable-event-never-succeeds-after-persistence-failure
  ;; Only the external persistence port is replaced. Codec, schema,
  ;; authorization, tenant stamping and mutation settlement remain real.
  (let* ((symbol 'star.rabbit::persist-rabbit-document-mutation)
         (original (symbol-function symbol)) (calls 0))
    (unwind-protect
         (progn
           (setf (symbol-function symbol)
                 (lambda (document operation)
                   (declare (ignore document operation))
                   (incf calls)
                   (is (string= "event/1"
                                (jsown:val star.databases.couchdb::*outbox-message-context* "messageId")))
                   (is (string= "origin/1"
                                (jsown:val star.databases.couchdb::*outbox-message-context* "causationId")))
                   (is (string= "event-principal"
                                (star.auth:service-call-context-principal-id
                                 star.frontends.http-api::*service-call-context*)))
                   (error "synthetic persistence failure")))
           (signals error
             (star.router.events:commit-document-event
              (binding (lambda (key) (declare (ignore key)) (context))) (event) (peer-key)))
           (is (= 1 calls)))
      (setf (symbol-function symbol) original))))

(test committed-event-preserves-identity-and-server-tenant
  ;; A successful external persistence port permits the REAL consumer settlement
  ;; to return ACK. This is adapter evidence only, not a live durable-backend test.
  (let* ((symbol 'star.rabbit::persist-rabbit-document-mutation)
         (original (symbol-function symbol)) (calls 0))
    (unwind-protect
         (progn
           (setf (symbol-function symbol)
                 (lambda (document operation)
                   (incf calls)
                   (is (eq :new operation))
                   (is (string= "fixture:event-document"
                                (star.documents:document-id document)))
                   (is (string= "default" (star.documents:document-value document "tenant_id")))
                   t))
           (let ((result (star.router.events:commit-document-event
                          (binding (lambda (key) (declare (ignore key)) (context)))
                          (event) (peer-key))))
             (is (eq :committed (getf result :status)))
             (is (eq t (getf result :durable-p)))
             (is (string= "event/1" (getf result :message-id)))
             (is (string= "trace/1" (getf result :correlation-id)))
             (is (string= "origin/1" (getf result :causation-id))))
           (is (= 1 calls)))
      (setf (symbol-function symbol) original))))

(test host-wrapper-delegates-other-kinds-and-propagates-event-denial
  (let* ((calls 0)
         (handler (star.router.events:make-event-handler
                   (binding (lambda (key) (declare (ignore key)) nil))
                   (lambda (route envelope key)
                     (declare (ignore route envelope key)) (incf calls) :delegated))))
    (is (eq :delegated (funcall handler "route" '(:kind :command) (peer-key))))
    (signals star.router.application:application-rejection
      (funcall handler "route" (event) (peer-key)))
    (is (= 1 calls))))
