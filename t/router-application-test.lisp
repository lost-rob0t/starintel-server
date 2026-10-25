(defpackage :star.router.application.tests (:use :cl :fiveam))
(in-package :star.router.application.tests)
(def-suite :router-application)
(in-suite :router-application)

(test outbox-recovery-retains-original-lifecycle-context
  (let* ((original (jsown:new-js ("messageId" "origin/1")
                                  ("correlationId" "trace/1")
                                  ("causationId" "target/1")))
         (entry (jsown:new-js
                  ("routing_key" "documents.new.person") ("payload" (jsown:empty-object))
                  ("event_id" "stable-event")
                  ("message_context"
                   (star.databases.couchdb::checked-outbox-message-context original))))
         (star.databases.couchdb::*outbox-message-context*
           (jsown:new-js ("messageId" "unrelated-recovery-request"))))
    (setf (jsown:val original "messageId") "mutated-after-admission")
    ;; Exercise actual recovery publication boundary after serialization/reload.
    (star.databases.couchdb::publish-outbox-entry
     (lambda (route payload event-id)
       (declare (ignore payload))
       (is (string= "documents.new.person" route))
       (is (string= "stable-event" event-id))
       (is (string= "origin/1"
                    (jsown:val star.databases.couchdb::*outbox-message-context* "messageId")))
       (is (string= "target/1"
                    (jsown:val star.databases.couchdb::*outbox-message-context* "causationId"))))
     (star.databases.couchdb::clone-outbox-json entry))
    (is (string= "unrelated-recovery-request"
                 (jsown:val star.databases.couchdb::*outbox-message-context* "messageId")))))

(test outbox-context-rejects-credentials-and-duplicate-keys
  (signals error
    (star.databases.couchdb::checked-outbox-message-context
     (jsown:new-js ("credential" "must-not-persist"))))
  (signals error
    (star.databases.couchdb::checked-outbox-message-context
     '(:obj ("messageId" . "one") ("messageId" . "two")))))

(test rabbit-event-identity-and-persistence-survive-router-metadata
  (let* ((symbol 'star.rabbit::emit-document)
         (original (symbol-function symbol)) (calls 0))
    (unwind-protect
         (progn
           (setf (symbol-function symbol)
                 (lambda (exchange route payload &key properties)
                   (declare (ignore exchange route payload))
                   (incf calls)
                   (is (= 2 (cdr (assoc :delivery-mode properties))))
                   (is (string= "stable-event" (cdr (assoc :message-id properties))))
                   (if (= calls 1)
                       (is (null (assoc :headers properties)))
                       (progn
                         (is (string= "trace" (cdr (assoc :correlation-id properties))))
                         (is (string= "origin"
                                      (cdr (assoc "x-star-origin-message-id"
                                                  (cdr (assoc :headers properties))
                                                  :test #'equal))))))
                   t))
           (let ((star.databases.couchdb::*outbox-message-context* nil))
             (star.rabbit::publish-outbox-event "documents.new.person" "{}" "stable-event"))
           (let ((star.databases.couchdb::*outbox-message-context*
                   (jsown:new-js ("messageId" "origin") ("correlationId" "trace"))))
             (star.rabbit::publish-outbox-event "documents.new.person" "{}" "stable-event"))
           (is (= 2 calls)))
      (setf (symbol-function symbol) original))))

(defun manifest ()
  '(:messages ((:name "fixture.ingest@1"
                :fields ((:name "document" :type "map" :required t))))))
(defun binding ()
  (star.router.application:make-document-binding (manifest) "fixture.ingest@1" "server.ingest"))
(defun context (&optional (scopes '("documents:write" "tenant:default" "dataset:fixture")))
  (star.auth::%make-service-call-context
   :principal-id "fixture" :principal-type "api_client" :credential-id "fixture-key"
   :scopes scopes :correlation-id "trusted"))
(defun command (&key (actor "server.ingest") (dataset "fixture") invalid)
  (star-actor-wire:encode-envelope
   (manifest)
   (staractorprotocol:make-command-envelope
    :message-id "fixture/1" :message-type "fixture.ingest@1" :actor actor
    :sender "untrusted-sender" :reply-to "fixture.actor"
    :correlation-id "fixture/correlation" :idempotency-key "fixture/1"
    :dataset dataset
    :payload `(("document" . (("id" . "fixture:document") ("dataset" . "fixture")
                              ("dtype" . "person") ("schemaVersion" . "0.10.1")
                              (,(if invalid "unexpectedField" "fname") . "Synthetic")))))))

(test rejects-untrusted-context-destination-and-dataset
  (signals star.router.application:application-rejection
    (star.router.application:submit-document-command (binding) nil (command)))
  (signals star.router.application:application-rejection
    (star.router.application:submit-document-command (binding) (context) (command :actor "other")))
  (signals star.router.application:application-rejection
    (star.router.application:submit-document-command (binding) (context) (command :dataset "other")))
  (signals star-actor-wire:unsupported-peer-protocol
    (star.router.application:submit-document-command (binding) (context) (command)
                                                     :peer-profiles '("SC01"))))

(test rejects-expired-session-and-invalid-schema
  (signals star.router.application:application-rejection
    (star.router.application:submit-document-command
     (binding) (star.auth::%make-service-call-context :principal-id "fixture"
                                                    :deadline (1- (get-universal-time)))
     (command)))
  (signals star.frontends.http-api:http-input-error
    (star.router.application:submit-document-command (binding) (context) (command :invalid t))))

(test authorization-denial-happens-before-persistence
  ;; Real authorization policy; no database/transport substitutes are installed.
  ;; An accidental effect would fail this test through missing external services.
  (signals star.authorization:authorization-error
    (star.router.application:commit-document-command
     (binding) (context '("documents:read" "dataset:fixture" "tenant:default")) (command))))

(test submission-is-not-durable-and-keeps-trusted-identity
  ;; Replace only the external Rabbit publication port, not authorization/codec.
  (let* ((symbol 'star.frontends.http-api::publish-document-unchecked)
         (original (symbol-function symbol)) (calls 0))
    (unwind-protect
         (progn
           (setf (symbol-function symbol)
                 (lambda (document)
                   (incf calls)
                   (is (string= "fixture:document" (star.documents:document-id document)))
                   (is (string= "fixture"
                                (star.auth:service-call-context-principal-id
                                 star.frontends.http-api::*service-call-context*)))
                   t))
           (let ((result (star.router.application:submit-document-command
                          (binding) (context) (command))))
             (is (eq :submitted (getf result :status)))
             (is (null (getf result :durable-p)))
             (is (= 1 calls))))
      (setf (symbol-function symbol) original))))

(test commit-never-succeeds-after-persistence-failure
  ;; Replace only the external persistence port. Canonical decode, schema,
  ;; authorization and Rabbit mutation settlement all remain real.
  (let* ((symbol 'star.rabbit::persist-rabbit-document-mutation)
         (original (symbol-function symbol)) (calls 0))
    (unwind-protect
         (progn
           (setf (symbol-function symbol)
                 (lambda (document operation)
                   (declare (ignore document operation))
                   (incf calls)
                   (error "synthetic persistence failure")))
           (signals error
             (star.router.application:commit-document-command (binding) (context) (command)))
           (is (= 1 calls)))
      (setf (symbol-function symbol) original))))
