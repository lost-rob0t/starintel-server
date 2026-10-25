(defpackage :star.router.events
  (:use :cl)
  (:export #:make-document-event-binding #:commit-document-event #:make-event-handler))
(in-package :star.router.events)

(defstruct (document-event-binding (:constructor %make-document-event-binding))
  manifest message-type actor payload-field authenticate)

(defun reject (code) (star.router.application::reject code))

(defun make-document-event-binding (manifest message-type actor authenticate
                                    &key (payload-field "document"))
  "Bind a declared canonical event to document ingestion, never command admission.
AUTHENTICATE resolves a current trusted service context from the authenticated
CURVE public key on EVERY event. Sender and route identities grant no authority.
This adapter defines no stream wire protocol or lifecycle acknowledgement."
  (check-type authenticate function)
  (let ((binding (star.router.application:make-document-binding
                  manifest message-type actor :payload-field payload-field)))
    (%make-document-event-binding
     :manifest (star.router.application::document-binding-manifest binding)
     :message-type (copy-seq message-type) :actor (copy-seq actor)
     :payload-field (copy-seq payload-field) :authenticate authenticate)))

(defun commit-document-event (binding envelope peer-key)
  "Commit one canonical document event through existing mutation/outbox authority.
ENVELOPE is decoded by the host; the binding validates a private snapshot again.
Only :COMMITTED proves this call's durable effect; exceptions may mean unknown.
Redelivery retains document/outbox identities. This does not add an event inbox,
exactly-once guarantee, command completion, Target dispatch, or subscription."
  (check-type binding document-event-binding)
  (unless (typep peer-key '(simple-array (unsigned-byte 8) (32)))
    (reject :authentication-required))
  (let* ((owned (staractorprotocol:snapshot-portable-wire-value envelope))
         (manifest (document-event-binding-manifest binding)))
    (star-actor-wire:encode-binding-message :zmq '("star.actor2actor/1") manifest owned)
    (unless (eq :event (getf owned :kind)) (reject :not-event-input))
    (unless (and (equal (getf owned :message-type)
                        (document-event-binding-message-type binding))
                 (equal (getf owned :actor) (document-event-binding-actor binding)))
      (reject :unbound-operation))
    (let* ((context
             (star.router.application::command-service-context
              (funcall (document-event-binding-authenticate binding) (copy-seq peer-key)) owned))
           (field (assoc (document-event-binding-payload-field binding)
                         (getf owned :payload) :test #'equal)))
      (unless field (reject :missing-document))
      (let* ((document
               (star.frontends.http-api::require-json-object
                (star.frontends.http-api::parse-json-octets
                 (star-actor-wire:encode-json-value (cdr field)) "application/json")))
             (star.frontends.http-api::*service-call-context* context)
             (star.frontends.http-api::*http-correlation-id* (getf owned :correlation-id))
             (star.frontends.http-api::*service-message-metadata*
               (list :message-id (getf owned :message-id)
                     :correlation-id (getf owned :correlation-id)
                     :causation-id (getf owned :causation-id)))
             (star.databases.couchdb::*outbox-message-context*
               (cons :obj
                     (loop for (wire key) in '(("messageId" :message-id)
                                               ("correlationId" :correlation-id)
                                               ("causationId" :causation-id))
                           for value = (getf owned key)
                           when value collect (cons wire value)))))
        (star.frontends.http-api::validate-document-input document)
        (unless (equal (getf owned :dataset) (star.documents:document-dataset document))
          (reject :dataset-mismatch))
        ;; Target execution remains exclusively owned by its existing API/runtime.
        (when (equal "target" (star.documents:object-value document "dtype" nil))
          (reject :event-target-ingress-unsupported))
        (when (star.documents:document-transient-p document) (reject :transient-not-durable))
        (star.authorization:authorized-publish-document
         document
         (lambda (authorized-document)
           (star.router.application::command-service-context context owned)
           (let ((settlement
                   (star.rabbit::process-rabbit-document-mutation
                    (cons (star.documents:document-json
                           (star.frontends.http-api::stamp-server-tenant! authorized-document)) 0)
                    :new)))
             (unless (eq :ack (star.consumers:consumer-settlement-action settlement))
               (reject :not-committed))))
         :principal context
         :metadata (list :route "internal:router-event" :method "PUBLISH"
                         :correlation-id (getf owned :correlation-id)))
        (list :status :committed :durable-p t
              :document-id (star.documents:document-id document)
              :message-id (getf owned :message-id)
              :correlation-id (getf owned :correlation-id)
              :causation-id (getf owned :causation-id))))))

(defun make-event-handler (binding inbound-handler)
  "Compose with START-HOST: commit declared events, delegate other envelopes.
No wire reply is emitted: a canonical event has no command acknowledgement.
Commit errors propagate to the host rejection boundary and may be ambiguous."
  (check-type binding document-event-binding)
  (check-type inbound-handler function)
  (lambda (route envelope peer-key)
    (if (eq :event (getf envelope :kind))
        (progn (commit-document-event binding envelope peer-key) nil)
        (funcall inbound-handler route envelope peer-key))))
