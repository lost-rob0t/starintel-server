(defpackage :star.router.application
  (:use :cl)
  (:export #:make-document-binding #:submit-document-command #:commit-document-command
           #:application-rejection #:application-rejection-code))
(in-package :star.router.application)

(define-condition application-rejection (error)
  ((code :initarg :code :reader application-rejection-code))
  (:report (lambda (condition stream)
             (format stream "Router application rejected command: ~A"
                     (application-rejection-code condition)))))

(defun reject (code) (error 'application-rejection :code code))

(defstruct (document-binding (:constructor %make-document-binding))
  manifest message-type actor payload-field)

(defun make-document-binding (manifest message-type actor &key (payload-field "document"))
  "Trusted configuration maps one canonical command to existing document ingest.
This does not create a new message schema: MANIFEST must declare MESSAGE-TYPE.
No network listener, credential lookup or application effect occurs here."
  (dolist (value (list message-type actor payload-field))
    (unless (and (stringp value) (< 0 (length value) 257))
      (reject :invalid-binding)))
  (%make-document-binding
   :manifest (staractorprotocol:snapshot-portable-wire-value manifest)
   :message-type (copy-seq message-type) :actor (copy-seq actor)
   :payload-field (copy-seq payload-field)))

(defun command-service-context (context envelope)
  ;; Caller supplies a context established by trusted authentication. Neither
  ;; sender, ROUTER identity, payload nor peer capability advertisement grants it.
  (unless (and (typep context 'star.auth:service-call-context)
               (let ((id (star.auth:service-call-context-principal-id context)))
                 (and (stringp id) (plusp (length id)))))
    (reject :authentication-required))
  (let* ((now (get-universal-time))
         (session-deadline (star.auth:service-call-context-deadline context))
         (wire-deadline (getf envelope :deadline))
         (command-deadline
           (when wire-deadline
             (handler-case
                 (local-time:timestamp-to-universal
                  (local-time:parse-timestring wire-deadline))
               (error () (reject :invalid-deadline)))))
         (deadline (cond ((and session-deadline command-deadline)
                          (min session-deadline command-deadline))
                         (t (or session-deadline command-deadline)))))
    (when (and deadline (<= deadline now)) (reject :deadline-exceeded))
    (star.auth::%make-service-call-context
     :principal-id (star.auth:service-call-context-principal-id context)
     :principal-type (star.auth:service-call-context-principal-type context)
     :credential-id (star.auth:service-call-context-credential-id context)
     :scopes (star.auth:service-call-context-scopes context)
     :correlation-id (getf envelope :correlation-id)
     :deadline deadline)))

(defun apply-document-command (binding context bytes peer-profiles commit-p)
  "Validate and authorize before either existing publication or mutation boundary.
CONTEXT is supplied by trusted authentication, never decoded from BYTES.
No lifecycle reply is fabricated: the owning runtime controls that lifecycle."
  (check-type binding document-binding)
  (let* ((envelope (star-actor-wire:decode-command-message
                    :zmq peer-profiles (document-binding-manifest binding) bytes))
         (service-context (command-service-context context envelope)))
    (unless (and (equal (getf envelope :message-type)
                        (document-binding-message-type binding))
                 (equal (getf envelope :actor) (document-binding-actor binding)))
      (reject :unbound-operation))
    (let* ((payload (getf envelope :payload))
           (field (assoc (document-binding-payload-field binding) payload :test #'equal)))
      (unless field (reject :missing-document))
      (let* ((document
               (star.frontends.http-api::require-json-object
                (star.frontends.http-api::parse-json-octets
                 (star-actor-wire:encode-json-value (cdr field)) "application/json")))
             (star.frontends.http-api::*service-call-context* service-context)
             (star.frontends.http-api::*service-message-metadata*
               (list :message-id (getf envelope :message-id)
                     :causation-id (getf envelope :causation-id)
                     :idempotency-key (getf envelope :idempotency-key)))
             (star.frontends.http-api::*http-correlation-id* (getf envelope :correlation-id)))
        (star.frontends.http-api::validate-document-input document)
        (unless (equal (getf envelope :dataset)
                       (star.documents:document-dataset document))
          (reject :dataset-mismatch))
        ;; Recheck immediately before the existing authorized effect boundary.
        (command-service-context service-context envelope)
        (if commit-p
            (let ((star.databases.couchdb::*outbox-message-context*
                    (cons :obj
                          (loop for (wire key) in '(("messageId" :message-id)
                                                    ("correlationId" :correlation-id)
                                                    ("causationId" :causation-id)
                                                    ("idempotencyKey" :idempotency-key))
                                for value = (getf envelope key)
                                when value collect (cons wire value)))))
              (star.authorization:authorized-publish-document
             document
             (lambda (authorized-document)
               ;; The exact consumer mutation/outbox authority is shared with
               ;; Rabbit; this is not an alternate direct CouchDB write path.
               (when (star.documents:document-transient-p authorized-document)
                 (reject :transient-not-durable))
               (let ((settlement
                       (star.rabbit::process-rabbit-document-mutation
                        (cons (star.documents:document-json
                               (star.frontends.http-api::stamp-server-tenant!
                                authorized-document)) 0)
                        :new)))
                 (unless (eq :ack (star.consumers:consumer-settlement-action settlement))
                   (reject :not-committed))))
             :principal service-context
             :metadata (list :route "internal:router-commit" :method "PUBLISH"
                             :correlation-id (getf envelope :correlation-id))))
            (star.frontends.http-api::publish-document document))
        (list :status (if commit-p :committed :submitted) :durable-p commit-p
              :document-id (star.documents:document-id document)
              :message-id (getf envelope :message-id)
              :correlation-id (getf envelope :correlation-id)
              :causation-id (getf envelope :causation-id))))))

(defun submit-document-command (binding context bytes
                                &key (peer-profiles '("star.actor2actor/1")))
  "Asynchronously publish through existing authorized Rabbit ingest; not durable."
  (apply-document-command binding context bytes peer-profiles nil))

(defun commit-document-command (binding context bytes
                                &key (peer-profiles '("star.actor2actor/1")))
  "Synchronously use the existing Rabbit consumer's mutation/outbox authority.
Return :COMMITTED only after that authority returns its durable :ACK settlement.
Any persistence/publication exception propagates: outcome may be unknown and
must be reconciled using canonical document identity. Transient docs fail closed.
This does not claim new broker-confirm guarantees or a lifecycle execution journal."
  (apply-document-command binding context bytes peer-profiles t))
