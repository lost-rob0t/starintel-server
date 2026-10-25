(defpackage :star.router.sc01-application
  (:use :cl)
  (:export #:make-binding #:process-event #:process-once #:send-dispatch))
(in-package :star.router.sc01-application)

(defstruct (binding (:constructor %make-binding))
  public-key actor instance-id context-function)

(defun reject (code)
  (error 'star.router.application:application-rejection :code code))

(defun make-binding (public-key actor instance-id context-function)
  "Trusted SC01 deployment binding; never an actor2actor compatibility fallback.
Pin the authenticated CURVE key, actor registration topic and instance source ID.
CONTEXT-FUNCTION resolves current application credentials on EVERY event; it
receives the authenticated key and must return a service-call-context or fail.
Binding permits only the pinned registration, heartbeat and newDocument ingest."
  (check-type public-key (simple-array (unsigned-byte 8) (32)))
  (check-type context-function function)
  (unless (star.actors::valid-target-actor-name-p actor) (reject :invalid-actor))
  (unless (and (stringp instance-id) (<= 1 (length instance-id) 255)
               (not (find (code-char 0) instance-id)))
    (reject :invalid-instance))
  (%make-binding :public-key (copy-seq public-key) :actor (copy-seq actor)
                 :instance-id (copy-seq instance-id) :context-function context-function))

(defun text-frame (frame)
  (unless (typep frame '(simple-array (unsigned-byte 8) (*)))
    (reject :invalid-sc01-frame))
  (handler-case (babel:octets-to-string frame :encoding :utf-8)
    (error () (reject :invalid-sc01-utf8))))

(defun trusted-context (binding key)
  (unless (and (typep key '(simple-array (unsigned-byte 8) (32)))
               (equalp key (binding-public-key binding)))
    (reject :authentication-required))
  (let ((context (funcall (binding-context-function binding) (copy-seq key))))
    ;; Check credentials and their deadline without inventing missing SC01
    ;; correlation, causation, idempotency, cancellation or execution metadata.
    (star.router.application::command-service-context context nil)))

(defun require-private-router (binding router)
  ;; Legacy SUB clients choose their own topics. A shared multi-peer PUB is not
  ;; tenant isolation, even with CURVE; require the sole enrolled key to be this
  ;; binding's key before either reception/settlement or outbound publication.
  (check-type router star.router::sc01-router)
  (let ((auth (star.router::sc01-router-authenticator router)))
    (unless (and (star.router::sc01-router-secure router)
                 (star.router::authenticator-valid-p
                  auth (star.router::sc01-router-context router) t)
                 (equalp (first (star.router::authenticator-keys auth))
                         (binding-public-key binding)))
      (reject :sc01-private-peer-required))))

(defun commit-document (context bytes message-id)
  (let* ((document
           (star.frontends.http-api::require-json-object
            (star.frontends.http-api::parse-json-octets bytes "application/json")))
         (star.frontends.http-api::*service-call-context* context)
         (star.frontends.http-api::*service-message-metadata*
           (list :message-id message-id))
         (star.frontends.http-api::*http-correlation-id* nil)
         (star.databases.couchdb::*outbox-message-context*
           (cons :obj (list (cons "messageId" message-id)))))
    (star.frontends.http-api::validate-document-input document)
    ;; A legacy payload cannot invoke the target scheduler or imply lifecycle
    ;; completion/lease settlement. Existing canonical application API owns it.
    (when (equal "target" (star.documents:object-value document "dtype" nil))
      (reject :sc01-target-ingress-unsupported))
    (when (star.documents:document-transient-p document)
      (reject :transient-not-durable))
    (star.authorization:authorized-publish-document
     document
     (lambda (authorized-document)
       (star.router.application::command-service-context context nil)
       (let ((settlement
               (star.rabbit::process-rabbit-document-mutation
                (cons (star.documents:document-json
                       (star.frontends.http-api::stamp-server-tenant! authorized-document)) 0)
                :new)))
         (unless (eq :ack (star.consumers:consumer-settlement-action settlement))
           (reject :not-committed))))
     :principal context
     :metadata (list :route "internal:sc01-commit" :method "PUBLISH"))
    (list :status :committed :durable-p t :message-id message-id
          :document-id (star.documents:document-id document))))

(defun process-event (binding frames key)
  "Authorize a previously native-validated SC01 event and apply its allowed effect.
Return only after a newDocument's shared mutation/outbox boundary succeeds.
Registration/heartbeat results are transport control, never durable acceptance.
Caller must settle; this function itself neither sends ACK nor publishes frames."
  (check-type binding binding)
  (unless (and (listp frames) (= 9 (length frames))) (reject :invalid-sc01-frames))
  (dolist (frame frames)
    (unless (typep frame '(simple-array (unsigned-byte 8) (*)))
      (reject :invalid-sc01-frame)))
  (unless (and (zerop (length (second frames)))
               (equal "SC01" (text-frame (third frames))))
    (reject :sc01-profile-required))
  (let* ((context (trusted-context binding key))
         (source (text-frame (nth 3 frames)))
         (message-id (text-frame (nth 4 frames)))
         (event (text-frame (nth 6 frames)))
         (topic (text-frame (nth 7 frames))))
    (unless (equal source (binding-instance-id binding)) (reject :instance-mismatch))
    (unless (<= 1 (length message-id) 255) (reject :invalid-message-id))
    (cond
      ((member event '("0" "7") :test #'equal)
       (unless (equal topic (binding-actor binding)) (reject :actor-mismatch))
       (unless (zerop (length (nth 8 frames))) (reject :unexpected-control-payload))
       (list :status :control-authorized :durable-p nil))
      ((equal event "3") (commit-document context (nth 8 frames) message-id))
      (t (reject :unsupported-sc01-operation)))))

(defun process-once (binding router)
  "Poll on the SC01 owner thread and settle after policy/application completion.
All rejection paths attempt NACK. A settlement failure after commit is an
unknown transport outcome; no blind replay or synthetic success is returned."
  (require-private-router binding router)
  (multiple-value-bind (frames key status) (star.router:receive-sc01 router)
    (when (eq status :timeout) (return-from process-once :timeout))
    (let ((settled nil))
      (unwind-protect
           (let ((result (process-event binding frames key)))
             (star.router:settle-sc01 router t)
             (setf settled t)
             result)
        (unless settled
          ;; Preserve the original rejection if the transport also failed.
          (ignore-errors (star.router:settle-sc01 router nil)))))))

(defun send-dispatch (binding router dispatch context)
  "Project an existing authorized Server dispatch to explicit legacy SC01.
Must run on ROUTER's owner thread. Retains existing dispatch extensions and ID;
does not create schedules, acceptance records or claim actor execution success.
PUB acceptance is volatile. SC01 cannot carry canonical cancel/result/stream,
application causation or durable actor admission, and is never selected as an
actor2actor fallback. Caller supplies a current trusted dispatch service context."
  (check-type binding binding)
  (check-type dispatch star.actors::target-dispatch-envelope)
  (require-private-router binding router)
  (trusted-context binding (binding-public-key binding))
  (star.actors::validate-target-dispatch-record
   (star.actors::target-dispatch-envelope-record dispatch))
  (let* ((context (star.router.application::command-service-context context nil))
         (document (star.actors::target-dispatch-document dispatch))
         (destination (star.actors::target-dispatch-envelope-destination dispatch)))
    (unless (and (equal (binding-actor binding)
                        (star.actors::target-destination-handle-name destination))
                 (equal (binding-actor binding)
                        (star.actors::target-record-actor
                         (star.actors::target-dispatch-envelope-record dispatch))))
      (reject :actor-mismatch))
    (star.frontends.http-api::validate-document-input document)
    (star.authorization:authorized-publish-document
     document
     (lambda (authorized)
       (star.router:send-sc01-target
        router (binding-actor binding)
        (star.actors::target-dispatch-envelope-execution-id dispatch)
        (babel:string-to-octets (star.documents:document-json authorized) :encoding :utf-8)))
     :principal context :actor-name (binding-actor binding)
     :metadata (list :route "internal:sc01-target" :method "PUBLISH"))))
