(defpackage :star.router.server
  (:use :cl)
  (:export #:bind-target-route #:bind-sc01-target-route #:unbind-target-route))
(in-package :star.router.server)

(defstruct (target-route (:constructor %make-target-route))
  actor host route envelope-function descriptor payload-field outbound actor-reference context-function lease-policy
  (profile :actor2actor))
(defvar *routes* (make-hash-table :test #'equal))
(defvar *routes-lock* (bordeaux-threads:make-lock "native target deployment bindings"))

(defun resolve-route (actor)
  (bordeaux-threads:with-lock-held (*routes-lock*)
    (let ((binding (gethash actor *routes*)))
      (when binding
        (star.actors::make-target-destination-handle
         :zmq actor :component binding :routing-key (target-route-descriptor binding))))))

(defun bind-target-route (actor host route envelope-function
                          &key (payload-field "target") outbound actor-reference context-function
                            lease-policy lease-store lease-owner lease-identity-function lease-instance-id
                            (lease-ttl-ms 60000))
  "Trusted init binds an existing actor name to native delivery, without granting access.
ENVELOPE-FUNCTION projects the full existing dispatch envelope into its emitted
manifest's canonical message. No alternate scheduler or persistence is installed.
HOST must pin a singleton CURVE allowlist: ROUTER identity alone cannot bind a
recipient key when multiple authenticated peers can claim the same route.
OUTBOUND is a durable pending coordinator. ACTOR-REFERENCE is trusted enrollment;
CONTEXT-FUNCTION(dispatch) returns expected authenticated peer context and tenant.
These cannot come from the actor's target payload or ROUTER identity."
  (unless (star.actors::valid-target-actor-name-p actor)
    (error "Invalid native actor binding"))
  (check-type host star.router::host)
  (unless (star.router:secure-host-p host :singleton-peer-p t)
    (error "Native application target routes require a running singleton CURVE host"))
  (check-type envelope-function function)
  (check-type outbound star.router.outbound::coordinator)
  (check-type actor-reference staractorprotocol:star-actor-reference)
  (check-type context-function function)
  (when (and lease-policy lease-store) (error "Configure one native lease policy"))
  (setf lease-policy
        (or lease-policy (star.router.leases:make-policy
                          lease-store lease-owner lease-identity-function lease-instance-id
                          :ttl-ms lease-ttl-ms)))
  (check-type lease-policy star.router.leases::policy)
  (unless (and (stringp payload-field) (<= 1 (length payload-field) 256))
    (error "Invalid native Target payload field"))
  (check-type route (simple-array (unsigned-byte 8) (*)))
  (unless (<= 1 (length route) 255) (error "Invalid native route size"))
  (bordeaux-threads:with-lock-held (*routes-lock*)
    (when (gethash actor *routes*) (error "Actor already has a native deployment binding"))
    (when (and star.actors::*target-zmq-destination-resolver*
               (not (eq star.actors::*target-zmq-destination-resolver* #'resolve-route)))
      (error "Another native deployment resolver is installed"))
    (bordeaux-threads:with-lock-held ((star.router::host-lock host))
      (let ((existing (star.router::host-outcome-authority host)))
        (when (and existing (not (eq outbound existing)))
          (error "One host must retain one pending outcome authority"))
        (unless existing
          (setf (star.router::host-handler host)
                (star.router.outbound:make-outcome-handler outbound (star.router::host-handler host))
                (star.router::host-outcome-authority host) outbound))))
    (setf (gethash actor *routes*)
          (%make-target-route
           :actor (copy-seq actor) :host host :route (copy-seq route)
           :envelope-function envelope-function
           :outbound outbound :context-function context-function
           :lease-policy lease-policy
           :actor-reference (star.router.outbound::json-reference
                             (star.router.outbound::reference-json actor-reference))
           :payload-field (copy-seq payload-field)
           :descriptor (format nil "star.actor2actor/1:~{~2,'0X~}"
                               (coerce route 'list)))
          star.actors::*target-zmq-destination-resolver* #'resolve-route))
  actor)

(defun unbind-target-route (actor)
  "Remove an owned deployment binding; existing schedules fail closed on stale binding."
  (bordeaux-threads:with-lock-held (*routes-lock*)
    (remhash actor *routes*)
    (when (and (zerop (hash-table-count *routes*))
               (eq star.actors::*target-zmq-destination-resolver* #'resolve-route))
      (setf star.actors::*target-zmq-destination-resolver* nil)))
  nil)

(defun bind-sc01-target-route (actor host context-function
                               &key lease-policy lease-store lease-owner lease-identity-function
                                 lease-instance-id (lease-ttl-ms 60000))
  "Explicit SC01 deployment in the SAME target resolver/registry as actor2actor.
CONTEXT-FUNCTION(dispatch) resolves current trusted dispatch credentials on the
SC01 owner. Existing HTTP admission, scheduling, persistence and lease authority
remain in place. Legacy PUB queueing is not durable actor admission."
  (check-type context-function function)
  (when (and lease-policy lease-store) (error "Configure one native lease policy"))
  (setf lease-policy
        (or lease-policy (star.router.leases:make-policy
                          lease-store lease-owner lease-identity-function lease-instance-id
                          :ttl-ms lease-ttl-ms)))
  (check-type lease-policy star.router.leases::policy)
  (unless (and (star.actors::valid-target-actor-name-p actor)
               (star.router.sc01-host:secure-host-p host)
               (equal actor (star.router.sc01-application::binding-actor
                             (star.router.sc01-host::host-binding host))))
    (error "SC01 target binding requires the matching singleton secure host"))
  (bordeaux-threads:with-lock-held (*routes-lock*)
    (when (gethash actor *routes*) (error "Actor already has a native deployment binding"))
    (when (and star.actors::*target-zmq-destination-resolver*
               (not (eq star.actors::*target-zmq-destination-resolver* #'resolve-route)))
      (error "Another native deployment resolver is installed"))
    (setf (gethash actor *routes*)
          (%make-target-route :actor (copy-seq actor) :host host :profile :sc01
                              :lease-policy lease-policy
                              :envelope-function context-function
                              :descriptor (format nil "SC01:~A" actor))
          star.actors::*target-zmq-destination-resolver* #'resolve-route))
  actor)

(defmethod star.actors::prepare-zmq-target-envelope ((binding target-route) dispatch)
  (bordeaux-threads:with-lock-held (*routes-lock*)
    (unless (eq binding (gethash (target-route-actor binding) *routes*))
      (error 'star.actors::target-destination-unavailable :reason "stale native lease binding")))
  (star.router.leases:prepare-envelope
   (target-route-lease-policy binding) dispatch
   :reply-budget-ms (if (target-route-outbound binding)
                       (* 1000 (star.router.outbound::coordinator-wait-seconds
                                (target-route-outbound binding)))
                       1000)))

(defun send-actor2actor-target (binding dispatch)
  (unless (star.router:secure-host-p (target-route-host binding) :singleton-peer-p t)
    (error 'star.actors::target-destination-unavailable :reason "secure native host unavailable"))
  (bordeaux-threads:with-lock-held (*routes-lock*)
    (unless (eq binding (gethash (target-route-actor binding) *routes*))
      (error 'star.actors::target-destination-unavailable :reason "stale native binding")))
  (let* ((command (funcall (target-route-envelope-function binding) dispatch))
         (execution-id (star.actors::target-dispatch-envelope-execution-id dispatch))
         (record (star.actors::target-dispatch-envelope-record dispatch)))
    ;; A projection must retain invocation identity. Lease/fencing remain in the
    ;; original dispatch document and must be included by the declared contract.
    (unless (and (eq (getf command :kind) :command)
                 (equal (getf command :message-id) execution-id)
                 (equal (getf command :idempotency-key) execution-id)
                 (equal (getf command :correlation-id)
                        (star.actors::target-dispatch-envelope-trace-id dispatch))
                 (equal (getf command :causation-id) (star.actors::target-record-id record))
                 (equal (getf command :dataset)
                        (star.documents:document-dataset (star.actors::target-record-document record)))
                 (equal (getf command :actor) (target-route-actor binding)))
      (error 'star.actors::invalid-target-dispatch :reason "native projection loses dispatch identity"))
    (unless (eql (getf command :attempt) (star.router.leases:canonical-attempt dispatch))
      (error 'star.actors::invalid-target-dispatch :reason "native projection loses canonical attempt"))
    (let ((expected
            (star-actor-wire:decode-json-value
             (babel:string-to-octets
              (star.documents:document-json (star.actors::target-dispatch-document dispatch))
              :encoding :utf-8)))
          (actual (cdr (assoc (target-route-payload-field binding) (getf command :payload)
                              :test #'equal))))
      (unless (equalp expected actual)
        (error 'star.actors::invalid-target-dispatch
               :reason "native projection loses canonical Target or lease/fencing metadata")))
    (multiple-value-bind (context tenant) (funcall (target-route-context-function binding) dispatch)
      (multiple-value-bind (invocation disposition)
          (star.router.outbound:register-invocation
           (target-route-outbound binding) command (target-route-actor-reference binding)
           context (star.router:authenticator-single-key
                    (star.router::host-authenticator (target-route-host binding)))
           tenant execution-id (star.actors::target-dispatch-envelope-lease-id dispatch)
           (star.actors::target-dispatch-envelope-fencing-token dispatch))
        (declare (ignore invocation))
        (unless (eq disposition :registered)
          (error 'star.actors::target-destination-unavailable
                 :reason "existing invocation requires durable outcome reconciliation; no automatic resend"))))
    (unless (eq :queued
                (star.router:await-send
                 (star.router:enqueue-envelope (target-route-host binding)
                                                (target-route-route binding) command)))
      (error 'star.actors::target-destination-unavailable :reason "native transport backpressure"))
    (star.router.outbound:mark-transport-queued (target-route-outbound binding) execution-id)))

(defmethod star.actors::send-zmq-target ((binding target-route) dispatch)
  (star.router.leases:verify-envelope (target-route-lease-policy binding) dispatch)
  (ecase (target-route-profile binding)
    (:actor2actor (send-actor2actor-target binding dispatch))
    (:sc01
     (unless (star.router.sc01-host:secure-host-p (target-route-host binding))
       (error 'star.actors::target-destination-unavailable :reason "SC01 secure host unavailable"))
     (flet ((context (snapshot)
              ;; Check again on the owner after the bounded handoff, so an
              ;; unbound/replaced route cannot silently send queued work.
              (bordeaux-threads:with-lock-held (*routes-lock*)
                (unless (eq binding (gethash (target-route-actor binding) *routes*))
                  (error 'star.actors::target-destination-unavailable :reason "stale SC01 binding")))
              (funcall (target-route-envelope-function binding) snapshot)))
       (unless (eq :queued
                   (star.router:await-send
                    (star.router.sc01-host:enqueue-dispatch
                     (target-route-host binding) dispatch #'context)))
         (error 'star.actors::target-destination-unavailable :reason "SC01 target not queued"))))))
