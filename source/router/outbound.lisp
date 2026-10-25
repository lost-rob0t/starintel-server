(defpackage :star.router.outbound
  (:use :cl)
  (:export #:make-coordinator #:register-invocation #:mark-transport-queued
           #:observe-outcome #:invocation-snapshot #:arbitrate-invocation
           #:make-outcome-handler))
(in-package :star.router.outbound)

(defstruct (coordinator (:constructor %make-coordinator))
  manifest journal authenticate observe clock (limit 64) (wait-seconds 30)
  (index (make-hash-table :test #'equal))
  (lock (bordeaux-threads:make-lock "outbound invocation arbitration")))

(defun reject (code) (star.router.application::reject code))
(defun make-coordinator (manifest journal authenticate observe
                         &key (limit 64) (wait-seconds 30) (clock #'get-universal-time))
  "Compose durable pending invocation records with existing authentication/leases.
AUTHENTICATE(raw32-key) returns trusted context and tenant. OBSERVE(context,
command,execution-id,lease-id,fence) returns current :actor-reference, :attempt,
:execution-id, :lease-id, :fencing-token, :lease-valid-p, :authorized-p and optional
:cancel-won-p from authoritative runtime state. It must not infer these from wire
claims. JOURNAL is mandatory; no volatile fallback or automatic effect replay.
LIMIT bounds lifetime enrollment, including terminal executions. Rebuild a
coordinator and explicitly re-register stored identities for recovery; terminal
entries are never evicted to make an old invocation executable again."
  (unless (and journal (functionp authenticate) (functionp observe) (functionp clock)
               (integerp limit) (<= 1 limit 4096)
               (integerp wait-seconds) (<= 1 wait-seconds 3600))
    (reject :invalid-outbound-configuration))
  (%make-coordinator :manifest (staractorprotocol:snapshot-portable-wire-value manifest)
                    :journal journal :authenticate authenticate :observe observe
                    :limit limit :wait-seconds wait-seconds :clock clock))

(defun key-string (key)
  (unless (typep key '(simple-array (unsigned-byte 8) (32))) (reject :unauthenticated-peer))
  (format nil "~{~2,'0X~}" (coerce key 'list)))
(defun context-id (context)
  (unless (typep context 'star.auth:service-call-context) (reject :authentication-required))
  (let ((id (star.auth:service-call-context-principal-id context))
        (type (star.auth:service-call-context-principal-type context)))
    (unless (and (stringp id) (plusp (length id)) (stringp type) (plusp (length type)))
      (reject :authentication-required))
    (jsown:to-json (vector type id))))
(defun reference-json (ref)
  ;; Reconstruct using canonical authority to validate mutable ActorRef fields.
  (let ((copy (staractorprotocol:make-star-actor-reference
               :domain-id (staractorprotocol:star-actor-reference-domain-id ref)
               :logical-path (staractorprotocol:star-actor-reference-logical-path ref)
               :node-id (staractorprotocol:star-actor-reference-node-id ref)
               :generation (staractorprotocol:star-actor-reference-generation ref)
               :protocol-revision (staractorprotocol:star-actor-reference-protocol-revision ref)
               :capability-set-hash (staractorprotocol:star-actor-reference-capability-set-hash ref))))
    (jsown:new-js
      ("domain" (staractorprotocol:star-actor-reference-domain-id copy))
      ("path" (staractorprotocol:star-actor-reference-logical-path copy))
      ("node" (staractorprotocol:star-actor-reference-node-id copy))
      ("generation" (staractorprotocol:star-actor-reference-generation copy))
      ("revision" (staractorprotocol:star-actor-reference-protocol-revision copy))
      ("capabilities" (or (staractorprotocol:star-actor-reference-capability-set-hash copy) :null)))))
(defun json-reference (json)
  (staractorprotocol:make-star-actor-reference
   :domain-id (jsown:val json "domain") :logical-path (jsown:val json "path")
   :node-id (jsown:val json "node") :generation (jsown:val json "generation")
   :protocol-revision (jsown:val json "revision")
   :capability-set-hash (let ((value (jsown:val json "capabilities")))
                          (unless (eq value :null) value))))
(defun wire-text (coordinator envelope)
  (babel:octets-to-string
   (star-actor-wire:encode-binding-message :zmq '("star.actor2actor/1")
                                          (coordinator-manifest coordinator) envelope)
   :encoding :utf-8))
(defun row-command (coordinator row)
  (star-actor-wire:decode-command-message
   :zmq '("star.actor2actor/1") (coordinator-manifest coordinator)
   (babel:string-to-octets (jsown:val row "request") :encoding :utf-8)))
(defun row-key (coordinator invocation)
  (or (gethash invocation (coordinator-index coordinator)) (reject :unknown-invocation)))
(defun row-observation (coordinator row context command)
  (let ((observation (funcall (coordinator-observe coordinator) context command
                             (jsown:val row "executionId") (jsown:val row "leaseId")
                             (jsown:val row "fencingToken"))))
    (unless (and (eq t (getf observation :authorized-p))
                 (eq t (getf observation :lease-valid-p))
                 (equal (jsown:val row "executionId") (getf observation :execution-id))
                 (equal (jsown:val row "leaseId") (getf observation :lease-id))
                 (eql (jsown:val row "fencingToken") (getf observation :fencing-token)))
      (reject :stale-or-unauthorized-target-fence))
    observation))

(defun register-invocation (coordinator command reference context peer-key tenant
                            execution-id lease-id fencing-token)
  "Durably register BEFORE enqueue. Re-registration reconciles exact stored identity.
Target execution, Target lease fence and actor incarnation are separate checks."
  (unless (and (eq :command (getf command :kind))
               (equal execution-id (getf command :message-id))
               (stringp tenant) (plusp (length tenant))
               (stringp lease-id) (plusp (length lease-id))
               (integerp fencing-token) (plusp fencing-token))
    (reject :invalid-pending-invocation))
  (let* ((request (wire-text coordinator command))
         (principal (context-id context))
         (key (star.router.journal:journal-key
               principal tenant "outbound"
               (list (getf command :actor) (getf command :message-type)
                     (getf command :idempotency-key) execution-id)))
         (disposition nil)
         (identity (jsown:new-js ("request" request) ("reference" (reference-json reference))
                                ("principal" principal) ("tenant" tenant)
                                ("peerKey" (key-string peer-key)) ("executionId" execution-id)
                                ("leaseId" lease-id) ("fencingToken" fencing-token)
                                ("attempt" (getf command :attempt)))))
    (bordeaux-threads:with-lock-held ((coordinator-lock coordinator))
      (let ((previous (gethash execution-id (coordinator-index coordinator))))
        (when (and previous (not (equal previous key))) (reject :invocation-identity-conflict)))
      (when (and (not (gethash execution-id (coordinator-index coordinator)))
                 (>= (hash-table-count (coordinator-index coordinator)) (coordinator-limit coordinator)))
        (reject :pending-invocation-limit))
      (let ((observation (row-observation coordinator identity context command)))
        (unless (and (equal (reference-json reference)
                            (reference-json (getf observation :actor-reference)))
                     (eql (getf command :attempt) (getf observation :attempt))
                     (not (getf observation :cancel-won-p)))
          (reject :stale-invocation-admission)))
      (setf disposition (star.router.journal:journal-transition
       (coordinator-journal coordinator) key
       (lambda (existing)
         (if existing
             (progn
               (unless (equal (jsown:val existing "identity") identity)
                 (reject :invocation-identity-conflict))
               (values nil :existing))
             (let* ((row (copy-tree identity))
                    (wait-until (+ (funcall (coordinator-clock coordinator))
                                   (coordinator-wait-seconds coordinator)))
                    (deadline (getf command :deadline)))
               (when deadline
                 (setf wait-until
                       (min wait-until (local-time:timestamp-to-universal
                                        (local-time:parse-timestring deadline)))))
               (when (<= wait-until (funcall (coordinator-clock coordinator)))
                 (reject :deadline-exceeded))
               (setf (jsown:val row "identity") identity
                     (jsown:val row "state") "pending"
                     (jsown:val row "terminal") "no"
                     (jsown:val row "waitUntil") wait-until
                     (jsown:val row "sequence") 0)
               (values row :registered))))))
      (setf (gethash (copy-seq execution-id) (coordinator-index coordinator)) key))
    (values execution-id disposition)))

(defun invocation-snapshot (coordinator invocation)
  (bordeaux-threads:with-lock-held ((coordinator-lock coordinator))
    (star.router.journal:journal-read (coordinator-journal coordinator) (row-key coordinator invocation))))

(defun mark-transport-queued (coordinator invocation)
  "Record only socket queue acceptance; never actor admission or completion."
  (bordeaux-threads:with-lock-held ((coordinator-lock coordinator))
    (star.router.journal:journal-transition
     (coordinator-journal coordinator) (row-key coordinator invocation)
     (lambda (row)
       (unless row (reject :missing-durable-invocation))
       (if (equal "pending" (jsown:val row "state"))
           (progn (setf (jsown:val row "state") "transport-queued") (values row :transport-queued))
           (values nil :unchanged))))))

(defun authenticated-observation (coordinator row peer-key)
  (unless (equal (key-string peer-key) (jsown:val row "peerKey")) (reject :wrong-peer-key))
  (multiple-value-bind (context tenant) (funcall (coordinator-authenticate coordinator) peer-key)
    (unless (and (equal (context-id context) (jsown:val row "principal"))
                 (equal tenant (jsown:val row "tenant")))
      (reject :wrong-peer-principal))
    (let ((deadline (star.auth:service-call-context-deadline context)))
      (when (and deadline (<= deadline (funcall (coordinator-clock coordinator))))
        (reject :session-expired)))
    (row-observation coordinator row context (row-command coordinator row))))

(defun observe-outcome (coordinator outcome peer-key)
  "Authenticate and fence canonical ACK/reply/error before durable arbitration.
Only canonical terminal evidence settles completion. A reply is retained as
evidence, not silently upgraded to a completed acknowledgement."
  (let ((text (wire-text coordinator outcome)) (invocation (getf outcome :causation-id)))
    (bordeaux-threads:with-lock-held ((coordinator-lock coordinator))
      (star.router.journal:journal-transition
       (coordinator-journal coordinator) (row-key coordinator invocation)
       (lambda (row)
         (unless row (reject :missing-durable-invocation))
         (let* ((command (row-command coordinator row))
                (observed (authenticated-observation coordinator row peer-key))
                (decision
                  (staractorprotocol:actor2actor-completion-decision
                   command outcome (json-reference (jsown:val row "reference"))
                   (getf observed :actor-reference)
                   :expected-attempt (jsown:val row "attempt") :observed-attempt (getf observed :attempt)
                   :terminal-p (equal "yes" (jsown:val row "terminal"))
                   :deadline-expired-p (>= (funcall (coordinator-clock coordinator)) (jsown:val row "waitUntil"))
                   :cancel-won-p (eq t (getf observed :cancel-won-p)))))
           (unless (eq decision :accept) (return-from observe-outcome decision))
           (when (equal text (jsown:val-safe row "lastOutcome"))
             (return-from observe-outcome :duplicate))
           (setf (jsown:val row "lastOutcome") text
                 (jsown:val row "state")
                 (string-downcase (symbol-name (staractorprotocol:actor2actor-status-for-envelope outcome)))
                 (jsown:val row "terminal")
                 (if (and (not (eq :outcome-unknown
                                  (staractorprotocol:actor2actor-status-for-envelope outcome)))
                          (staractorprotocol:terminal-lifecycle-envelope-p outcome)) "yes" "no"))
           (when (eq :reply (getf outcome :kind)) (setf (jsown:val row "reply") text))
           (incf (jsown:val row "sequence"))
           (values row :accepted)))))))

(defun arbitrate-invocation (coordinator invocation peer-key)
  "Persist authoritative cancellation/deadline arbitration without asserting rollback.
Cancel wins only when the trusted runtime observer reports it. A local wait
deadline records outcome-unknown; it is not evidence that remote effects stopped."
  (bordeaux-threads:with-lock-held ((coordinator-lock coordinator))
    (star.router.journal:journal-transition
     (coordinator-journal coordinator) (row-key coordinator invocation)
     (lambda (row)
       (unless row (reject :missing-durable-invocation))
       (let ((observed (authenticated-observation coordinator row peer-key)))
         (cond
           ((equal "yes" (jsown:val row "terminal")) (values nil :already-terminal))
           ((eq t (getf observed :cancel-won-p))
            (setf (jsown:val row "terminal") "yes" (jsown:val row "state") "cancelled")
            (values row :cancelled))
           ((>= (funcall (coordinator-clock coordinator)) (jsown:val row "waitUntil"))
            (setf (jsown:val row "observation") "outcome-unknown")
            (values row :deadline-exceeded))
           (t (values nil :pending))))))))

(defun make-outcome-handler (coordinator inbound-handler)
  "Host callback routes outcomes to the pending authority; commands stay inbound."
  (lambda (route envelope peer-key)
    (if (member (getf envelope :kind) '(:ack :reply :error))
        (progn (observe-outcome coordinator envelope peer-key) nil)
        (funcall inbound-handler route envelope peer-key))))
