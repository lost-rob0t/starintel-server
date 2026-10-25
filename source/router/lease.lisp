(defpackage :star.router.leases
  (:use :cl)
  (:export #:make-policy #:prepare-envelope #:verify-envelope
           #:make-outbound-observer #:canonical-attempt))
(in-package :star.router.leases)

(defstruct (policy (:constructor %make-policy)) store owner identity-function instance ttl-ms clock)
(defun reject (reason)
  (error 'star.actors::target-destination-unavailable :reason reason))
(defun milliseconds () (* 1000 (- (get-universal-time) 2208988800)))
(defun make-policy (store owner identity-function instance &key (ttl-ms 60000) (clock #'milliseconds))
  "Trusted native one-shot lease configuration; no I/O at construction.
OWNER needs targets:dispatch and targets:lease for the concrete Target. Identity
mapping is trusted deployment configuration, never a peer-supplied lock key."
  (check-type store star.leases:lease-store)
  (check-type owner star.auth:service-call-context)
  (check-type identity-function function)
  (unless (and (stringp instance) (plusp (length instance))
               (integerp ttl-ms) (<= 1000 ttl-ms 3600000) (functionp clock))
    (reject "invalid native lease policy"))
  (%make-policy :store store :owner owner :identity-function identity-function
                :instance (copy-seq instance) :ttl-ms ttl-ms :clock clock))
(defun canonical-attempt (dispatch)
  "Existing Target attempts are zero-based; canonical wire attempts start at one."
  (let ((attempt (star.actors::target-dispatch-envelope-attempt dispatch)))
    (unless (and (integerp attempt) (>= attempt 0)) (reject "invalid Target attempt"))
    (1+ attempt)))
(defun document (dispatch)
  (star.actors::target-record-document (star.actors::target-dispatch-envelope-record dispatch)))
(defun identity-for (policy document)
  (let* ((identity (funcall (policy-identity-function policy) document))
         (resource (star.authorization:resource-from-document document)))
    (check-type identity star.leases:lease-identity)
    (unless (and (equal (star.leases:lease-identity-tenant-id identity)
                        (string-downcase (star.authorization:authorization-resource-tenant-id resource)))
                 (equal (star.leases:lease-identity-target-id identity)
                        (string-downcase (star.documents:document-id document)))
                 (equal (star.leases:lease-identity-actor-name identity)
                        (string-downcase (star.documents:document-value document "actor"))))
      (reject "lease identity does not address the authorized Target"))
    identity))
(defun authorize-owner (policy document)
  (let ((deadline (star.auth:service-call-context-deadline (policy-owner policy))))
    (when (and deadline (<= deadline (get-universal-time))) (reject "lease owner expired")))
  (dolist (action '("targets:dispatch" "targets:lease"))
    (star.authorization:authorize-document! action document :principal (policy-owner policy)
                                            :actor-name (star.documents:document-value document "actor")))
  t)
(defun get-current (policy identity request-id)
  (let* ((now (funcall (policy-clock policy)))
         (outcome (star.leases:get-lease (policy-store policy) identity
                                       :deadline (+ now 1000) :request-id request-id)))
    (unless (eq :found (star.leases:lease-outcome-code outcome)) (reject "native Target lease is not active"))
    (let ((lease (star.leases:lease-outcome-lease outcome)))
      (unless (and (eq :active (star.leases:lease-record-state lease))
                   (> (star.leases:lease-record-expires-at lease) now)
                   (equal (policy-instance policy) (star.leases:lease-record-service-instance-id lease))
                   (equal (star.auth:service-call-context-principal-id (policy-owner policy))
                          (star.leases:lease-record-owner-principal-id lease)))
        (reject "native Target lease ownership or expiry changed"))
      lease)))
(defun matching-execution (lease execution-id lease-id fence)
  (unless (and (equal execution-id (star.leases:lease-record-execution-id lease))
               (equal lease-id (star.leases:lease-record-lease-id lease))
               (eql fence (star.leases:lease-record-fencing-token lease)))
    (reject "native Target execution or lease fence changed"))
  lease)

(defun prepare-envelope (policy dispatch &key (reply-budget-ms 1000))
  "Authorize then acquire a real lease BEFORE the acceptance snapshot is stored.
No release on ambiguous publication or admission failure: expiry/reconciliation
owns that recovery. Native recurring Targets require a separate per-occurrence
acceptance/lease transaction and are deliberately refused."
  (check-type policy policy)
  (let* ((record (star.actors::target-dispatch-envelope-record dispatch))
         (document (document dispatch))
         (owner (policy-owner policy))
         (execution (star.actors::target-dispatch-envelope-execution-id dispatch))
         (now (funcall (policy-clock policy)))
         (ttl (policy-ttl-ms policy)))
    (unless (and (integerp reply-budget-ms) (plusp reply-budget-ms))
      (reject "invalid native reply budget"))
    (let ((deadline (star.actors::target-dispatch-envelope-deadline dispatch)))
      (when deadline
        (setf ttl (min ttl (- (* 1000 (if (integerp deadline) deadline
                                         (local-time:timestamp-to-unix
                                          (local-time:parse-timestring deadline)))) now)))))
    (let ((deadline (star.auth:service-call-context-deadline owner)))
      (when deadline (setf ttl (min ttl (- (* 1000 (- deadline 2208988800)) now)))))
    (when (star.actors::target-record-recurring-p record) (reject "native recurring Target leases are unsupported"))
    (when (< ttl (+ 1000 reply-budget-ms (* 1000 (star.actors::target-record-delay record))))
      (reject "native Target delay exceeds lease TTL budget"))
    ;; Both resource authorization decisions precede the first lease I/O.
    (authorize-owner policy document)
    (let* ((identity (identity-for policy document))
           (snapshot (star.actors::target-dispatch-digest
                      (star.documents:document-json document)))
           (current (star.leases:get-lease (policy-store policy) identity
                                          :deadline (+ now 1000) :request-id execution)))
      (case (star.leases:lease-outcome-code current)
        (:found
         (let ((lease (get-current policy identity execution)))
           (unless (and (equal execution (star.leases:lease-record-execution-id lease))
                        (equal snapshot (jsown:val-safe (star.leases:lease-record-metadata lease) "targetDigest"))
                        (eql (canonical-attempt dispatch)
                             (jsown:val-safe (star.leases:lease-record-metadata lease) "wireAttempt")))
             (reject "active native lease has different execution or Target content"))
           ;; Reconcile the original active admission without acquiring again.
           (setf (star.actors::target-dispatch-envelope-lease-id dispatch) (star.leases:lease-record-lease-id lease)
                 (star.actors::target-dispatch-envelope-fencing-token dispatch) (star.leases:lease-record-fencing-token lease)
                 (star.actors::target-dispatch-envelope-trace-id dispatch) (star.leases:lease-record-trace-id lease))
           (return-from prepare-envelope dispatch)))
        (:not-found nil)
        (otherwise (reject "native lease reconciliation requires an active or absent lease")))
      (let ((outcome
             (star.leases:acquire-lease
              (policy-store policy) identity
              :owner-principal-id (star.auth:service-call-context-principal-id owner)
              :owner-client-id (star.auth:service-call-context-principal-id owner)
              :owner-credential-id (star.auth:service-call-context-credential-id owner)
              :service-instance-id (policy-instance policy) :ttl-ms ttl :maximum-lifetime-ms ttl
              :execution-id execution :job-id (star.actors::target-dispatch-envelope-schedule-id dispatch)
              :trace-id (star.actors::target-dispatch-envelope-trace-id dispatch)
              :metadata (jsown:new-js ("wireAttempt" (canonical-attempt dispatch))
                                      ("targetDigest" snapshot))
              :deadline (+ now 1000) :request-id execution)))
        (unless (eq :acquired (star.leases:lease-outcome-code outcome))
          (reject "native Target lease acquisition refused"))
        ;; An idempotent acquire result may outlive the actual lease: re-read it.
        (let ((lease (get-current policy identity execution)))
          (unless (equal execution (star.leases:lease-record-execution-id lease))
            (reject "native Target lease belongs to another execution"))
          (setf (star.actors::target-dispatch-envelope-lease-id dispatch) (star.leases:lease-record-lease-id lease)
                (star.actors::target-dispatch-envelope-fencing-token dispatch) (star.leases:lease-record-fencing-token lease)))))
    dispatch))

(defun verify-envelope (policy dispatch)
  (let* ((doc (document dispatch))
         (execution (star.actors::target-dispatch-envelope-execution-id dispatch)))
    (authorize-owner policy doc)
    (let ((lease (matching-execution
                  (get-current policy (identity-for policy doc) execution)
                  execution (star.actors::target-dispatch-envelope-lease-id dispatch)
                  (star.actors::target-dispatch-envelope-fencing-token dispatch))))
      (unless (eql (canonical-attempt dispatch)
                   (jsown:val-safe (star.leases:lease-record-metadata lease) "wireAttempt"))
        (reject "native Target lease attempt changed"))
      lease)))

(defun make-outbound-observer (policy reference-function &key (payload-field "target"))
  "Build OBSERVE for the outbound coordinator using actual GET-LEASE evidence.
REFERENCE-FUNCTION(context,document) reads trusted actor enrollment; generation
is not inferred from the payload. Peer context needs targets:read scope."
  (check-type reference-function function)
  (lambda (context command execution-id lease-id fence)
    (let* ((payload (cdr (assoc payload-field (getf command :payload) :test #'equal)))
           (doc (star.frontends.http-api::parse-json-octets
                 (star-actor-wire:encode-json-value payload) "application/json")))
      (star.frontends.http-api::validate-document-input doc)
      (star.authorization:authorize-document! "targets:read" doc :principal context
                                              :actor-name (star.documents:document-value doc "actor"))
      (authorize-owner policy doc)
      (let ((lease (matching-execution
                    (get-current policy (identity-for policy doc) execution-id)
                    execution-id lease-id fence)))
        (list :actor-reference (funcall reference-function context doc)
              :attempt (jsown:val-safe (star.leases:lease-record-metadata lease) "wireAttempt")
              :execution-id (star.leases:lease-record-execution-id lease)
              :lease-id (star.leases:lease-record-lease-id lease)
              :fencing-token (star.leases:lease-record-fencing-token lease)
              :lease-valid-p t :authorized-p t)))))
