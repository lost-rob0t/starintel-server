(defpackage :star.router.lifecycle
  (:use :cl)
  (:export #:make-runtime-binding #:runtime-submit-envelope #:runtime-run-next
           #:runtime-handle-envelope #:runtime-finish-deferred))
(in-package :star.router.lifecycle)

(defstruct (runtime-binding (:constructor %make-runtime-binding))
  manifest actor handler authenticate clock owner max-peers max-requests max-queue
  journal durable-p journal-owner journal-checkpoint
  (sessions (make-hash-table :test #'equal)))
(defstruct runtime-session dispatcher context identity (requests 0))

(defun utc-clock ()
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(defun make-runtime-binding (manifest actor handler authenticate
                             &key (max-peers 8) (max-requests-per-peer 64)
                               (max-queue 8) (clock #'utc-clock)
                               journal (durable-p t) journal-checkpoint)
  "Host-side composition of the canonical StarLang deterministic dispatcher.
AUTHENTICATE receives a transport-authenticated CURVE public key and returns a
trusted service context or NIL. HANDLER receives that context and a canonical
command, returning a StarLang complete/retry/fail/defer result. It must perform
application authorization and commit before returning a completed effect.
Durable mode requires JOURNAL and persists admission before effects and canonical
outcomes before emission. Recovery of an ambiguous effect never executes again.
Volatile mode requires explicit :DURABLE-P NIL and offers no restart protection.
JOURNAL-CHECKPOINT(stage command), when supplied by trusted configuration, runs
after durable :ADMITTED/:STARTED/:TERMINAL transitions, for crash acceptance."
  (unless (and (functionp handler) (functionp authenticate) (functionp clock)
               (stringp actor) (plusp (length actor)))
    (star.router.application::reject :invalid-runtime-binding))
  (unless (and (member durable-p '(nil t))
               (or (not durable-p) journal)
               (or (null journal-checkpoint) (functionp journal-checkpoint)))
    (star.router.application::reject :durable-journal-required))
  (when durable-p (check-type journal star.router.journal::journal-store))
  (dolist (bound (list max-peers max-requests-per-peer max-queue))
    (unless (and (integerp bound) (<= 1 bound 1024))
      (star.router.application::reject :invalid-runtime-bound)))
  (let ((snapshot (staractorprotocol:snapshot-portable-wire-value manifest)))
    ;; Validate the actual runtime contract and destination before any session.
    (starlangruntime:register-dispatch-actor
     (starlangruntime:make-deterministic-dispatcher snapshot :now (funcall clock))
     actor (lambda (&rest ignored) (declare (ignore ignored))))
    (%make-runtime-binding :manifest snapshot :actor (copy-seq actor)
                           :handler handler :authenticate authenticate :clock clock
                           :owner (bordeaux-threads:current-thread)
                           :max-peers max-peers :max-requests max-requests-per-peer
                           :max-queue max-queue :journal journal :durable-p durable-p
                           :journal-owner (princ-to-string (uuid:make-v4-uuid))
                           :journal-checkpoint journal-checkpoint)))

(defun context-identity (context)
  (list (star.auth:service-call-context-principal-id context)
        (star.auth:service-call-context-principal-type context)
        (star.auth:service-call-context-credential-id context)
        (star.auth:service-call-context-scopes context)))

(defun authenticated-session (binding peer-key)
  (unless (and (eq (runtime-binding-owner binding) (bordeaux-threads:current-thread))
               (typep peer-key '(simple-array (unsigned-byte 8) (32))))
    (star.router.application::reject :invalid-runtime-owner-or-peer))
  (let* ((key (format nil "~{~2,'0X~}" (coerce peer-key 'list)))
         (context (funcall (runtime-binding-authenticate binding) (copy-seq peer-key)))
         (session (gethash key (runtime-binding-sessions binding))))
    (unless (and (typep context 'star.auth:service-call-context)
                 (stringp (star.auth:service-call-context-principal-id context))
                 (plusp (length (star.auth:service-call-context-principal-id context))))
      (star.router.application::reject :authentication-required))
    (let ((deadline (star.auth:service-call-context-deadline context)))
      (when (and deadline (<= deadline (get-universal-time)))
        (star.router.application::reject :session-expired)))
    (if session
        (unless (equal (runtime-session-identity session) (context-identity context))
          (star.router.application::reject :session-authority-changed))
        (progn
          (when (>= (hash-table-count (runtime-binding-sessions binding))
                    (runtime-binding-max-peers binding))
            (star.router.application::reject :peer-budget-exhausted))
          (setf session
                (make-runtime-session
                 :context context :identity (context-identity context)
                 :dispatcher (starlangruntime:make-deterministic-dispatcher
                              (runtime-binding-manifest binding)
                              :now (funcall (runtime-binding-clock binding)))))
          (starlangruntime:register-dispatch-actor
           (runtime-session-dispatcher session) (runtime-binding-actor binding)
           (lambda (dispatcher command)
             (declare (ignore dispatcher))
             (if (runtime-binding-durable-p binding)
                 (durable-invoke-handler binding session command)
                 (funcall (runtime-binding-handler binding)
                          (runtime-session-context session) command))))
          (setf (gethash key (runtime-binding-sessions binding)) session)))
    (setf (runtime-session-context session) context)
    (starlangruntime:advance-dispatcher-clock
     (runtime-session-dispatcher session) (funcall (runtime-binding-clock binding)))
    session))

(defun tracked-cancel-p (dispatcher envelope)
  "Bound both cancellation indexes to a single already tracked command pair.
StarLang applies cancellation by message OR correlation; admitting a mixed pair
would therefore let arbitrary new keys grow its cancellation tables."
  (when (eq :cancel (getf envelope :kind))
    (let ((message (staractorprotocol:cancel-target-message-id envelope))
          (correlation (staractorprotocol:cancel-target-correlation-id envelope)))
      (labels ((matches (command)
                 (and (equal message (getf command :message-id))
                      (equal correlation (getf command :correlation-id)))))
        (or (some #'matches (starlangruntime:deterministic-dispatcher-queue dispatcher))
            (loop for record being the hash-values of
                    (starlangruntime:deterministic-dispatcher-idempotency dispatcher)
                  thereis (matches (getf record :command))))))))

(defun runtime-submit-envelope (binding envelope peer-key)
  "Admit command/cancel using the canonical runtime; return emitted envelopes.
ACK/reply/event/error are output/control observations, never command admission."
  (let* ((session (authenticated-session binding peer-key))
         (dispatcher (runtime-session-dispatcher session))
         (owned (staractorprotocol:snapshot-portable-wire-value envelope)))
    ;; Codec validation bounds bytes and checks the supplied generated manifest.
    (star-actor-wire:encode-binding-message :zmq '("star.actor2actor/1")
                                          (runtime-binding-manifest binding) owned)
    (unless (member (getf owned :kind) '(:command :cancel))
      (star.router.application::reject :not-runtime-input))
    (unless (equal (getf owned :actor) (runtime-binding-actor binding))
      (star.router.application::reject :unbound-operation))
    (when (and (>= (runtime-session-requests session) (runtime-binding-max-requests binding))
               (not (tracked-cancel-p dispatcher owned)))
      (star.router.application::reject :session-budget-exhausted))
    (when (and (eq :command (getf owned :kind))
               (>= (length (starlangruntime:deterministic-dispatcher-queue dispatcher))
                   (runtime-binding-max-queue binding)))
      (star.router.application::reject :queue-full))
    (when (runtime-binding-durable-p binding)
      (multiple-value-bind (admit-p outcomes)
          (durable-admit-envelope binding session owned)
        (unless admit-p
          (return-from runtime-submit-envelope (values outcomes :observation)))))
    ;; A cancel for an exact retained target pair cannot add unbounded keys:
    ;; its own message ID is not indexed by the canonical cancellation tables.
    (unless (tracked-cancel-p dispatcher owned)
      (incf (runtime-session-requests session)))
    (starlangruntime:submit-dispatch-envelope dispatcher owned)
    (let ((outputs (starlangruntime:drain-dispatcher-emitted dispatcher)))
      (when (and (runtime-binding-durable-p binding) (eq :cancel (getf owned :kind)))
        (durable-checkpoint-cancel-results binding session dispatcher))
      (values outputs :admitted))))

(defun runtime-run-next (binding peer-key)
  "Execute one admitted command on the owner thread and return canonical outputs."
  (let* ((session (authenticated-session binding peer-key))
         (dispatcher (runtime-session-dispatcher session))
         (command (first (starlangruntime:deterministic-dispatcher-queue dispatcher))))
    (starlangruntime:run-dispatcher-next dispatcher)
    (let ((outputs (starlangruntime:drain-dispatcher-emitted dispatcher)))
      (when (and command (runtime-binding-durable-p binding))
        (setf outputs (receiver-public-outcomes binding command outputs))
        (durable-checkpoint-result binding session dispatcher command))
      outputs)))

(defun runtime-handle-envelope (binding envelope peer-key)
  "Synchronous host callback; output is a LIST of canonical lifecycle envelopes."
  (multiple-value-bind (outputs admission)
      (runtime-submit-envelope binding envelope peer-key)
    ;; A journal replay/unknown observation admits no work. It must not consume
    ;; a different command already waiting in this session.
    (if (eq admission :observation) outputs
        (append outputs (runtime-run-next binding peer-key)))))

(defun runtime-finish-deferred (binding peer-key command result)
  "Owner-thread completion through canonical late-terminal/cancel handling."
  (let* ((session (authenticated-session binding peer-key))
         (dispatcher (runtime-session-dispatcher session)))
    (let ((record (gethash (staractorprotocol:idempotency-scope-key command)
                           (starlangruntime:deterministic-dispatcher-idempotency dispatcher))))
      (when record
        (starlangruntime:validate-command-idempotency-identity
         (getf record :command) command)
        ;; Delivery IDs may vary under canonical idempotency comparison, but
        ;; deferred settlement belongs to the delivery that actually started.
        (setf command (staractorprotocol:snapshot-portable-wire-value
                       (getf record :command)))))
    (starlangruntime:finish-deferred-dispatch dispatcher command result)
    (let ((outputs (starlangruntime:drain-dispatcher-emitted dispatcher)))
      (when (runtime-binding-durable-p binding)
        (setf outputs (receiver-public-outcomes binding command outputs))
        (durable-checkpoint-result binding session dispatcher command))
      outputs)))
