(in-package :star.playbook)

(defstruct (playbook-event
             (:constructor %make-playbook-event
                 (&key sequence type node-id data timestamp)))
  sequence
  type
  node-id
  data
  timestamp)

(defclass playbook-fact ()
  ((kind
    :initarg :kind
    :reader playbook-fact-kind)
   (payload
    :initarg :payload
    :initform nil
    :reader playbook-fact-payload)))

(defstruct (playbook-run
             (:constructor %make-playbook-run
                 (&key id tenant-id principal-id definition state engine)))
  id
  tenant-id
  principal-id
  definition
  (state :created)
  engine
  (events '())
  (fact-history '())
  (next-sequence 0))

(defvar *playbook-runs* (make-hash-table :test #'equal))
(defvar *playbook-runs-lock* (bt:make-lock "star-playbook-runs"))
(defvar *current-run* nil)

(defun default-effect-dispatcher (run action)
  (declare (ignore run))
  (list :status :not-connected :action (copy-tree action)))

(defparameter *effect-dispatcher* #'default-effect-dispatcher
  "Function called for typed non-local playbook effects.

The scheduler owns when this is called. Implementations must route effects
through StarIntel authorization/service boundaries rather than executing raw
host code from a playbook module.")

(defun event-now ()
  (get-universal-time))

(defun record-event (run type &key node-id data)
  (let ((event
          (%make-playbook-event
           :sequence (incf (playbook-run-next-sequence run))
           :type type
           :node-id node-id
           :data data
           :timestamp (event-now))))
    (setf (playbook-run-events run)
          (nconc (playbook-run-events run) (list event)))
    event))

(defun run-key (tenant-id run-id)
  (list (canonical-id tenant-id :tenant-id)
        (canonical-id run-id :run-id)))

(defun store-run (run)
  (bt:with-lock-held (*playbook-runs-lock*)
    (setf (gethash (run-key (playbook-run-tenant-id run)
                            (playbook-run-id run))
                   *playbook-runs*)
          run))
  run)

(defun find-playbook-run (tenant-id run-id)
  (bt:with-lock-held (*playbook-runs-lock*)
    (gethash (run-key tenant-id run-id) *playbook-runs*)))

(defun require-run (tenant-id run-id)
  (or (find-playbook-run tenant-id run-id)
      (fail-playbook :run-not-found
                     "Run ~a does not exist in tenant ~a"
                     run-id tenant-id)))

(defun list-playbook-runs (tenant-id)
  (let ((tenant-id (canonical-id tenant-id :tenant-id)))
    (bt:with-lock-held (*playbook-runs-lock*)
      (sort
       (loop for run being the hash-values of *playbook-runs*
             when (string= tenant-id (playbook-run-tenant-id run))
               collect run)
       #'string<
       :key #'playbook-run-id))))

(defun rule-symbol (flow)
  (make-symbol
   (string-upcase
    (format nil "PLAYBOOK-~a" (playbook-flow-id flow)))))

(defun install-flow-into-engine (engine flow)
  (let ((name (rule-symbol flow))
        (kind (playbook-flow-match-kind flow))
        (node-id (playbook-flow-id flow))
        (actions (playbook-flow-actions flow))
        (salience (playbook-flow-salience flow)))
    (lisa:with-inference-engine (engine)
      (eval
       `(lisa:defrule ,name (:salience ,salience)
          (star.playbook:playbook-fact (kind ,kind))
          =>
          (star.playbook::fire-current-flow ,node-id ',actions))))))

(defun install-definition-into-engine (definition engine)
  (dolist (flow (playbook-definition-flows definition))
    (install-flow-into-engine engine flow))
  engine)

(defun append-fact-history (run fact)
  (setf (playbook-run-fact-history run)
        (nconc (playbook-run-fact-history run) (list fact)))
  fact)

(defun assert-current-fact (kind payload &key node-id)
  (unless *current-run*
    (fail-playbook :no-current-run "No active playbook run"))
  (let ((fact
          (make-instance 'playbook-fact
                         :kind (canonical-kind kind :fact-kind)
                         :payload payload)))
    (lisa:assert-instance fact)
    (append-fact-history *current-run* fact)
    (record-event
     *current-run*
     :fact-asserted
     :node-id node-id
     :data (list :kind (playbook-fact-kind fact)
                 :payload (playbook-fact-payload fact)))
    fact))

(defun effect-operation-id (run node-id index)
  (digest-string
   (format nil "~a/~a/~a/~d"
           (playbook-run-id run)
           (playbook-run-next-sequence run)
           node-id
           index)))

(defun execute-flow-action (run node-id action index)
  (case (action-type action)
    (:assert-fact
     (assert-current-fact
      (getf action :kind)
      (getf action :payload)
      :node-id node-id))
    (otherwise
     (let* ((operation-id (effect-operation-id run node-id index))
            (intent (append (copy-list action)
                            (list :operation-id operation-id)))
            (result (funcall *effect-dispatcher* run intent)))
       (record-event
        run
        :effect-emitted
        :node-id node-id
        :data (list :operation-id operation-id
                    :intent intent
                    :result result))
       result))))

(defun fire-current-flow (node-id actions)
  (unless *current-run*
    (fail-playbook :no-current-run "LISA fired without a bound StarIntel run"))
  (record-event *current-run* :rule-fired :node-id node-id)
  (loop for action in actions
        for index from 0
        do (execute-flow-action *current-run* node-id action index))
  t)

(defun assert-playbook-fact (run kind &optional payload)
  (unless (typep run 'playbook-run)
    (fail-playbook :invalid-run "Expected PLAYBOOK-RUN"))
  (let ((*current-run* run))
    (lisa:with-inference-engine ((playbook-run-engine run))
      (assert-current-fact kind payload))))

(defun playbook-run-facts (run)
  (copy-list (playbook-run-fact-history run)))

(defun normalize-input-fact (fact)
  (cond
    ((stringp fact)
     (list :kind fact :payload nil))
    ((symbolp fact)
     (list :kind (symbol-name fact) :payload nil))
    ((and (listp fact) (getf fact :kind))
     (list :kind (getf fact :kind) :payload (getf fact :payload)))
    (t
     (fail-playbook :invalid-fact "Invalid input fact ~s" fact))))

(defun run-lisa-to-quiescence (run)
  (when (eq :stopped (playbook-run-state run))
    (fail-playbook :stopped-run "Run ~a is stopped" (playbook-run-id run)))
  (setf (playbook-run-state run) :running)
  (record-event run :run-resumed)
  (let ((*current-run* run))
    (lisa:with-inference-engine ((playbook-run-engine run))
      (lisa:run)))
  (unless (eq :stopped (playbook-run-state run))
    (setf (playbook-run-state run) :completed)
    (record-event run :run-quiescent))
  run)

(defun start-playbook-run
    (tenant-id playbook-id version
     &key principal-id facts (autorun t))
  (let* ((tenant-id (canonical-id tenant-id :tenant-id))
         (definition (find-playbook tenant-id playbook-id version)))
    (unless definition
      (fail-playbook :playbook-not-found
                     "Playbook ~a/~a does not exist in tenant ~a"
                     playbook-id version tenant-id))
    (let* ((engine (lisa:make-inference-engine))
           (run
             (%make-playbook-run
              :id (cms-ulid:ulid)
              :tenant-id tenant-id
              :principal-id principal-id
              :definition definition
              :state :created
              :engine engine)))
      (install-definition-into-engine definition engine)
      (store-run run)
      (record-event
       run
       :run-created
       :data (list :playbook-id (playbook-definition-id definition)
                   :version (playbook-definition-version definition)
                   :digest (playbook-definition-digest definition)
                   :principal-id principal-id))
      (dolist (fact (mapcar #'normalize-input-fact (or facts '())))
        (assert-playbook-fact run (getf fact :kind) (getf fact :payload)))
      (if autorun
          (run-lisa-to-quiescence run)
          (progn
            (setf (playbook-run-state run) :paused)
            (record-event run :run-paused)
            run)))))

(defun pause-playbook-run (tenant-id run-id)
  (let ((run (require-run tenant-id run-id)))
    (unless (member (playbook-run-state run) '(:running :created :completed) :test #'eq)
      (fail-playbook :invalid-transition
                     "Cannot pause run ~a from ~a"
                     run-id (playbook-run-state run)))
    (setf (playbook-run-state run) :paused)
    (record-event run :run-paused)
    run))

(defun resume-playbook-run (tenant-id run-id)
  (let ((run (require-run tenant-id run-id)))
    (unless (member (playbook-run-state run) '(:paused :completed) :test #'eq)
      (fail-playbook :invalid-transition
                     "Cannot resume run ~a from ~a"
                     run-id (playbook-run-state run)))
    (run-lisa-to-quiescence run)))

(defun step-playbook-run (tenant-id run-id)
  "Fire at most one LISA activation, then return to :PAUSED state."
  (let ((run (require-run tenant-id run-id)))
    (unless (member (playbook-run-state run) '(:paused :completed) :test #'eq)
      (fail-playbook :invalid-transition
                     "Cannot step run ~a from ~a"
                     run-id (playbook-run-state run)))
    (setf (playbook-run-state run) :running)
    (let ((*current-run* run))
      (lisa:with-inference-engine ((playbook-run-engine run))
        (lisa:walk 1)))
    (setf (playbook-run-state run) :paused)
    (record-event run :run-stepped)
    run))

(defun stop-playbook-run (tenant-id run-id)
  (let ((run (require-run tenant-id run-id)))
    (unless (eq :stopped (playbook-run-state run))
      (setf (playbook-run-state run) :stopped)
      (record-event run :run-stopped))
    run))
