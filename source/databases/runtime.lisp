(in-package :star.databases.runtime)

(define-condition database-runtime-error (error)
  ((code :initarg :code :reader database-runtime-error-code)
   (retryable-p :initarg :retryable-p :initform nil
                :reader database-runtime-error-retryable-p)
   (details :initarg :details :initform nil
            :reader database-runtime-error-details))
  (:report
   (lambda (condition stream)
     (format stream "Database runtime error ~A"
             (database-runtime-error-code condition)))))

(defun fail-database-runtime (code &key retryable-p details)
  (error 'database-runtime-error
         :code code
         :retryable-p retryable-p
         :details details))

(defun required-runtime-name (value context)
  (unless (and (stringp value)
               (plusp (length value))
               (<= (length value) 256))
    (fail-database-runtime
     :invalid-request
     :details (list :field context)))
  value)

(defun normalize-database-access (value)
  (unless (member value
                  '(:read :write :transaction :subscribe :logic :admin)
                  :test #'eq)
    (fail-database-runtime
     :invalid-request
     :details (list :field :access :value value)))
  value)

(defun database-action-for-access (access)
  (ecase (normalize-database-access access)
    (:read "database:read")
    (:write "database:write")
    (:transaction "database:transaction")
    (:subscribe "database:subscribe")
    (:logic "database:read")
    (:admin "database:admin")))

(defclass database-backend ()
  ((id :initarg :id :reader database-backend-id))
  (:documentation
   "Abstract database adapter. Concrete backends implement only the access
classes they support."))

(defgeneric database-backend-health (backend)
  (:documentation "Return true when BACKEND is ready to accept work."))

(defgeneric database-execute-read
    (backend operation bindings context)
  (:documentation "Execute one trusted named read operation."))

(defgeneric database-execute-write
    (backend operation bindings context idempotency-key precondition)
  (:documentation "Execute one trusted named mutation."))

(defgeneric database-execute-transaction
    (backend operation bindings context idempotency-key)
  (:documentation "Execute one bounded trusted transaction operation."))

(defgeneric database-subscribe
    (backend operation bindings context)
  (:documentation "Open one bounded subscription and return its result/token."))

(defgeneric database-cancel-subscription
    (backend subscription-id context)
  (:documentation "Cancel one active backend subscription."))

(defgeneric database-close (backend)
  (:documentation "Release resources held by BACKEND."))

(defmethod database-backend-health ((backend database-backend))
  (declare (ignore backend))
  t)

(defmethod database-execute-read
    ((backend database-backend) operation bindings context)
  (declare (ignore backend operation bindings context))
  (fail-database-runtime :operation-unavailable))

(defmethod database-execute-write
    ((backend database-backend) operation bindings context idempotency-key precondition)
  (declare
   (ignore backend operation bindings context idempotency-key precondition))
  (fail-database-runtime :operation-unavailable))

(defmethod database-execute-transaction
    ((backend database-backend) operation bindings context idempotency-key)
  (declare
   (ignore backend operation bindings context idempotency-key))
  (fail-database-runtime :operation-unavailable))

(defmethod database-subscribe
    ((backend database-backend) operation bindings context)
  (declare (ignore backend operation bindings context))
  (fail-database-runtime :operation-unavailable))

(defmethod database-cancel-subscription
    ((backend database-backend) subscription-id context)
  (declare (ignore backend subscription-id context))
  (fail-database-runtime :operation-unavailable))

(defmethod database-close ((backend database-backend))
  (declare (ignore backend))
  t)

(defstruct (database-profile
            (:constructor %make-database-profile))
  name
  backend-id
  options)

(defstruct (database-operation
            (:constructor %make-database-operation))
  name
  profile
  access
  descriptor
  result-limit
  timeout-ms)

(defstruct (database-call-context
            (:constructor make-database-call-context
                (&key principal tenant-id dataset-id
                      correlation-id deadline)))
  principal
  tenant-id
  dataset-id
  correlation-id
  deadline)

(defstruct (database-request
            (:constructor make-database-request
                (&key operation bindings context
                      idempotency-key precondition)))
  operation
  bindings
  context
  idempotency-key
  precondition)

(defstruct (database-result
            (:constructor %make-database-result))
  status
  operation
  value
  error-code
  retryable-p
  details)

(defstruct (database-runtime-command
            (:constructor make-database-runtime-command
                (&key request)))
  request)

(defvar *database-backends* (make-hash-table :test #'equal))
(defvar *database-profiles* (make-hash-table :test #'equal))
(defvar *database-operations* (make-hash-table :test #'equal))
(defvar *database-runtime-lock*
  (make-lock "starintel-database-runtime"))

(defun clear-database-runtime ()
  "Clear all runtime registries and close registered backends."
  (with-lock-held (*database-runtime-lock*)
    (maphash
     (lambda (id backend)
       (declare (ignore id))
       (ignore-errors (database-close backend)))
     *database-backends*)
    (clrhash *database-backends*)
    (clrhash *database-profiles*)
    (clrhash *database-operations*))
  t)

(defun register-database-backend (id backend)
  (required-runtime-name id :backend-id)
  (unless (typep backend 'database-backend)
    (fail-database-runtime
     :invalid-request
     :details (list :field :backend :value (type-of backend))))
  (unless (string= id (database-backend-id backend))
    (fail-database-runtime
     :invalid-request
     :details (list :field :backend-id :reason :identity-mismatch)))
  (with-lock-held (*database-runtime-lock*)
    (when (gethash id *database-backends*)
      (fail-database-runtime
       :conflict
       :details (list :backend-id id)))
    (setf (gethash id *database-backends*) backend))
  backend)

(defun unregister-database-backend (id)
  (with-lock-held (*database-runtime-lock*)
    (let ((backend (gethash id *database-backends*)))
      (when backend
        (database-close backend)
        (remhash id *database-backends*))
      backend)))

(defun find-database-backend (id)
  (gethash id *database-backends*))

(defun register-database-profile (name backend-id &key options)
  (required-runtime-name name :profile)
  (required-runtime-name backend-id :backend-id)
  (unless (find-database-backend backend-id)
    (fail-database-runtime
     :database-unavailable
     :details (list :backend-id backend-id)))
  (let ((profile
          (%make-database-profile
           :name name
           :backend-id backend-id
           :options (copy-tree options))))
    (with-lock-held (*database-runtime-lock*)
      (when (gethash name *database-profiles*)
        (fail-database-runtime
         :conflict
         :details (list :profile name)))
      (setf (gethash name *database-profiles*) profile))
    profile))

(defun unregister-database-profile (name)
  (with-lock-held (*database-runtime-lock*)
    (prog1 (gethash name *database-profiles*)
      (remhash name *database-profiles*))))

(defun find-database-profile (name)
  (gethash name *database-profiles*))

(defun register-database-operation
    (name profile access descriptor
     &key result-limit timeout-ms)
  (required-runtime-name name :operation)
  (required-runtime-name profile :profile)
  (normalize-database-access access)
  (unless (find-database-profile profile)
    (fail-database-runtime
     :operation-unavailable
     :details (list :profile profile)))
  (when (and result-limit
             (not (and (integerp result-limit)
                       (plusp result-limit))))
    (fail-database-runtime
     :invalid-request
     :details (list :field :result-limit)))
  (when (and timeout-ms
             (not (and (integerp timeout-ms)
                       (plusp timeout-ms))))
    (fail-database-runtime
     :invalid-request
     :details (list :field :timeout-ms)))
  (let ((operation
          (%make-database-operation
           :name name
           :profile profile
           :access access
           :descriptor (copy-tree descriptor)
           :result-limit result-limit
           :timeout-ms timeout-ms)))
    (with-lock-held (*database-runtime-lock*)
      (when (gethash name *database-operations*)
        (fail-database-runtime
         :conflict
         :details (list :operation name)))
      (setf (gethash name *database-operations*) operation))
    operation))

(defun unregister-database-operation (name)
  (with-lock-held (*database-runtime-lock*)
    (prog1 (gethash name *database-operations*)
      (remhash name *database-operations*))))

(defun find-database-operation (name)
  (gethash name *database-operations*))

(defun context-correlation-id (context)
  (or (database-call-context-correlation-id context)
      (let ((principal
              (database-call-context-principal context)))
        (and (typep principal 'star.auth:service-call-context)
             (star.auth:service-call-context-correlation-id principal)))))

(defun validate-database-call-context (context)
  (unless (database-call-context-p context)
    (fail-database-runtime
     :invalid-request
     :details (list :field :context)))
  (unless (database-call-context-principal context)
    (fail-database-runtime
     :unauthorized
     :details (list :reason :missing-principal)))
  (required-runtime-name
   (database-call-context-tenant-id context)
   :tenant-id)
  (when (database-call-context-dataset-id context)
    (required-runtime-name
     (database-call-context-dataset-id context)
     :dataset-id))
  (when (context-correlation-id context)
    (required-runtime-name
     (context-correlation-id context)
     :correlation-id))
  context)

(defun authorize-database-operation! (operation context)
  (star.authorization:authorize!
   (database-action-for-access
    (database-operation-access operation))
   :principal (database-call-context-principal context)
   :resource
   (star.authorization:make-authorization-resource
    :tenant-id (database-call-context-tenant-id context)
    :dataset-id (database-call-context-dataset-id context)
    :database-id (database-operation-profile operation)
    :resource-id (database-operation-name operation))
   :metadata
   (list :correlation-id (context-correlation-id context))))

(defun ensure-write-idempotency (request operation)
  (when (member (database-operation-access operation)
                '(:write :transaction)
                :test #'eq)
    (required-runtime-name
     (database-request-idempotency-key request)
     :idempotency-key)))

(defun execute-operation-on-backend
    (backend operation request context)
  (let ((descriptor (database-operation-descriptor operation))
        (bindings (database-request-bindings request)))
    (ecase (database-operation-access operation)
      ((:read :logic)
       (database-execute-read
        backend descriptor bindings context))
      (:write
       (database-execute-write
        backend descriptor bindings context
        (database-request-idempotency-key request)
        (database-request-precondition request)))
      (:transaction
       (database-execute-transaction
        backend descriptor bindings context
        (database-request-idempotency-key request)))
      (:subscribe
       (database-subscribe
        backend descriptor bindings context))
      (:admin
       (fail-database-runtime :operation-unavailable)))))

(defun database-error-result (operation condition)
  (typecase condition
    (database-runtime-error
     (%make-database-result
      :status :error
      :operation operation
      :error-code (database-runtime-error-code condition)
      :retryable-p
      (database-runtime-error-retryable-p condition)
      :details
      (database-runtime-error-details condition)))
    (star.authorization:authorization-error
     (%make-database-result
      :status :error
      :operation operation
      :error-code :capability-denied
      :retryable-p nil
      :details nil))
    (t
     (%make-database-result
      :status :error
      :operation operation
      :error-code :backend-fault
      :retryable-p nil
      :details
      (list :condition-type
            (string-downcase
             (princ-to-string (type-of condition))))))))

(defun execute-database-request (request)
  "Resolve, authorize and execute one trusted named database operation."
  (unless (database-request-p request)
    (fail-database-runtime
     :invalid-request
     :details (list :field :request)))
  (let* ((name
           (required-runtime-name
            (database-request-operation request)
            :operation))
         (context
           (validate-database-call-context
            (database-request-context request)))
         (operation (find-database-operation name)))
    (unless operation
      (return-from execute-database-request
        (%make-database-result
         :status :error
         :operation name
         :error-code :operation-unavailable
         :retryable-p nil)))
    (handler-case
        (progn
          (ensure-write-idempotency request operation)
          (authorize-database-operation! operation context)
          (let* ((profile
                   (find-database-profile
                    (database-operation-profile operation)))
                 (backend
                   (and profile
                        (find-database-backend
                         (database-profile-backend-id profile)))))
            (unless (and backend
                         (database-backend-health backend))
              (fail-database-runtime
               :database-unavailable
               :retryable-p t))
            (%make-database-result
             :status :success
             :operation name
             :value
             (execute-operation-on-backend
              backend operation request context)
             :retryable-p nil)))
      (error (condition)
        (database-error-result name condition)))))

(defun complete-database-command (result)
  (when *sender*
    (reply result *sender*))
  result)

(defun make-database-runtime-handler ()
  "Create the Sento handler for database-runtime-command messages."
  (lambda (message)
    (complete-database-command
     (handler-case
         (etypecase message
           (database-runtime-command
            (execute-database-request
             (database-runtime-command-request message)))
           (database-request
            (execute-database-request message)))
       (error (condition)
         (database-error-result nil condition))))))

(defun start-database-runtime-actor
    (system &key (name "database-runtime")
                 (dispatcher :pinned))
  "Start one supervised Sento database execution actor."
  (let ((actor
          (actor-of
           system
           :name name
           :dispatcher dispatcher
           :receive (make-database-runtime-handler))))
    (when (and (boundp 'star.actors:*actor-index-agent*)
               star.actors:*actor-index-agent*)
      (star.actors:register-actor name actor))
    actor))

(defclass fake-database-backend (database-backend)
  ((calls :initform '()
          :accessor fake-database-backend-calls)
   (healthy-p :initarg :healthy-p
              :initform t
              :accessor fake-database-backend-healthy-p))
  (:documentation "Hermetic adapter used by the shared DB runtime contract tests."))

(defun make-fake-database-backend
    (id &key (healthy-p t))
  (make-instance 'fake-database-backend
                 :id id
                 :healthy-p healthy-p))

(defun record-fake-call (backend access operation bindings &rest extra)
  (push
   (append
    (list :access access
          :operation (copy-tree operation)
          :bindings (copy-tree bindings))
    extra)
   (fake-database-backend-calls backend))
  (list :backend (database-backend-id backend)
        :access access
        :operation (copy-tree operation)
        :bindings (copy-tree bindings)))

(defmethod database-backend-health
    ((backend fake-database-backend))
  (fake-database-backend-healthy-p backend))

(defmethod database-execute-read
    ((backend fake-database-backend) operation bindings context)
  (declare (ignore context))
  (record-fake-call backend :read operation bindings))

(defmethod database-execute-write
    ((backend fake-database-backend) operation bindings context
     idempotency-key precondition)
  (declare (ignore context))
  (record-fake-call
   backend :write operation bindings
   :idempotency-key idempotency-key
   :precondition (copy-tree precondition)))

(defmethod database-execute-transaction
    ((backend fake-database-backend) operation bindings context
     idempotency-key)
  (declare (ignore context))
  (record-fake-call
   backend :transaction operation bindings
   :idempotency-key idempotency-key))

(defmethod database-subscribe
    ((backend fake-database-backend) operation bindings context)
  (declare (ignore context))
  (record-fake-call backend :subscribe operation bindings))
