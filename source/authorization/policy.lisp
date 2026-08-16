(in-package :star.authorization)

(defparameter +capabilities+
  '("identity:read"
    "documents:read"
    "documents:write"
    "documents:delete"
    "documents:bulk"
    "search:read"
    "views:read"
    "targets:read"
    "targets:dispatch"
    "targets:lease"
    "targets:force-release"
    "events:write"
    "events:replay"
    "credentials:read"
    "credentials:create"
    "credentials:rotate"
    "credentials:revoke"
    "credentials:disable"
    "principals:manage"
    "audit:read")
  "Closed capability vocabulary. Unknown capabilities are rejected at issuance.")

(defparameter +scope-prefixes+
  '("tenant:"
    "dataset:"
    "actor:"
    "target:"
    "target-namespace:"
    "program:")
  "Closed resource-scope vocabulary. Values follow the prefix and may be `*`.")

(define-condition authorization-error (error)
  ((code
    :initarg :code
    :initform "access_denied"
    :reader authorization-error-code)
   (decision
    :initarg :decision
    :reader authorization-error-decision))
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (write-string "Access denied" stream))))

(defstruct (authorization-resource
            (:constructor make-authorization-resource
                (&key tenant-id dataset-id actor-name target-id
                      target-namespace program-id resource-id dtype))
            (:copier nil))
  (tenant-id nil :read-only t)
  (dataset-id nil :read-only t)
  (actor-name nil :read-only t)
  (target-id nil :read-only t)
  (target-namespace nil :read-only t)
  (program-id nil :read-only t)
  (resource-id nil :read-only t)
  (dtype nil :read-only t))

(defstruct (authorization-request
            (:constructor make-authorization-request
                (&key principal action resource metadata quotas))
            (:copier nil))
  (principal nil :read-only t)
  (action nil :read-only t)
  (resource nil :read-only t)
  (metadata nil :read-only t)
  (quotas nil :read-only t))

(defstruct (authorization-decision
            (:constructor make-authorization-decision
                (&key id allowed-p reason action resource principal-id))
            (:copier nil))
  (id nil :read-only t)
  (allowed-p nil :read-only t)
  (reason nil :read-only t)
  (action nil :read-only t)
  (resource nil :read-only t)
  (principal-id nil :read-only t))

(defstruct (trusted-authorization-context
            (:constructor make-trusted-authorization-context
                (&key id principal-type scopes))
            (:copier nil))
  (id nil :read-only t)
  (principal-type nil :read-only t)
  (scopes nil :read-only t))

(defclass policy-engine () ())
(defclass default-deny-policy-engine (policy-engine) ())

(defgeneric evaluate-authorization (engine request))

(defvar *policy-engine* (make-instance 'default-deny-policy-engine))
(defvar *trusted-authorization-context* nil)
(defvar *current-authorization-decision* nil)

(defun default-authorization-audit-sink (event allowed-p)
  (if allowed-p
      (log:info "authorization ~a" (jsown:to-json event))
      (log:warn "authorization ~a" (jsown:to-json event))))

(defvar *authorization-audit-sink* #'default-authorization-audit-sink)

(defun non-empty-bounded-string-p (value &optional (maximum 256))
  (and (stringp value)
       (<= 1 (length value) maximum)
       (every (lambda (character)
                (and (graphic-char-p character)
                     (not (member character '(#\Newline #\Return #\Tab)))))
              value)))

(defun prefixed-scope-p (scope prefix)
  (and (> (length scope) (length prefix))
       (string= prefix scope :end2 (length prefix))))

(defun valid-restriction-scope-p (scope)
  (some (lambda (prefix)
          (and (prefixed-scope-p scope prefix)
               (non-empty-bounded-string-p
                (subseq scope (length prefix)))))
        +scope-prefixes+))

(defun validate-grant-scopes (scopes)
  (unless (and (listp scopes)
               (every #'non-empty-bounded-string-p scopes))
    (star.auth::signal-lifecycle-error
     "invalid_scopes"
     "Scopes must be a list of bounded non-empty strings"))
  (dolist (scope scopes)
    (unless (or (string= scope "admin")
                (member scope +capabilities+ :test #'string=)
                (valid-restriction-scope-p scope))
      (star.auth::signal-lifecycle-error
       "invalid_scope"
       (format nil "Unknown capability or resource scope: ~a" scope))))
  (remove-duplicates
   (mapcar #'copy-seq scopes)
   :test #'string=))

(defun install-closed-scope-validator ()
  (setf (symbol-function 'star.auth::normalize-scopes)
        #'validate-grant-scopes))

(eval-when (:load-toplevel :execute)
  (install-closed-scope-validator))

(defun candidate-principal (candidate)
  (or candidate
      (star.auth:current-request-principal)
      *trusted-authorization-context*))

(defun principal-id (principal)
  (typecase principal
    (star.auth:request-principal
     (star.auth:request-principal-id principal))
    (star.auth:service-call-context
     (star.auth:service-call-context-principal-id principal))
    (trusted-authorization-context
     (and (eq principal *trusted-authorization-context*)
          (trusted-authorization-context-id principal)))
    (t nil)))

(defun principal-type (principal)
  (typecase principal
    (star.auth:request-principal
     (star.auth:request-principal-type principal))
    (star.auth:service-call-context
     (star.auth:service-call-context-principal-type principal))
    (trusted-authorization-context
     (and (eq principal *trusted-authorization-context*)
          (trusted-authorization-context-principal-type principal)))
    (t nil)))

(defun principal-scopes (principal)
  (typecase principal
    (star.auth:request-principal
     (star.auth:request-principal-scopes principal))
    (star.auth:service-call-context
     (star.auth:service-call-context-scopes principal))
    (trusted-authorization-context
     (and (eq principal *trusted-authorization-context*)
          (mapcar #'copy-seq
                  (trusted-authorization-context-scopes principal))))
    (t nil)))

(defun administrator-scopes-p (scopes)
  (member "admin" scopes :test #'string=))

(defun capability-granted-p (scopes action)
  (and scopes
       (or (administrator-scopes-p scopes)
           (member action scopes :test #'string=))))

(defun scope-values (scopes prefix)
  (loop for scope in scopes
        when (prefixed-scope-p scope prefix)
          collect (subseq scope (length prefix))))

(defun dimension-granted-p (scopes prefix value)
  (if (null value)
      t
      (let ((values (scope-values scopes prefix)))
        (or (administrator-scopes-p scopes)
            (member "*" values :test #'string=)
            (member value values :test #'string=)))))

(defun resource-granted-p (scopes resource)
  (or (null resource)
      (and (dimension-granted-p
            scopes "tenant:" (authorization-resource-tenant-id resource))
           (dimension-granted-p
            scopes "dataset:" (authorization-resource-dataset-id resource))
           (dimension-granted-p
            scopes "actor:" (authorization-resource-actor-name resource))
           (dimension-granted-p
            scopes "target:" (authorization-resource-target-id resource))
           (dimension-granted-p
            scopes "target-namespace:"
            (authorization-resource-target-namespace resource))
           (dimension-granted-p
            scopes "program:" (authorization-resource-program-id resource)))))

(defun decision-reason (principal scopes action resource)
  (cond
    ((null principal) "missing_principal")
    ((null (principal-id principal)) "invalid_principal_context")
    ((not (member action +capabilities+ :test #'string=)) "unknown_action")
    ((not (capability-granted-p scopes action)) "missing_capability")
    ((not (resource-granted-p scopes resource)) "scope_mismatch")
    (t "matching_grant")))

(defmethod evaluate-authorization
    ((engine default-deny-policy-engine) request)
  (declare (ignore engine))
  (let* ((principal (candidate-principal
                     (authorization-request-principal request)))
         (scopes (principal-scopes principal))
         (action (authorization-request-action request))
         (resource (authorization-request-resource request))
         (reason (decision-reason principal scopes action resource))
         (allowed-p (string= reason "matching_grant")))
    (make-authorization-decision
     :id (cms-ulid:ulid)
     :allowed-p allowed-p
     :reason reason
     :action action
     :resource resource
     :principal-id (principal-id principal))))

(defun json-nullable (value)
  (or value :null))

(defun resource-json (resource)
  (if resource
      (jsown:new-js
        ("tenant_id" (json-nullable
                      (authorization-resource-tenant-id resource)))
        ("dataset_id" (json-nullable
                       (authorization-resource-dataset-id resource)))
        ("actor_name" (json-nullable
                       (authorization-resource-actor-name resource)))
        ("target_id" (json-nullable
                      (authorization-resource-target-id resource)))
        ("target_namespace" (json-nullable
                             (authorization-resource-target-namespace resource)))
        ("program_id" (json-nullable
                       (authorization-resource-program-id resource)))
        ("resource_id" (json-nullable
                        (authorization-resource-resource-id resource)))
        ("dtype" (json-nullable
                  (authorization-resource-dtype resource))))
      :null))

(defun authorization-audit-json (decision request)
  (let* ((principal (candidate-principal
                     (authorization-request-principal request)))
         (metadata (authorization-request-metadata request)))
    (jsown:new-js
      ("event_type" "authorization_decision")
      ("decision_id" (authorization-decision-id decision))
      ("decision" (if (authorization-decision-allowed-p decision)
                       "allow"
                       "deny"))
      ("reason" (authorization-decision-reason decision))
      ("principal_id" (json-nullable (principal-id principal)))
      ("principal_type" (json-nullable (principal-type principal)))
      ("action" (authorization-decision-action decision))
      ("resource" (resource-json
                    (authorization-decision-resource decision)))
      ("correlation_id"
       (json-nullable
        (or (and metadata (getf metadata :correlation-id))
            (and star.auth:*request-security-context*
                 (star.auth:request-security-context-correlation-id
                  star.auth:*request-security-context*)))))
      ("route" (json-nullable
                (and metadata (getf metadata :route))))
      ("method" (json-nullable
                 (and metadata (getf metadata :method)))))))

(defun emit-authorization-audit (decision request)
  (funcall *authorization-audit-sink*
           (authorization-audit-json decision request)
           (authorization-decision-allowed-p decision)))

(defun authorize (action &key resource principal metadata quotas
                           (engine *policy-engine*))
  (let* ((request
           (make-authorization-request
            :principal principal
            :action action
            :resource resource
            :metadata metadata
            :quotas quotas))
         (decision (evaluate-authorization engine request)))
    (emit-authorization-audit decision request)
    decision))

(defun authorize! (action &key resource principal metadata quotas
                            (engine *policy-engine*))
  (let ((decision
          (authorize action
                     :resource resource
                     :principal principal
                     :metadata metadata
                     :quotas quotas
                     :engine engine)))
    (unless (authorization-decision-allowed-p decision)
      (error 'authorization-error
             :code "access_denied"
             :decision decision))
    decision))

(defmacro with-trusted-authorization-context ((context) &body body)
  `(let ((*trusted-authorization-context* ,context))
     ,@body))

(defun document-value (document &rest names)
  (loop for name in names
        for value = (jsown:val-safe document name)
        when (and value (not (eq value :null)))
          return value))

(defun target-document-p (document)
  (let ((dtype (document-value document "dtype")))
    (and (stringp dtype)
         (string-equal dtype "target"))))

(defun resource-from-document (document &key actor-name)
  (let ((target-p (target-document-p document)))
    (make-authorization-resource
     :tenant-id (or (document-value document "tenant_id" "tenant")
                    "default")
     :dataset-id (document-value document "dataset")
     :actor-name (or actor-name
                     (and target-p
                          (document-value document "actor")))
     :target-id (and target-p
                     (document-value document "_id" "target_id"))
     :target-namespace
     (and target-p
          (document-value document "target_namespace" "namespace"))
     :program-id (and target-p
                      (document-value document "program_id" "program"))
     :resource-id (document-value document "_id")
     :dtype (document-value document "dtype"))))

(defun decision-rabbit-headers (decision)
  (let ((resource (authorization-decision-resource decision)))
    (list
     (cons "x-star-authorization-decision-id"
           (authorization-decision-id decision))
     (cons "x-star-authorization-action"
           (authorization-decision-action decision))
     (cons "x-star-authorization-tenant"
           (or (and resource
                    (authorization-resource-tenant-id resource))
               ""))
     (cons "x-star-authorization-dataset"
           (or (and resource
                    (authorization-resource-dataset-id resource))
               ""))
     (cons "x-star-authorization-actor"
           (or (and resource
                    (authorization-resource-actor-name resource))
               "")))))
