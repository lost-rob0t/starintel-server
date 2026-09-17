(in-package #:star.request-audit)

;; Only trusted server code binds these. Never accept them from request headers.
(defvar *input-text* :absent)
(defvar *subject-key* nil)
(defvar *request-tenant* nil)
(defvar *request-dataset* nil)

(defclass audit-policy-engine (star.authorization:policy-engine)
  ((delegate :initarg :delegate :reader delegate)
   (writers :initarg :writers :reader writers)
   (key :initarg :key :reader audit-key)
   (key-id :initarg :key-id :reader key-id)
   (version :initarg :version :reader policy-version)
   (classifier :initarg :classifier :reader classifier)
   (policy :initarg :policy :reader policy)
   (enforce :initarg :enforce :reader enforce-p)
   (assessment-error :initarg :assessment-error :reader assessment-error-policy)
   (audit-error :initarg :audit-error :reader audit-error-policy)
   (failure :initarg :failure :reader on-failure)))

(defun pseudonym (engine tenant domain value)
  "Domain/tenant separated keyed references; no reversible raw identifiers."
  (if (null value) "none"
      (let* ((parts (list tenant domain value))
             (input (format nil "~{~d:~a~}" (loop for part in parts append (list (length part) part))))
             (mac (ironclad:make-hmac (audit-key engine) :sha256)))
        (ironclad:update-hmac mac (babel:string-to-octets input :encoding :utf-8))
        (concatenate 'string "hmac-" (ironclad:byte-array-to-hex-string (ironclad:hmac-digest mac))))))

(defun bounded-reference (value)
  (and (stringp value) (<= 1 (length value) 256) value))

(defun refusal-decision (base reason)
  (star.authorization::make-authorization-decision
   :id (star.authorization:authorization-decision-id base)
   :allowed-p nil :reason reason
   :action (star.authorization:authorization-decision-action base)
   :resource (star.authorization:authorization-decision-resource base)
   :principal-id (star.authorization:authorization-decision-principal-id base)))

(defun request-event (engine request base result tenant proposed)
  (let* ((id (star.authorization:authorization-decision-id base))
         (resource (star.authorization:authorization-request-resource request))
         (metadata (star.authorization:authorization-request-metadata request))
         (subject (or (bounded-reference *subject-key*)
                      (and resource (bounded-reference
                                     (star.authorization:authorization-resource-resource-id resource)))))
         (identity (format nil "~a~%request-audit~%~a~%~a" tenant id (policy-version engine)))
         (digest (ironclad:byte-array-to-hex-string
                  (ironclad:digest-sequence :sha256 (babel:string-to-octets identity :encoding :utf-8)))))
    (jsown:new-js
      ("_id" (concatenate 'string "audit-" digest))
      ("schema" "starintel.request-audit.v1")
      ("event_id" id) ("occurred_at" (star.audit::decision-time id))
      ("tenant_id" tenant) ("source" "starintel-server")
      ("action" (star.authorization:authorization-request-action request))
      ("principal_ref" (pseudonym engine tenant "principal"
                         (bounded-reference (star.authorization:authorization-decision-principal-id base))))
      ("subject_ref" (pseudonym engine tenant "subject" subject))
      ("correlation_ref" (pseudonym engine tenant "correlation"
                           (bounded-reference (getf metadata :correlation-id))))
      ("key_id" (key-id engine)) ("policy_version" (policy-version engine))
      ("local_signals" (assessment-local-signals result))
      ("model_signals" (assessment-model-signals result))
      ("flags" (assessment-flags result)) ("rule_ids" (assessment-rule-ids result))
      ("assessment_status" (assessment-status result))
      ("mode" (if (enforce-p engine) "enforce" "audit"))
      ("decision" (cond ((and proposed (enforce-p engine)) "refuse")
                          (proposed "would-refuse") (t "allow"))))))

(defun report-failure (engine code)
  (handler-case (funcall (on-failure engine) code) (error () nil)))

(defun search-scope-ready-p (request)
  "Mirror the existing search service's pure preconditions before classifier I/O.
Do not replace its decision, resource, quota accounting, or eventual error."
  (let* ((principal (star.authorization::candidate-principal
                     (star.authorization:authorization-request-principal request)))
         (scopes (star.authorization::principal-scopes principal))
         (admin (star.authorization::administrator-scopes-p scopes)))
    (and (or admin
             (member "*" (star.authorization:scope-values scopes "dataset:") :test #'string=)
             (star.authorization::restricted-values scopes "dataset:" *request-dataset*))
         (or admin
             (member "*" (star.authorization:scope-values scopes "tenant:") :test #'string=)
             (star.authorization::restricted-values scopes "tenant:" *request-tenant*)))))

(defmethod star.authorization:evaluate-authorization ((engine audit-policy-engine) request)
  "The delegate remains the authorization authority; this layer can only restrict."
  (let ((base (star.authorization:evaluate-authorization (delegate engine) request)))
    (unless (and (star.authorization:authorization-decision-allowed-p base)
                 (not (eq *input-text* :absent))
                 (or (not (equal "search:read" (star.authorization:authorization-request-action request)))
                     (search-scope-ready-p request)))
      (return-from star.authorization:evaluate-authorization base))
    (let* ((resource (star.authorization:authorization-request-resource request))
           (tenant (or (and resource (star.authorization:authorization-resource-tenant-id resource))
                       *request-tenant*))
           (result (if (not (stringp tenant))
                       (unavailable-assessment "unavailable")
                       (handler-case
                           (analyze-request *input-text* :classifier (classifier engine) :policy (policy engine))
                         (error () (unavailable-assessment "policy-error")))))
           (failed (member (assessment-status result)
                           '("unavailable" "input-too-large" "policy-error") :test #'string=))
           (proposed (or (assessment-refuse-p result)
                         (and failed (eq (assessment-error-policy engine) :deny))))
           (writer (and (stringp tenant) (cdr (assoc tenant (writers engine) :test #'string=))))
           (audit-failed nil))
      (handler-case
          (progn
            (unless writer (error "No tenant audit writer"))
            (funcall writer (request-event engine request base result tenant proposed)))
        (error () (setf audit-failed t) (report-failure engine :audit-unavailable)))
      (when failed (report-failure engine :assessment-unavailable))
      (cond
        ((and (enforce-p engine) proposed) (refusal-decision base "request_refused"))
        ((and (enforce-p engine) audit-failed (eq (audit-error-policy engine) :deny))
         (refusal-decision base "request_audit_unavailable"))
        (t base)))))

(defun document-intent (document &optional dtype-override)
  "Only operational intent fields, never document text, posts, targets or credentials.
Absent intent is not a clean assessment. The normal authorization audit still runs."
  (let ((dtype (or (star.audit::value document "dtype") dtype-override)))
    (unless (member dtype '("target" "operation" "research-node") :test #'equal)
      (return-from document-intent :absent))
    (let* ((data (star.audit::value document "data"))
           (parts (loop for field in '("objective" "mission" "query" "prompt" "instructions")
                        for value = (star.audit::value data field)
                        when (stringp value) collect value)))
      (cond ((null parts) :absent)
            ((> (reduce #'+ parts :key #'length) (- +maximum-input+ 5))
             (make-string (1+ +maximum-input+) :initial-element #\Space))
            (t (format nil "~{~a~^ ~}" parts))))))

(defun service-wrappers ()
  "Return function-cell adapters; installed explicitly before serving requests."
  (list
   (cons 'star.authorization:authorize-document!
         (lambda (original)
           (lambda (action document &rest options)
             (let ((*input-text* (if (eq *input-text* :absent) (document-intent document) *input-text*)))
               (apply original action document options)))))
   (cons 'star.authorization:authorized-update-document
         (lambda (original)
           (lambda (id patch fetch-fn update-fn &rest options)
             (let ((*input-text* nil))
               (flet ((fetch-with-intent (document-id)
                        (let* ((raw (funcall fetch-fn document-id))
                               (old (if (stringp raw) (jsown:parse raw) raw))
                               (intent (document-intent patch (star.audit::value old "dtype"))))
                          ;; Inspect the incoming patch even when dtype is only on the old document.
                          ;; NIL suppresses falling back to old content that is not being submitted.
                          (setf *input-text* (if (eq intent :absent) nil intent))
                          raw)))
                 (apply original id patch #'fetch-with-intent update-fn options))))))
   (cons 'star.authorization:authorized-search-query
         (lambda (original)
           (lambda (query &rest options)
             (let ((*input-text* query) (*request-tenant* (getf options :requested-tenant "default"))
                   (*request-dataset* (getf options :requested-dataset)))
               (apply original query options)))))
   (cons 'star.authorization:authorize!
         (lambda (original)
           (lambda (&rest arguments)
             (handler-case (apply original arguments)
               (star.authorization:authorization-error (condition)
                 (let* ((decision (star.authorization:authorization-error-decision condition))
                        (reason (star.authorization:authorization-decision-reason decision)))
                   (if (member reason '("request_refused" "request_audit_unavailable") :test #'equal)
                       (error 'star.authorization:authorization-error :code reason :decision decision)
                       (error condition))))))))))

(defun install (&key writers hmac-key (key-id "k1") (version "local-v1") classifier
                     (policy #'default-policy) (refusals-enabled nil)
                     (on-assessment-error :deny) (on-audit-error :allow)
                     (on-failure (lambda (code) (warn "Request audit degraded: ~a" code))))
  "Install audit plus optional refusal; return an ownership-safe uninstall closure.
Invoke from trusted init before workers/HTTP start, not concurrently with requests.
No default classifier, database creation, worker or outbound call on load/install."
  (unless (and (member refusals-enabled '(nil t))
               (member on-assessment-error '(:allow :deny))
               (member on-audit-error '(:allow :deny))
               (typep hmac-key '(simple-array (unsigned-byte 8) (*)))
               (<= 32 (length hmac-key) 64)
               (safe-rule-id-p key-id) (safe-rule-id-p version)
               (functionp policy) (functionp on-failure)
               (or (null classifier) (functionp classifier))
               (bounded-list-p writers 256) writers
               (every (lambda (pair) (and (consp pair) (star.audit::safe-id (car pair) nil)
                                          (functionp (cdr pair)))) writers)
               (= (length writers) (length (remove-duplicates writers :key #'car :test #'string=))))
    (error "Invalid request-audit configuration"))
  (when (typep star.authorization:*policy-engine* 'audit-policy-engine)
    (error "Request audit already installed; uninstall before replacing configuration"))
  (let* ((previous star.authorization:*policy-engine*)
         (engine (make-instance 'audit-policy-engine :delegate previous :writers (copy-tree writers)
                  :key (copy-seq hmac-key) :key-id key-id :version version :classifier classifier
                  :policy policy :enforce refusals-enabled :assessment-error on-assessment-error
                  :audit-error on-audit-error :failure on-failure))
         (replacements (loop for (name . builder) in (service-wrappers)
                             for old = (symbol-function name)
                             collect (list name old (funcall builder old)))))
    (dolist (entry replacements) (setf (symbol-function (first entry)) (third entry)))
    (setf star.authorization:*policy-engine* engine)
    (lambda ()
      (when (eq star.authorization:*policy-engine* engine)
        (setf star.authorization:*policy-engine* previous))
      (dolist (entry replacements)
        (when (eq (symbol-function (first entry)) (third entry))
          (setf (symbol-function (first entry)) (second entry)))))))
