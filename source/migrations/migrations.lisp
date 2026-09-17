(uiop:define-package :star.migrations
  (:nicknames :migrations)
  (:use :cl)
  (:export
   #:migration-candidate-error
   #:migration-candidate-error-code
   #:migration-candidate-error-reason
   #:migration-current-schema-version
   #:migration-effective-tenant
   #:migration-source-schema
   #:prepare-migration-candidate)
  (:documentation
   "Pure StarIntel document-migration validation and invariant helpers."))

(in-package :star.migrations)

(define-condition migration-candidate-error (error)
  ((code
    :initarg :code
    :reader migration-candidate-error-code)
   (reason
    :initarg :reason
    :reader migration-candidate-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "~a"
             (migration-candidate-error-reason condition))))
  (:documentation
   "Raised when a proposed schema-migration document cannot be committed safely."))

(setf (documentation 'migration-candidate-error-code 'function)
      "Stable machine-readable rejection code for a migration candidate.")
(setf (documentation 'migration-candidate-error-reason 'function)
      "Human-readable rejection reason for a migration candidate.")

(defun migration-current-schema-version ()
  "Return the immutable StarIntel document schema accepted by this runtime."
  starintel:+starintel-doc-version+)

(defun copy-json-object (document)
  (jsown:with-injective-reader
    (jsown:parse (jsown:to-json document))))

(defun document-value (document key)
  (and document
       (jsown:keyp document key)
       (jsown:val document key)))

(defun non-empty-string-p (value)
  (and (stringp value)
       (plusp (length value))))

(defun reject-candidate (code control &rest arguments)
  (error 'migration-candidate-error
         :code code
         :reason (apply #'format nil control arguments)))

(defun migration-effective-tenant (document)
  "Return DOCUMENT's internal tenant, rejecting conflicting tenant fields."
  (let ((tenant-id (document-value document "tenant_id"))
        (tenant (document-value document "tenant")))
    (when (and (non-empty-string-p tenant-id)
               (non-empty-string-p tenant)
               (not (string= tenant-id tenant)))
      (reject-candidate
       "conflicting_tenant"
       "Document tenant_id ~s conflicts with tenant ~s"
       tenant-id tenant))
    (or (and (non-empty-string-p tenant-id) tenant-id)
        (and (non-empty-string-p tenant) tenant)
        "default")))

(defun migration-source-schema (document)
  "Return the source schema recorded in a migrated candidate's lineage."
  (let ((lineage (document-value document "lineage")))
    (and lineage
         (jsown:keyp lineage "migration_from")
         (jsown:val lineage "migration_from"))))

(defun require-same-string-field (current candidate key code)
  (let ((before (document-value current key))
        (after (document-value candidate key)))
    (unless (and (non-empty-string-p before)
                 (non-empty-string-p after)
                 (string= before after))
      (reject-candidate
       code
       "Migration must preserve ~a (current=~s candidate=~s)"
       key before after))))

(defun require-same-optional-string-field (current candidate key code)
  (let ((before (document-value current key))
        (after (document-value candidate key)))
    (unless (equal before after)
      (reject-candidate
       code
       "Migration must preserve ~a (current=~s candidate=~s)"
       key before after))))

(defun validate-migration-invariants (current candidate)
  (unless (and (consp current) (eq (car current) :obj))
    (reject-candidate
     "current_document_required"
     "Current persisted document is not a JSON object"))
  (unless (and (consp candidate) (eq (car candidate) :obj))
    (reject-candidate
     "candidate_document_required"
     "Migration candidate is not a JSON object"))
  (require-same-string-field
   current candidate "_id" "document_id_changed")
  (require-same-string-field
   current candidate "dataset" "dataset_changed")
  (require-same-string-field
   current candidate "_rev" "stale_revision")
  (require-same-optional-string-field
   current candidate "date_added" "date_added_changed")
  (let ((current-tenant (migration-effective-tenant current))
        (candidate-tenant (migration-effective-tenant candidate)))
    (unless (string= current-tenant candidate-tenant)
      (reject-candidate
       "tenant_changed"
       "Migration must preserve tenant (current=~s candidate=~s)"
       current-tenant candidate-tenant)))
  (let ((target (document-value candidate "schema_version"))
        (expected (migration-current-schema-version)))
    (unless (and (stringp target)
                 (string= target expected))
      (reject-candidate
       "unsupported_target_schema"
       "Migration candidate targets ~s, but this runtime accepts ~s"
       target expected)))
  t)

(defun strip-internal-tenancy (document)
  (let ((tenant-id (document-value document "tenant_id"))
        (tenant (document-value document "tenant")))
    (when (jsown:keyp document "tenant_id")
      (jsown:remkey document "tenant_id"))
    (when (jsown:keyp document "tenant")
      (jsown:remkey document "tenant"))
    (values document tenant-id tenant)))

(defun restore-internal-tenancy (document tenant-id tenant)
  (when tenant-id
    (setf (jsown:val document "tenant_id") tenant-id))
  (when tenant
    (setf (jsown:val document "tenant") tenant))
  document)

(defun strict-normalize-candidate (candidate)
  (let ((copy (copy-json-object candidate)))
    (multiple-value-bind (public tenant-id tenant)
        (strip-internal-tenancy copy)
      (handler-case
          (progn
            (star.documents:validate-v09-document public)
            (let ((ensured (star.documents:ensure-document public)))
              (restore-internal-tenancy ensured tenant-id tenant)))
        (star.documents:document-schema-validation-error (condition)
          (reject-candidate
           "invalid_target_schema"
           "Migrated document failed strict v0.9 validation: ~a"
           condition))
        (error (condition)
          (reject-candidate
           "invalid_target_document"
           "Migrated document normalization failed: ~a"
           condition))))))

(defun prepare-migration-candidate (current candidate)
  "Validate migration invariants and return a strict normalized commit candidate.

CURRENT must be the document re-read immediately before commit. CANDIDATE is
the document emitted by the Prolog migration view. `_id`, `_rev`, dataset,
tenant, and date_added are immutable across this operation; schema_version may
change only to the runtime's current immutable schema. The returned document
has passed the same strict v0.9 validator used by normal StarIntel ingest."
  (validate-migration-invariants current candidate)
  (let ((normalized (strict-normalize-candidate candidate)))
    (validate-migration-invariants current normalized)
    normalized))
