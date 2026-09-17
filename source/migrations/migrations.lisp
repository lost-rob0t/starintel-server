(uiop:define-package :star.migrations
  (:nicknames :migrations)
  (:use :cl)
  (:export
   #:migration-candidate-error
   #:migration-candidate-error-code
   #:migration-candidate-error-reason
   #:migration-effective-dataset
   #:migration-effective-tenant
   #:migration-source-schema
   #:prepare-migration-candidate)
  (:documentation
   "Pure StarIntel document-migration invariant helpers with injected validation."))

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

(defun distinct-non-empty-strings (&rest values)
  (remove-duplicates
   (remove-if-not #'non-empty-string-p values)
   :test #'string=))

(defun migration-effective-tenant (document)
  "Return DOCUMENT's internal tenant, rejecting conflicting tenant fields."
  (let* ((tenant-id (document-value document "tenant_id"))
         (tenant (document-value document "tenant"))
         (values (distinct-non-empty-strings tenant-id tenant)))
    (when (> (length values) 1)
      (reject-candidate
       "conflicting_tenant"
       "Document tenant aliases conflict: ~s"
       values))
    (or (first values) "default")))

(defun migration-effective-dataset (document)
  "Return DOCUMENT's canonical or historical dataset, rejecting conflicts."
  (let* ((dataset (document-value document "dataset"))
         (source-dataset (document-value document "source_dataset"))
         (source-dataset-camel (document-value document "sourceDataset"))
         (values
           (distinct-non-empty-strings
            dataset source-dataset source-dataset-camel)))
    (when (> (length values) 1)
      (reject-candidate
       "conflicting_dataset"
       "Document dataset aliases conflict: ~s"
       values))
    (first values)))

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

(defun historical-date-added (document)
  (or (let ((value (document-value document "date_added")))
        (and (non-empty-string-p value) value))
      (let ((value (document-value document "dateAdded")))
        (and (non-empty-string-p value) value))))

(defun require-migrated-dataset (current candidate)
  (let ((before (migration-effective-dataset current))
        (after (migration-effective-dataset candidate)))
    (unless (and (non-empty-string-p before)
                 (non-empty-string-p after)
                 (string= before after))
      (reject-candidate
       "dataset_changed"
       "Migration must preserve effective dataset (current=~s candidate=~s)"
       before after))))

(defun require-migrated-date-added (current candidate)
  (let ((before (historical-date-added current))
        (after (document-value candidate "date_added")))
    (unless (non-empty-string-p after)
      (reject-candidate
       "date_added_required"
       "Migrated document must contain canonical date_added"))
    (when (and before (not (string= before after)))
      (reject-candidate
       "date_added_changed"
       "Migration must preserve existing date_added (current=~s candidate=~s)"
       before after))))

(defun validate-migration-invariants
    (current candidate target-schema)
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
  (require-migrated-dataset current candidate)
  (require-same-string-field
   current candidate "_rev" "stale_revision")
  (require-migrated-date-added current candidate)
  (let ((current-tenant (migration-effective-tenant current))
        (candidate-tenant (migration-effective-tenant candidate)))
    (unless (string= current-tenant candidate-tenant)
      (reject-candidate
       "tenant_changed"
       "Migration must preserve tenant (current=~s candidate=~s)"
       current-tenant candidate-tenant)))
  (let ((target (document-value candidate "schema_version")))
    (unless (and (non-empty-string-p target-schema)
                 (stringp target)
                 (string= target target-schema))
      (reject-candidate
       "unsupported_target_schema"
       "Migration candidate targets ~s, but the caller accepts ~s"
       target target-schema)))
  t)

(defun normalize-with-validator (candidate validator)
  (handler-case
      (funcall validator (copy-json-object candidate))
    (migration-candidate-error (condition)
      (error condition))
    (error (condition)
      (reject-candidate
       "invalid_target_schema"
       "Migrated document failed strict validation: ~a"
       condition))))

(defun prepare-migration-candidate
    (current candidate target-schema validator)
  "Validate migration invariants and return VALIDATOR's normalized candidate.

CURRENT is the document re-read immediately before commit. CANDIDATE is the
Prolog view output. TARGET-SCHEMA comes from the caller's live schema authority;
this library intentionally owns no schema-version constant. VALIDATOR is called
on a defensive copy and must reject invalid target documents.

`_id`, `_rev`, effective dataset, tenant, and any existing date_added value are
preserved. Historical dataset/date aliases may be canonicalized and a missing
date_added may be filled deterministically by the migration projection. The
normalized result is checked again so the validator cannot break CAS/resource
invariants."
  (validate-migration-invariants current candidate target-schema)
  (let ((normalized
          (normalize-with-validator candidate validator)))
    (validate-migration-invariants
     current normalized target-schema)
    normalized))
