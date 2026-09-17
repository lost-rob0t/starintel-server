(in-package :star.databases.couchdb)

(defparameter +migration-design-document+ "migrations_prolog"
  "CouchDB design document containing the Prolog-backed migration views.")

(defparameter +migration-promote-view+ "promote_current"
  "Migration view whose value is the full proposed current-schema document.")

(defun migration-outcome (status id &key document code reason)
  (list :status status
        :id id
        :document document
        :code code
        :reason reason))

(defun migration-outcome-status (outcome)
  "Return the keyword status from a migration OUTCOME plist."
  (getf outcome :status))

(defun migration-outcome-id (outcome)
  "Return the document identifier from a migration OUTCOME plist."
  (getf outcome :id))

(defun migration-outcome-document (outcome)
  "Return the result document from a migration OUTCOME plist, when present."
  (getf outcome :document))

(defun migration-outcome-code (outcome)
  "Return the stable machine code from a migration OUTCOME plist."
  (getf outcome :code))

(defun migration-outcome-reason (outcome)
  "Return the human-readable reason from a migration OUTCOME plist."
  (getf outcome :reason))

(defun couchdb-migration-candidates
    (client database tenant dataset &key (limit 50) (update t))
  "Return full Prolog-projected migration candidates for TENANT and DATASET.

The view key is exactly `[tenant,dataset]`, so no unscoped corpus scan is
returned to the caller. LIMIT bounds one migration slice; migrated documents
fall out of the view on the next index update, making repeated apply calls a
simple bounded drain."
  (unless (and (stringp tenant) (plusp (length tenant)))
    (error "Migration tenant must be a non-empty string"))
  (unless (and (stringp dataset) (plusp (length dataset)))
    (error "Migration dataset must be a non-empty string"))
  (unless (and (integerp limit) (<= 1 limit 100))
    (error "Migration limit must be between 1 and 100"))
  (let* ((response
           (query-view
            client
            database
            +migration-design-document+
            +migration-promote-view+
            :key (list tenant dataset)
            :limit limit
            :include-docs nil
            :reduce nil
            :update update))
         (rows (or (jsown:val-safe response "rows") nil)))
    (loop for row in rows
          for candidate = (jsown:val-safe row "value")
          when candidate
            collect candidate)))

(defun migration-current-schema-p (document)
  (let ((schema (and document
                     (jsown:val-safe document "schema_version"))))
    (and (stringp schema)
         (string=
          schema
          (star.migrations:migration-current-schema-version)))))

(defun apply-migration-candidate
    (load-fn save-fn candidate
     &key authorize-fn (write-p t))
  "Re-read, authorize, validate, and optionally CAS-save one migration candidate.

LOAD-FN receives the candidate `_id`. AUTHORIZE-FN, when supplied, receives
the freshly loaded current document before validation or persistence. SAVE-FN
receives the normalized candidate and must implement optimistic persistence.
When WRITE-P is NIL the complete validation path runs but SAVE-FN is never
called."
  (let ((id (and candidate
                 (jsown:val-safe candidate "_id"))))
    (unless (and (stringp id) (plusp (length id)))
      (return-from apply-migration-candidate
        (migration-outcome
         :rejected
         (or id "")
         :code "document_id_required"
         :reason "Migration candidate requires a non-empty _id")))
    (let ((current (funcall load-fn id)))
      (unless current
        (return-from apply-migration-candidate
          (migration-outcome
           :missing id
           :code "document_missing"
           :reason "Document disappeared before migration commit")))
      (when authorize-fn
        (funcall authorize-fn current))
      (when (migration-current-schema-p current)
        (return-from apply-migration-candidate
          (migration-outcome
           :already-current
           id
           :document current
           :code "already_current"
           :reason "Document already uses the current schema")))
      (handler-case
          (let ((prepared
                  (star.migrations:prepare-migration-candidate
                   current candidate)))
            (if write-p
                (handler-case
                    (let ((saved (funcall save-fn prepared)))
                      (migration-outcome
                       :updated id :document saved))
                  (outbox-store-conflict ()
                    (migration-outcome
                     :stale id
                     :code "stale_revision"
                     :reason
                     "Document revision changed before migration commit")))
                (migration-outcome
                 :ready id :document prepared)))
        (star.migrations:migration-candidate-error (condition)
          (migration-outcome
           :rejected id
           :code
           (star.migrations:migration-candidate-error-code condition)
           :reason
           (star.migrations:migration-candidate-error-reason condition)))))))

(defun couchdb-apply-migration-candidate
    (client database candidate
     &key authorize-fn (write-p t))
  "Apply one Prolog migration candidate using the existing CouchDB CAS store.

The document is re-read immediately before AUTHORIZE-FN and commit. A stale
`_rev` or CouchDB conflict becomes a non-writing `:stale` outcome rather than a
blind overwrite."
  (apply-migration-candidate
   (lambda (id)
     (couchdb-load-outbox-document client database id))
   (lambda (prepared)
     (couchdb-save-outbox-document client database prepared))
   candidate
   :authorize-fn authorize-fn
   :write-p write-p))

(defun migration-outcome-json (outcome &key public-document-fn)
  "Convert a migration OUTCOME plist into a stable JSON response object."
  (let* ((document (migration-outcome-document outcome))
         (rendered
           (if (and document public-document-fn)
               (funcall public-document-fn document)
               document)))
    (jsown:new-js
      ("status"
       (string-downcase
        (symbol-name (migration-outcome-status outcome))))
      ("id" (or (migration-outcome-id outcome) ""))
      ("code" (or (migration-outcome-code outcome) :null))
      ("reason" (or (migration-outcome-reason outcome) :null))
      ("document" (or rendered :null)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export
   '(+migration-design-document+
     +migration-promote-view+
     couchdb-migration-candidates
     apply-migration-candidate
     couchdb-apply-migration-candidate
     migration-outcome-status
     migration-outcome-id
     migration-outcome-document
     migration-outcome-code
     migration-outcome-reason
     migration-outcome-json)
   :star.databases.couchdb))
