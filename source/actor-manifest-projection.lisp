(in-package :star.actors)

(defparameter *actor-manifest-authority* "document-manifests"
  "Stable server-owned catalog namespace for persisted actor declarations.")

(defun actor-registry-entry-from-document (document)
  "Adapt a canonical system manifest through the sole registry validator.

Only public descriptor fields are projected. Transport endpoints, runtime
paths and arbitrary extensions never enter the catalog or imply readiness."
  (unless (and (manifest-object-p document)
               (equal (jsown:val-safe document "dtype") "actor-manifest")
               (equal (jsown:val-safe document "dataset") "starintel-system")
               (jsown:keyp document "schemaVersion")
               (every (lambda (key)
                        (member (jsown:val-safe document key) '(nil "public") :test #'equal))
                      '("tenant_id" "tenant"))
               (member (jsown:val-safe document "visibility") '(nil "public") :test #'equal)
               (not (eq (jsown:val-safe document "deleted") :true)))
    (invalid-actor-manifest "expected a canonical system actor-manifest"))
  (unless (equal "public"
                 (star.authorization:authorization-resource-tenant-id
                  (star.authorization:resource-from-document document)))
    (invalid-actor-manifest "actor-manifest effective tenant must be public"))
  (when (and (jsown:keyp document "_id")
             (not (equal (jsown:val-safe document "id") (jsown:val document "_id"))))
    (invalid-actor-manifest "canonical manifest id disagrees with stored identity"))
  (star.documents:validate-stored-document document)
  (let* ((actor (required-manifest-string document "actor" "document"))
         (version (required-manifest-string document "actorVersion" "document"))
         (options (jsown:val-safe document "targetOptions"))
         (manifest
           (jsown:new-js
             ("resourceUri" (format nil "star://~a/actor/~a" *actor-manifest-authority* actor))
             ("resourceKind" "actor")
             ("semantic" (jsown:new-js ("name" actor) ("version" version) ("digest" "pending")))
             ("accepts" (jsown:new-js ("targets" nil) ("documents" nil) ("messages" nil)))
             ("produces" (jsown:new-js ("targets" nil) ("documents" nil) ("messages" nil)))
             ("capabilities" (validate-string-list (jsown:val-safe document "capabilities") "capabilities"))
             ("operatorVisible" :true)
             ("provenance" (jsown:new-js ("sourcePackage" "actor-manifest-documents"))))))
    (unless (and (every (lambda (char) (or (find char "abcdefghijklmnopqrstuvwxyz0123456789._:-"))) actor)
                 (find (char actor 0) "abcdefghijklmnopqrstuvwxyz0123456789"))
      (invalid-actor-manifest "actor must be a bounded wire actor name"))
    (dolist (side '("accepts" "produces"))
      (let ((types (validate-string-list (jsown:val-safe document side) side))
            (contract (jsown:val manifest side)))
        (setf (jsown:val contract "targets") (intersection types '("target") :test #'string=)
              (jsown:val contract "documents") (remove "target" types :test #'string=))))
    (when options
      (unless (manifest-object-p options)
        (invalid-actor-manifest "targetOptions must be an object"))
      (setf options (copy-manifest-json options))
      (setf (jsown:val manifest "options")
            (loop for key in (sort (copy-list (jsown:keywords options)) #'string<)
                  for hint = (jsown:val options key)
                  collect
                  (progn
                    (validate-object-keys hint +actor-option-keys+ '("description" "required") "targetOptions")
                    (when (and (jsown:keyp hint "key") (not (equal key (jsown:val hint "key"))))
                      (invalid-actor-manifest "targetOptions key disagrees with its hint"))
                    (setf (jsown:val hint "key") key)
                    hint))))
    (setf (jsown:val (jsown:val manifest "semantic") "digest")
          (concatenate 'string "sha256:"
                       (ironclad:byte-array-to-hex-string
                        (ironclad:digest-sequence :sha256
                         (babel:string-to-octets (jsown:to-json manifest) :encoding :utf-8)))))
    (actor-registry-entry-from-manifest manifest)))

(defun eligible-actor-manifest-entry (document)
  "Validate one persisted declaration, omitting invalid or non-public input."
  (handler-case (actor-registry-entry-from-document document)
    (invalid-actor-manifest () nil)
    (star.documents:document-schema-validation-error () nil)))

(defun query-actor-manifest-documents
    (&key client database (query-fn #'star.databases.couchdb:query-view)
          (max-pages 128))
  "Read eligible declarations in bounded pages from the maintained dtype view.

The 128-entry capacity counts distinct eligible resources, excluding trusted
local claims. An independent page budget fails explicitly rather than returning
an incomplete catalog. The unprocessed lookahead row is the inclusive cursor,
so deleting a cursor between requests cannot cause skip=1 to lose another row."
  (unless (and (integerp max-pages) (<= 1 max-pages 128))
    (error "Actor manifest scan page budget must be between 1 and 128"))
  (let ((seen (make-hash-table :test #'equal)) (documents nil) (count 0) (cursor nil))
    (bt:with-lock-held (*actor-registry-lock*)
      (maphash (lambda (uri entry) (declare (ignore entry)) (setf (gethash uri seen) t))
               *actor-registry*))
    (loop for page from 1 to max-pages do
      (let* ((response (funcall query-fn client database "data" "count_by_dtype"
                                :start-key "actor-manifest" :end-key "actor-manifest"
                                :start-key-doc-id cursor
                                :include-docs t :reduce nil :limit 129))
             (rows (coerce (jsown:val response "rows") 'list))
             (lookahead (nth 128 rows)))
        (when (> (length rows) 129)
          (error "Actor manifest query exceeded the requested page size"))
        (dolist (row (subseq rows 0 (min 128 (length rows))))
          (let* ((document (jsown:val-safe row "doc"))
                 (entry (eligible-actor-manifest-entry document))
                 (uri (and entry (actor-registry-entry-resource-uri entry))))
            (when (and uri (not (gethash uri seen)))
              (setf (gethash uri seen) t)
              (when (> (incf count) 128)
                (error "Persisted actor catalog exceeds 128 eligible manifests"))
              (push document documents))))
        (unless lookahead (return-from query-actor-manifest-documents (nreverse documents)))
        (let ((next (jsown:val lookahead "id")))
          (unless (and (stringp next) (plusp (length next))
                       (or (null cursor) (string< cursor next)))
            (error "Actor manifest query cursor did not advance"))
          (setf cursor next))))
    (error "Persisted actor catalog scan budget exhausted after ~d pages" max-pages)))
