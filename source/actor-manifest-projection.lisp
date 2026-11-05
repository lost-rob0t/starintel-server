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
               (member (jsown:val-safe document "visibility") '(nil "public") :test #'equal)
               (not (eq (jsown:val-safe document "deleted") :true)))
    (invalid-actor-manifest "expected a canonical system actor-manifest"))
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

(defun query-actor-manifest-documents
    (&key client database (query-fn #'star.databases.couchdb:query-view))
  "Read a bounded persisted catalog from the maintained dtype view.

Fail explicitly on overflow rather than silently advertise a partial fleet."
  (let* ((response (funcall query-fn client database "data" "count_by_dtype"
                            :key "actor-manifest" :include-docs t :reduce nil :limit 129))
         (rows (jsown:val response "rows")))
    (when (> (length rows) 128)
      (error "Persisted actor catalog exceeds 128 manifests"))
    (mapcar (lambda (row) (jsown:val row "doc")) rows)))
