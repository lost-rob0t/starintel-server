(in-package :star.actors)

(defparameter +document-storage-dispatcher+ :pinned
  "Dispatcher for blocking CouchDB/object-storage lifecycle work.")

(defparameter +document-storage-timeout-seconds+ 60
  "Synchronous actor request budget for one storage lifecycle operation.")

(defstruct (document-storage-command
            (:constructor make-document-storage-command
                (&key operation document-id tier patch revision)))
  "Runtime-neutral storage command mirrored by spec/document-storage.star."
  operation
  document-id
  tier
  patch
  revision)

(defstruct (document-storage-result
            (:constructor make-document-storage-result
                (&key status operation document-id document lifecycle outcome
                      error-code error-message)))
  "Result envelope returned by the document-storage actor."
  status
  operation
  document-id
  document
  lifecycle
  outcome
  error-code
  error-message)

(defvar *document-storage-actor* nil
  "Pinned actor owning document lifecycle and external object-store I/O.")

(defun storage-command-id! (command)
  (let ((id (document-storage-command-document-id command)))
    (unless (and (stringp id) (plusp (length id)))
      (error "Document storage command requires a non-empty document id"))
    id))

(defun normalize-storage-command (message)
  (typecase message
    (document-storage-command message)
    (list
     (make-document-storage-command
      :operation (getf message :operation)
      :document-id (or (getf message :document-id) (getf message :id))
      :tier (getf message :tier)
      :patch (getf message :patch)
      :revision (or (getf message :revision) (getf message :rev))))
    (t
     (error "Unsupported document storage command: ~s" message))))

(defun complete-document-storage-request (result)
  (when *sender*
    (reply result *sender*))
  result)

(defun document-storage-error-result (command condition &key code (status :error))
  (make-document-storage-result
   :status status
   :operation (and command (document-storage-command-operation command))
   :document-id (and command (document-storage-command-document-id command))
   :error-code (or code "storage_operation_failed")
   :error-message (princ-to-string condition)))

(defun storage-result-for-document (command document &key outcome)
  (make-document-storage-result
   :status :success
   :operation (document-storage-command-operation command)
   :document-id (document-storage-command-document-id command)
   :document document
   :lifecycle (star.storage:document-storage-lifecycle-json document)
   :outcome outcome))

(defun document-storage-command-get (client command)
  (storage-result-for-document
   command
   (star.storage:load-document
    client
    star:*couchdb-default-database*
    (storage-command-id! command))))

(defun document-storage-command-lifecycle (client command)
  (let* ((id (storage-command-id! command))
         (document
           (star.storage:load-document
            client star:*couchdb-default-database* id)))
    (storage-result-for-document command document)))

(defun document-storage-command-transition (client command)
  (let ((tier (document-storage-command-tier command)))
    (unless tier
      (error "Document storage transition requires a tier"))
    (let* ((id (storage-command-id! command))
           (stored
             (star.storage:transition-document
              client
              star:*couchdb-default-database*
              id
              tier))
           (document
             (star.storage:load-document
              client
              star:*couchdb-default-database*
              id)))
      (declare (ignore stored))
      (storage-result-for-document command document))))

(defun document-storage-command-upsert (client command)
  (let ((patch (document-storage-command-patch command)))
    (unless patch
      (error "Document storage upsert requires a patch"))
    (let* ((id (storage-command-id! command))
           (outcome
             (star.storage:storage-aware-upsert-document
              client
              star:*couchdb-default-database*
              id
              patch))
           (document
             (star.databases.couchdb:document-update-outcome-document outcome)))
      (make-document-storage-result
       :status :success
       :operation :upsert
       :document-id id
       :document document
       :lifecycle
       (and document
            (star.storage:document-storage-lifecycle-json document))
       :outcome outcome))))

(defun document-storage-command-delete (client command)
  (let* ((id (storage-command-id! command))
         (revision
           (or (document-storage-command-revision command)
               (let ((raw
                       (cl-couch:get-document
                        client star:*couchdb-default-database* id)))
                 (jsown:val-safe
                  (star.documents:parse-document-object raw)
                  "_rev")))))
    (unless revision
      (error "Document ~a has no CouchDB revision" id))
    (star.storage:delete-document-with-storage
     client
     star:*couchdb-default-database*
     id
     revision)
    (make-document-storage-result
     :status :success
     :operation :delete
     :document-id id)))

(defun document-storage-command-apply-policy (client command)
  (let* ((id (storage-command-id! command))
         (stored
           (star.storage:apply-document-storage-policy
            client star:*couchdb-default-database* id))
         (document
           (star.storage:load-document
            client star:*couchdb-default-database* id)))
    (declare (ignore stored))
    (storage-result-for-document command document)))

(defun execute-document-storage-command (client command)
  (case (document-storage-command-operation command)
    (:get (document-storage-command-get client command))
    (:lifecycle (document-storage-command-lifecycle client command))
    (:transition (document-storage-command-transition client command))
    (:upsert (document-storage-command-upsert client command))
    (:delete (document-storage-command-delete client command))
    (:apply-policy (document-storage-command-apply-policy client command))
    (otherwise
     (error "Unknown document storage operation: ~s"
            (document-storage-command-operation command)))))

(defun document-storage-handler (message)
  "StarLang/Sento handler for document storage lifecycle commands."
  (let ((command nil))
    (complete-document-storage-request
     (handler-case
         (progn
           (setf command (normalize-storage-command message))
           (anypool:with-connection
               (client star.databases.couchdb:*couchdb-pool*)
             (execute-document-storage-command client command)))
       (dex:http-request-not-found (condition)
         (declare (ignore condition))
         (make-document-storage-result
          :status :not-found
          :operation (and command (document-storage-command-operation command))
          :document-id (and command
                            (document-storage-command-document-id command))
          :error-code "document_not_found"
          :error-message "Document not found"))
       (dex:http-request-conflict (condition)
         (document-storage-error-result
          command condition
          :status :conflict
          :code "document_conflict"))
       (star.storage:storage-backend-error (condition)
         (document-storage-error-result
          command condition :code "storage_backend_error"))
       (error (condition)
         (document-storage-error-result command condition))))))

(defun start-document-storage-actor (&optional (system *sys*))
  "Start the StarLang document-storage actor and register its star:// name."
  (unless system
    (error "Document storage actor requires a running actor system"))
  (star.storage:configure-storage-backends)
  (setf *document-storage-actor*
        (actor-of system
                  :name "document-storage"
                  :dispatcher +document-storage-dispatcher+
                  :receive #'document-storage-handler))
  (register-actor "document-storage" *document-storage-actor*)
  *document-storage-actor*)

(defun start-document-storage-actor-hook ()
  (start-document-storage-actor *sys*))

(defun call-document-storage (command
                              &key (timeout +document-storage-timeout-seconds+))
  "Execute COMMAND through the storage actor and return its result envelope."
  (unless *document-storage-actor*
    (error "Document storage actor is not running"))
  (sento.actor:ask-s *document-storage-actor* command :time-out timeout))

(nhooks:add-hook star:*actors-start-hook* #'start-document-storage-actor-hook)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(document-storage-command
            make-document-storage-command
            document-storage-command-operation
            document-storage-command-document-id
            document-storage-command-tier
            document-storage-command-patch
            document-storage-command-revision
            document-storage-result
            document-storage-result-status
            document-storage-result-operation
            document-storage-result-document-id
            document-storage-result-document
            document-storage-result-lifecycle
            document-storage-result-outcome
            document-storage-result-error-code
            document-storage-result-error-message
            document-storage-handler
            start-document-storage-actor
            call-document-storage
            *document-storage-actor*)
          :star.actors))
