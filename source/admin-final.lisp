(in-package :starintel-gserver)

(defun admin/usage-handler (command)
  (clingon:print-usage-and-exit command t))

(defun admin/user-command ()
  (clingon:make-command
   :name "user"
   :description "Manage local human users without the HTTP listener"
   :handler #'admin/usage-handler
   :sub-commands
   (list
    (clingon:make-command
     :name "create"
     :description "Create a human user"
     :options (admin/user-create-options)
     :handler #'admin/user-create-handler)
    (clingon:make-command
     :name "list"
     :description "List human users"
     :handler #'admin/user-list-handler)
    (clingon:make-command
     :name "set-password"
     :description "Reset a human user's password"
     :options (admin/user-set-password-options)
     :handler #'admin/user-set-password-handler))))

(defun admin/credential-command ()
  (clingon:make-command
   :name "credential"
   :description "Manage API credentials without the HTTP listener"
   :handler #'admin/usage-handler
   :sub-commands
   (list
    (clingon:make-command
     :name "create"
     :description "Create an API credential"
     :options (admin/credential-create-options)
     :handler #'admin/credential-create-handler)
    (clingon:make-command
     :name "list"
     :description "List API credentials"
     :handler #'admin/credential-list-handler)
    (clingon:make-command
     :name "rotate"
     :description "Rotate an API credential"
     :options
     (list
      (admin/credential-id-option)
      (clingon:make-option
       :integer
       :description "Overlap window for the old credential"
       :long-name "overlap-seconds"
       :initial-value 0
       :key :admin-overlap-seconds))
     :handler #'admin/credential-rotate-handler)
    (clingon:make-command
     :name "revoke"
     :description "Revoke an API credential"
     :options (list (admin/credential-id-option))
     :handler
     (lambda (command)
       (admin/credential-status-handler command #'admin-revoke-credential*)))
    (clingon:make-command
     :name "disable"
     :description "Disable an API credential"
     :options (list (admin/credential-id-option))
     :handler
     (lambda (command)
       (admin/credential-status-handler command #'admin-disable-credential*))))))

(defun admin/geo-rebuild-handler (command)
  (safe-load-init (clingon:getopt command :init-value))
  (let ((batch-size (or (clingon:getopt command :geo-batch-size) 500))
        (max-documents (or (clingon:getopt command :geo-max-documents) 10000)))
    (let ((client
            (cl-couch:new-couchdb
             star:*couchdb-host*
             star:*couchdb-port*
             :scheme star:*couchdb-scheme*)))
      (cl-couch:password-auth
       client star:*couchdb-user* star:*couchdb-password*)
      (multiple-value-bind (processed projected)
          (star.databases.couchdb:couchdb-rebuild-geo-projections
           client
           star:*couchdb-default-database*
           :batch-size batch-size
           :max-documents max-documents)
        (admin/print-json
         (jsown:new-js
           ("status" "ok")
           ("database" star:*couchdb-default-database*)
           ("processed" processed)
           ("projected" projected)))))))

(defun admin/geo-command ()
  (clingon:make-command
   :name "geo"
   :description "Manage rebuildable CouchDB geographic projections"
   :handler #'admin/usage-handler
   :sub-commands
   (list
    (clingon:make-command
     :name "rebuild"
     :description "Rebuild explicit location/geometry/address projections"
     :options
     (list
      (clingon:make-option
       :integer
       :description "Projection candidate batch size (1..1000)"
       :long-name "batch-size"
       :initial-value 500
       :key :geo-batch-size)
      (clingon:make-option
       :integer
       :description "Maximum candidate documents to process (1..1000000)"
       :long-name "max-documents"
       :initial-value 10000
       :key :geo-max-documents))
     :handler #'admin/geo-rebuild-handler))))

(defun admin/command ()
  (clingon:make-command
   :name "admin"
   :description "Server-local bootstrap and recovery administration"
   :options (server/options)
   :handler #'admin/usage-handler
   :sub-commands (list (admin/user-command)
                       (admin/credential-command)
                       (admin/geo-command))))
