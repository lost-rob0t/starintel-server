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

(defun admin/command ()
  (clingon:make-command
   :name "admin"
   :description "Server-local bootstrap and recovery administration"
   :options (server/options)
   :handler #'admin/usage-handler
   :sub-commands (list (admin/user-command)
                       (admin/credential-command))))
