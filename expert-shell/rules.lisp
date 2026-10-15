(in-package :star.expert.shell)

(defun plan-request (request operation risk rule-name reason)
  (let ((plan (make-instance 'shell-plan
                             :operation operation
                             :risk risk
                             :rule-name rule-name
                             :reason reason
                             :args (copy-tree (shell-request-args request))
                             :confirmed-p (shell-request-confirmed-p request))))
    (setf (shell-session-last-plan *current-session*) plan)
    (trace-event :planned
                 :rule rule-name
                 :operation operation
                 :risk risk
                 :reason reason)
    (lisa:assert-instance plan)
    (lisa:retract request)
    plan))

(defun block-plan (plan)
  (trace-event :blocked
               :operation (shell-plan-operation plan)
               :risk (shell-plan-risk plan)
               :reason :confirmation-required)
  (finish-result
   (make-result
    :success-p nil
    :operation (shell-plan-operation plan)
    :code :confirmation-required
    :message "This operation mutates StarIntel state. Re-run it with --yes to confirm."))
  (lisa:retract plan)
  nil)

(defun unsupported-request (request)
  (trace-event :unsupported
               :verb (shell-request-verb request)
               :resource (shell-request-resource request)
               :qualifier (shell-request-qualifier request))
  (finish-result
   (make-result
    :success-p nil
    :code :unsupported-command
    :message (format nil "No expert rule matched: ~a"
                     (or (shell-request-raw request)
                         (list (shell-request-verb request)
                               (shell-request-resource request)
                               (shell-request-qualifier request))))))
  (lisa:retract request)
  nil)

(defun client-condition-code (condition)
  (cond
    ((typep condition 'star.api.client:client-authentication-error)
     :authentication-error)
    ((typep condition 'star.api.client:client-authorization-error)
     :authorization-error)
    ((typep condition 'star.api.client:client-not-found-error)
     :not-found)
    ((typep condition 'star.api.client:client-conflict-error)
     :conflict)
    ((typep condition 'star.api.client:client-validation-error)
     :validation-error)
    ((typep condition 'star.api.client:client-rate-limit-error)
     :rate-limited)
    ((typep condition 'star.api.client:client-server-unavailable-error)
     :server-unavailable)
    ((typep condition 'star.api.client:client-timeout-error)
     :timeout)
    ((typep condition 'star.api.client:client-connection-error)
     :connection-error)
    ((typep condition 'star.api.client:client-protocol-error)
     :protocol-error)
    ((typep condition 'star.api.client:star-client-error)
     :client-error)
    (t :operation-failed)))

(defun client-condition-details (condition)
  (when (typep condition 'star.api.client:client-http-error)
    (list :status (star.api.client:client-http-error-status condition)
          :server-code (star.api.client:client-http-error-code condition)
          :correlation-id (star.api.client:client-http-error-correlation-id condition)
          :operation-id (star.api.client:client-http-error-operation-id condition))))

(defun execute-planned-operation (plan)
  (unwind-protect
       (handler-case
           (progn
             (trace-event :execute
                          :operation (shell-plan-operation plan)
                          :rule (shell-plan-rule-name plan))
             (finish-result
              (make-result
               :success-p t
               :operation (shell-plan-operation plan)
               :code :ok
               :value (perform-operation *current-session* plan))))
         (error (condition)
           (let ((code (client-condition-code condition)))
             (trace-event :error
                          :operation (shell-plan-operation plan)
                          :code code
                          :condition (princ-to-string condition))
             (finish-result
              (make-result
               :success-p nil
               :operation (shell-plan-operation plan)
               :code code
               :value (client-condition-details condition)
               :message (princ-to-string condition))))))
    (ignore-errors (lisa:retract plan))))

(defparameter *shell-rule-forms*
  '((lisa:defrule plan-health (:salience 30)
      (?request (shell-request (verb :health) (resource :server)))
      =>
      (plan-request ?request :health :read 'plan-health
                    "Probe the StarIntel server health endpoint."))

    (lisa:defrule plan-server-info (:salience 30)
      (?request (shell-request (verb :info) (resource :server)))
      =>
      (plan-request ?request :server-info :read 'plan-server-info
                    "Read StarIntel server metadata and protocol information."))

    (lisa:defrule plan-auth-context (:salience 30)
      (?request (shell-request (verb :context) (resource :auth)))
      =>
      (plan-request ?request :auth-context :read 'plan-auth-context
                    "Read the authenticated StarIntel principal and authorization context."))

    (lisa:defrule plan-openapi (:salience 30)
      (?request (shell-request (verb :openapi) (resource :server)))
      =>
      (plan-request ?request :openapi :read 'plan-openapi
                    "Fetch the server OpenAPI contract."))

    (lisa:defrule plan-client-manifest (:salience 30)
      (?request (shell-request (verb :manifest) (resource :server)))
      =>
      (plan-request ?request :client-manifest :read 'plan-client-manifest
                    "Fetch the machine-readable StarIntel client manifest."))

    (lisa:defrule plan-document-get (:salience 30)
      (?request (shell-request (verb :get) (resource :document)))
      =>
      (plan-request ?request :document-get :read 'plan-document-get
                    "Fetch one persisted document by identifier."))

    (lisa:defrule plan-document-search (:salience 30)
      (?request (shell-request (verb :search) (resource :document)))
      =>
      (plan-request ?request :document-search :read 'plan-document-search
                    "Search indexed StarIntel documents."))

    (lisa:defrule plan-document-submit (:salience 30)
      (?request (shell-request (verb :submit) (resource :document)))
      =>
      (plan-request ?request :document-submit :write 'plan-document-submit
                    "Create a new StarIntel document."))

    (lisa:defrule plan-document-delete (:salience 30)
      (?request (shell-request (verb :delete) (resource :document)))
      =>
      (plan-request ?request :document-delete :destructive 'plan-document-delete
                    "Delete a persisted StarIntel document."))

    (lisa:defrule plan-target-list (:salience 30)
      (?request (shell-request (verb :list) (resource :target)))
      =>
      (plan-request ?request :target-list :read 'plan-target-list
                    "List persisted targets for an actor."))

    (lisa:defrule plan-target-get (:salience 30)
      (?request (shell-request (verb :get) (resource :target)))
      =>
      (plan-request ?request :target-get :read 'plan-target-get
                    "Fetch a target document by identifier."))

    (lisa:defrule plan-target-create (:salience 30)
      (?request (shell-request (verb :create) (resource :target)))
      =>
      (plan-request ?request :target-create :write 'plan-target-create
                    "Submit a target to an actor."))

    (lisa:defrule plan-dataset-size (:salience 30)
      (?request (shell-request (verb :size) (resource :dataset)))
      =>
      (plan-request ?request :dataset-size :read 'plan-dataset-size
                    "Read the current size of a dataset."))

    (lisa:defrule plan-groups (:salience 30)
      (?request (shell-request (verb :list) (resource :groups)))
      =>
      (plan-request ?request :groups :read 'plan-groups
                    "List message groups and channels."))

    (lisa:defrule plan-messages-user (:salience 30)
      (?request (shell-request (verb :list) (resource :messages) (qualifier :user)))
      =>
      (plan-request ?request :messages-by-user :read 'plan-messages-user
                    "Query message documents by user."))

    (lisa:defrule plan-messages-platform (:salience 30)
      (?request (shell-request (verb :list) (resource :messages) (qualifier :platform)))
      =>
      (plan-request ?request :messages-by-platform :read 'plan-messages-platform
                    "Query message documents by platform."))

    (lisa:defrule plan-messages-group (:salience 30)
      (?request (shell-request (verb :list) (resource :messages) (qualifier :group)))
      =>
      (plan-request ?request :messages-by-group :read 'plan-messages-group
                    "Query grouped message documents."))

    (lisa:defrule plan-social-user (:salience 30)
      (?request (shell-request (verb :list) (resource :social) (qualifier :user)))
      =>
      (plan-request ?request :social-by-user :read 'plan-social-user
                    "Query social-media posts by user."))

    (lisa:defrule plan-raw-get (:salience 30)
      (?request (shell-request (verb :get) (resource :raw)))
      =>
      (plan-request ?request :raw-get :read 'plan-raw-get
                    "Perform a raw GET through the StarIntel client boundary."))

    (lisa:defrule plan-raw-post (:salience 30)
      (?request (shell-request (verb :post) (resource :raw)))
      =>
      (plan-request ?request :raw-post :write 'plan-raw-post
                    "Perform a raw POST through the StarIntel client boundary."))

    (lisa:defrule plan-raw-put (:salience 30)
      (?request (shell-request (verb :put) (resource :raw)))
      =>
      (plan-request ?request :raw-put :write 'plan-raw-put
                    "Perform a raw PUT through the StarIntel client boundary."))

    (lisa:defrule plan-raw-delete (:salience 30)
      (?request (shell-request (verb :delete) (resource :raw)))
      =>
      (plan-request ?request :raw-delete :destructive 'plan-raw-delete
                    "Perform a raw DELETE through the StarIntel client boundary."))

    (lisa:defrule block-unconfirmed-write (:salience 20)
      (?plan (shell-plan (risk :write) (confirmed-p nil)))
      =>
      (block-plan ?plan))

    (lisa:defrule block-unconfirmed-destructive (:salience 20)
      (?plan (shell-plan (risk :destructive) (confirmed-p nil)))
      =>
      (block-plan ?plan))

    (lisa:defrule execute-read-plan (:salience 10)
      (?plan (shell-plan (risk :read)))
      =>
      (execute-planned-operation ?plan))

    (lisa:defrule execute-confirmed-write-plan (:salience 10)
      (?plan (shell-plan (risk :write) (confirmed-p t)))
      =>
      (execute-planned-operation ?plan))

    (lisa:defrule execute-confirmed-destructive-plan (:salience 10)
      (?plan (shell-plan (risk :destructive) (confirmed-p t)))
      =>
      (execute-planned-operation ?plan))

    (lisa:defrule unsupported-shell-request (:salience -100)
      (?request (shell-request))
      =>
      (unsupported-request ?request))))

(defun shell-rule-names ()
  "Return the names of rules installed by the StarIntel expert shell."
  (mapcar #'second *shell-rule-forms*))

(defun install-shell-rules (engine)
  (lisa:with-inference-engine (engine)
    (let ((*package* (find-package :star.expert.shell)))
      (dolist (form *shell-rule-forms*)
        (eval form))))
  engine)
