(in-package :star.authorization)

(defun quota-value (quotas key)
  (cond
    ((null quotas) nil)
    ((hash-table-p quotas) (gethash key quotas))
    ((listp quotas) (getf quotas key))
    (t nil)))

(defun quotas-granted-p (quotas)
  "Closed quota contract. Missing quota state is neutral; explicit exhaustion denies."
  (if (null quotas)
      t
      (let ((allowed-p (quota-value quotas :allowed-p))
            (exhausted-p (quota-value quotas :exhausted-p))
            (remaining (quota-value quotas :remaining)))
        (and (not (eq allowed-p nil))
             (not exhausted-p)
             (or (null remaining)
                 (and (numberp remaining)
                      (plusp remaining)))))))

(defun policy-decision-reason (principal scopes action resource quotas)
  (let ((base-reason (decision-reason principal scopes action resource)))
    (if (and (string= base-reason "matching_grant")
             (not (quotas-granted-p quotas)))
        "quota_exceeded"
        base-reason)))

(defmethod evaluate-authorization
    ((engine default-deny-policy-engine) request)
  (declare (ignore engine))
  (let* ((principal (candidate-principal
                     (authorization-request-principal request)))
         (scopes (principal-scopes principal))
         (action (authorization-request-action request))
         (resource (authorization-request-resource request))
         (reason
           (policy-decision-reason
            principal
            scopes
            action
            resource
            (authorization-request-quotas request)))
         (allowed-p (string= reason "matching_grant")))
    (make-authorization-decision
     :id (cms-ulid:ulid)
     :allowed-p allowed-p
     :reason reason
     :action action
     :resource resource
     :principal-id (principal-id principal))))
