(in-package :star.authorization)

(defun quota-value (quotas key)
  (cond
    ((null quotas) (values nil nil))
    ((hash-table-p quotas) (gethash key quotas))
    ((listp quotas)
     (let ((tail (member key quotas :test #'eq)))
       (if tail
           (values (second tail) t)
           (values nil nil))))
    (t (values nil nil))))

(defun quotas-granted-p (quotas)
  "Closed quota contract. Missing fields are neutral; explicit exhaustion denies."
  (if (null quotas)
      t
      (multiple-value-bind (allowed-p allowed-present-p)
          (quota-value quotas :allowed-p)
        (multiple-value-bind (exhausted-p exhausted-present-p)
            (quota-value quotas :exhausted-p)
          (multiple-value-bind (remaining remaining-present-p)
              (quota-value quotas :remaining)
            (and (or (not allowed-present-p) allowed-p)
                 (or (not exhausted-present-p) (not exhausted-p))
                 (or (not remaining-present-p)
                     (and (numberp remaining)
                          (plusp remaining)))))))))

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
