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

(defmethod evaluate-authorization :around
    ((engine default-deny-policy-engine) request)
  "Apply quota denial without replacing the policy engine's primary method."
  (let ((decision (call-next-method)))
    (if (and (authorization-decision-allowed-p decision)
             (not (quotas-granted-p
                   (authorization-request-quotas request))))
        (make-authorization-decision
         :id (authorization-decision-id decision)
         :allowed-p nil
         :reason "quota_exceeded"
         :action (authorization-decision-action decision)
         :resource (authorization-decision-resource decision)
         :principal-id (authorization-decision-principal-id decision))
        decision)))
