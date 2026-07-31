(in-package :star-server-tests)

(in-suite authorization-policy-tests)

(test explicit-quota-exhaustion-denies-an-otherwise-valid-request
  (let* ((principal
           (make-policy-principal
            "quota-reader"
            '("documents:read"
              "tenant:default"
              "dataset:dataset-a")))
         (resource (policy-resource :dataset "dataset-a")))
    (is-true
     (star.authorization:authorize!
      "documents:read"
      :principal principal
      :resource resource
      :quotas '(:remaining 1)))
    (dolist (quotas
             '((:remaining 0)
               (:exhausted-p t)
               (:allowed-p nil)))
      (handler-case
          (progn
            (star.authorization:authorize!
             "documents:read"
             :principal principal
             :resource resource
             :quotas quotas)
            (fail "Exhausted quota unexpectedly allowed"))
        (star.authorization:authorization-error (condition)
          (is (string= "quota_exceeded"
                       (star.authorization:authorization-decision-reason
                        (star.authorization:authorization-error-decision
                         condition)))))))))

(test bulk-preauthorization-prevents-all-side-effects-on-one-denial
  (let ((principal
          (make-policy-principal
           "bulk-writer"
           '("documents:bulk"
             "documents:write"
             "tenant:default"
             "dataset:dataset-a")))
        (published 0)
        (documents
          (list (policy-document :id "a" :dataset "dataset-a")
                (policy-document :id "b" :dataset "dataset-b"))))
    (handler-case
        (progn
          (star.authorization:authorize-bulk-documents!
           documents :principal principal)
          (dolist (document documents)
            (star.authorization:authorized-publish-document
             document
             (lambda (value)
               (declare (ignore value))
               (incf published))
             :principal principal)))
      (star.authorization:authorization-error () nil))
    (is (zerop published))))
