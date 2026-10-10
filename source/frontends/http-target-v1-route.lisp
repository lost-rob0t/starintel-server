(in-package :star.frontends.http-api)

(defun target-v1-outcome-disposition (outcome)
  (case (star.actors::target-dispatch-outcome-status outcome)
    (:accepted :created)
    (:duplicate :duplicate)
    (otherwise nil)))


(defun target-v1-historical-request-identity (principal idempotency-key)
  "Exact pre-canonical HTTP identity; used only to find historical acceptances."
  (target-v1-digest (format nil "~a|~a" principal idempotency-key)))

(defun target-v1-load-historical-acceptance (acceptance-id)
  (anypool:with-connection (client star.databases.couchdb:*couchdb-pool*)
    (star.databases.couchdb::couchdb-load-target-acceptance
     client star:*couchdb-default-database* acceptance-id)))

(defun target-v1-historical-identity-matches-p (acceptance principal identity)
  "Check ownership and linked identities without trusting a legacy digest alone."
  (handler-case
      (let* ((schedule-id (format nil "target-request:~a" identity))
             (target-id (format nil "target:~a" identity))
             (document (star.documents:object-value acceptance "target_document"))
             (extensions (star.documents:object-value document "extensions")))
        (and (json-object-p acceptance) (json-object-p document) (json-object-p extensions)
             (equal "_server_target_acceptance"
                    (star.documents:object-value acceptance "type"))
             (equal (star.actors:target-acceptance-id schedule-id)
                    (star.documents:object-value acceptance "_id"))
             (equal schedule-id (star.documents:object-value acceptance "schedule_id"))
             (equal target-id (star.documents:object-value acceptance "target_id"))
             (equal target-id (star.documents:object-value document "_id"))
             (equal "target" (star.documents:object-value document "dtype"))
             (equal starintel.legacy:+starintel-doc-version+
                    (star.documents:object-value document "schema_version"))
             (not (star.documents:object-has-key-p document "id"))
             (not (star.documents:object-has-key-p document "schemaVersion"))
             (equal identity (star.documents:object-value extensions "idempotency_key"))
             (equal schedule-id (star.documents:object-value extensions "schedule_id"))
             (equal (target-v1-digest principal)
                    (star.documents:object-value extensions "submitted_by"))
             (equal (or (star.documents:object-value document "_rev") :null)
                    (star.documents:object-value acceptance "target_revision"))
             (star.documents:validate-document document)
             t))
    (error () nil)))

(defun guard-target-v1-historical-acceptance
    (request principal &key (lookup-fn #'target-v1-load-historical-acceptance))
  "Never reuse or reschedule a historical request through the new identity."
  (let* ((identity (target-v1-historical-request-identity
                    principal (target-v1-idempotency-key request)))
         (schedule-id (format nil "target-request:~a" identity))
         (existing (funcall lookup-fn (star.actors:target-acceptance-id schedule-id))))
    (when existing
      (unless (target-v1-historical-identity-matches-p existing principal identity)
        (signal-http-input-error
         409 "target_idempotency_version_conflict"
         "This idempotency identity cannot be safely reused across Target versions"))
      (signal-http-input-error
       409 "target_idempotency_version_conflict"
       "An earlier Target version already used this idempotency key; cross-version reuse is blocked")))
  t)

(defun target-v1-accept-record
    (request principal record
     &key (lookup-fn #'target-v1-load-historical-acceptance)
          (accept-fn #'star.actors:accept-target-record))
  (guard-target-v1-historical-acceptance request principal :lookup-fn lookup-fn)
  (funcall accept-fn record))

(defun handle-v1-target-create-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (let* ((request (require-json-object (parse-json-request)))
           (principal (request-principal))
           (document (target-v1-document-from-request request principal))
           ;; Authorize the concrete resource before any lookup or durable effect.
           (star.authorization:*current-authorization-decision*
             (star.authorization:authorize-document!
              "targets:dispatch" document
              :principal (current-policy-principal)
              :actor-name (jsown:val document "actor")
              :metadata (route-policy-metadata +target-v1-path+ "POST")))
           (ledger (target-v1-request-ledger request document principal))
           (record (star.actors::parse-target-record document))
           (outcome (target-v1-accept-record request principal record))
           (disposition (target-v1-outcome-disposition outcome)))
      (unless disposition
        (signal-http-input-error
         409
         "target_request_rejected"
         (or (star.actors::target-dispatch-outcome-reason outcome)
             "Target request was rejected")))
      (setf (lack.response:response-status *response*)
            (if (eq disposition :created) 201 200))
      (jsown:to-json (target-v1-receipt ledger disposition)))))

(setf (ningle:route *app* +target-v1-path+ :method :post)
      #'handle-v1-target-create-route)