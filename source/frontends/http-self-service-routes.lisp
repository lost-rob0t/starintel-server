;; SPDX-License-Identifier: GPL-3.0-or-later
(in-package :star.frontends.http-api)

(defun current-self-principal ()
  (or (star.auth:current-request-principal)
      (signal-http-input-error
       401
       "invalid_credential"
       "Authentication failed")))

(defun nullable-self-user (principal-id)
  (let ((store star.auth:*credential-store*))
    (if store
        (let ((record
                (handler-case
                    (star.auth:user-store-get store principal-id)
                  (error () nil))))
          (if record
              (star.auth:user-metadata-json record)
              :null))
        :null)))

(defun self-user-document ()
  (let* ((principal (current-self-principal))
         (principal-id (star.auth:request-principal-id principal)))
    (jsown:new-js
      ("principal_id" principal-id)
      ("principal_type" (star.auth:request-principal-type principal))
      ("credential_id"
       (or (star.auth:request-principal-credential-id principal) :null))
      ("scopes" (copy-list (star.auth:request-principal-scopes principal)))
      ("user" (nullable-self-user principal-id))
      ("license" star.http.contract:+software-license+)
      ("license_scope" "server-software")
      ("source_repository" star.http.contract:+source-repository+)
      ("correlation_id" (current-correlation-id)))))

(defun handle-user-me-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (add-no-store-header)
    (jsown:to-json (self-user-document))))

(defun billing-preview-items (body)
  (let ((items (jsown:val-safe body "items")))
    (unless (and (json-array-p items) (plusp (length items)))
      (signal-http-input-error
       422
       "invalid_billing_preview"
       "Field items must be a non-empty JSON array"))
    items))

(defun handle-user-billing-preview-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (add-no-store-header)
    (let* ((body (require-json-object (parse-json-request)))
           (currency (require-auth-string body "currency"))
           (items (billing-preview-items body))
           (principal-id
             (star.auth:request-principal-id (current-self-principal))))
      (handler-case
          (let ((preview
                  (star.billing:make-billing-preview
                   principal-id currency items)))
            (setf (jsown:val preview "correlation_id")
                  (current-correlation-id))
            (jsown:to-json preview))
        (error (condition)
          (signal-http-input-error
           422
           "invalid_billing_preview"
           (princ-to-string condition)))))))

(defun export-request-tenant (body)
  (let ((tenant (jsown:val-safe body "tenant")))
    (cond
      ((or (null tenant) (eq tenant :null)) "default")
      ((and (stringp tenant) (plusp (length tenant))) tenant)
      (t
       (signal-http-input-error
        422
        "invalid_export_request"
        "Field tenant must be a non-empty string")))))

(defun authorize-dataset-export! (dataset tenant)
  (star.authorization:authorize!
   "documents:read"
   :principal (current-policy-principal)
   :resource
   (star.authorization:make-authorization-resource
    :tenant-id tenant
    :dataset-id dataset)
   :metadata (route-policy-metadata "/api/v1/exports" "POST")))

(defun authorize-ipfs-export-publish! (dataset tenant)
  (star.authorization:authorize!
   "targets:dispatch"
   :principal (current-policy-principal)
   :resource
   (star.authorization:make-authorization-resource
    :tenant-id tenant
    :dataset-id dataset
    :actor-name "ipfs")
   :metadata (route-policy-metadata "/api/v1/exports" "POST")))

(defun publish-export-to-ipfs (manifest)
  (handler-case
      (star.actors.ipfs:publish-export manifest)
    (error (condition)
      (signal-http-input-error
       502
       "ipfs_publish_failed"
       "The export completed, but IPFS publication failed"
       (jsown:new-js
         ("export_id" (jsown:val manifest "export_id"))
         ("reason" (princ-to-string condition)))))))

(defun handle-export-create-route (params)
  (declare (ignore params))
  (with-http-boundary ()
    (let* ((body (require-json-object (parse-json-request)))
           (dataset (require-auth-string body "dataset"))
           (tenant (export-request-tenant body))
           (publish-ipfs
             (optional-auth-boolean body "publish_ipfs" nil)))
      (when (> (length dataset) 256)
        (signal-http-input-error
         422
         "invalid_export_request"
         "Dataset name exceeds the 256 character bound"))
      (authorize-dataset-export! dataset tenant)
      (when publish-ipfs
        (authorize-ipfs-export-publish! dataset tenant))
      (couchdb-handler (client *couchdb-pool*)
        (let* ((manifest
                 (star.exports:create-dataset-export
                  client
                  star:*couchdb-default-database*
                  dataset))
               (ipfs-result
                 (when publish-ipfs
                   (publish-export-to-ipfs manifest)))
               (cid
                 (and ipfs-result
                      (jsown:val-safe ipfs-result "cid")))
               (receipt
                 (star.exports:export-receipt-json
                  manifest
                  :cid cid
                  :correlation-id (current-correlation-id))))
          (setf (lack.response:response-status *response*) 201)
          (add-no-store-header)
          (jsown:to-json receipt))))))
