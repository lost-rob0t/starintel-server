;; SPDX-License-Identifier: GPL-3.0-or-later
(in-package :star.http.contract)

(defparameter +export-create-request-schema+
  (object-schema
   (list
    (cons "dataset"
          (string-schema
           :min-length 1
           :description "Dataset to export. Authorization is checked before database I/O."))
    (cons "tenant"
          (string-schema
           :min-length 1
           :description "Authorization tenant. Defaults to default."))
    (cons "publish_ipfs"
          (boolean-schema
           :description "When true, publish the completed server-owned artifact through the local IPFS actor.")))
   :required '("dataset")
   :additional-properties nil
   :description "Create a bounded JSON Lines dataset export."))

(defparameter +export-receipt-schema+
  (object-schema
   (list
    (cons "export_id" (string-schema :min-length 1))
    (cons "dataset" (string-schema :min-length 1))
    (cons "format" (string-schema))
    (cons "exported" (integer-schema :minimum 0))
    (cons "bytes" (integer-schema :minimum 0))
    (cons "sha256" (string-schema :min-length 64))
    (cons "consistency" (string-schema))
    (cons "created_at" (integer-schema :minimum 0))
    (cons "status" (string-schema))
    (cons "cid" (string-schema))
    (cons "license" (string-schema))
    (cons "license_scope" (string-schema))
    (cons "source_repository" (string-schema))
    (cons "correlation_id" (string-schema)))
   :required '("export_id" "dataset" "format" "exported" "bytes" "sha256"
               "consistency" "created_at" "status" "license"
               "license_scope" "source_repository" "correlation_id")
   :additional-properties t
   :description "Content-integrity receipt for one server-owned export artifact."))

(defparameter +self-user-schema+
  (object-schema
   (list
    (cons "principal_id" (string-schema :min-length 1))
    (cons "principal_type" (string-schema :min-length 1))
    (cons "credential_id" (string-schema))
    (cons "scopes" (array-schema (string-schema)))
    (cons "user" (generic-object-schema))
    (cons "license" (string-schema))
    (cons "source_repository" (string-schema))
    (cons "correlation_id" (string-schema)))
   :required '("principal_id" "principal_type" "scopes" "license"
               "source_repository" "correlation_id")
   :additional-properties t
   :description "Self-service principal metadata. Secret verifier and password fields are never returned."))

(defparameter +billing-line-item-schema+
  (object-schema
   (list
    (cons "description" (string-schema :min-length 1))
    (cons "quantity" (integer-schema :minimum 1))
    (cons "unit_price_micros" (integer-schema :minimum 0)))
   :required '("description" "quantity" "unit_price_micros")
   :additional-properties nil
   :description "Caller-supplied deterministic billing example line item."))

(defparameter +billing-preview-request-schema+
  (object-schema
   (list
    (cons "currency"
          (string-schema
           :min-length 3
           :description "Three-letter currency code such as USD."))
    (cons "items"
          (array-schema
           +billing-line-item-schema+
           :description "Bounded caller-supplied line items.")))
   :required '("currency" "items")
   :additional-properties nil
   :description "Pure billing example input. This endpoint never charges or mutates balances."))

(defparameter +billing-preview-response-schema+
  (object-schema
   (list
    (cons "principal_id" (string-schema :min-length 1))
    (cons "currency" (string-schema :min-length 3))
    (cons "items" (array-schema (generic-object-schema)))
    (cons "total_micros" (integer-schema :minimum 0))
    (cons "preview" (boolean-schema))
    (cons "charged" (boolean-schema))
    (cons "created_at" (integer-schema :minimum 0))
    (cons "license" (string-schema))
    (cons "source_repository" (string-schema))
    (cons "correlation_id" (string-schema)))
   :required '("principal_id" "currency" "items" "total_micros"
               "preview" "charged" "created_at" "license"
               "source_repository" "correlation_id")
   :additional-properties false
   :description "Deterministic non-mutating billing example."))

(upsert-http-operation
 (make-http-operation
  :id "exports.create"
  :client-name "export-create"
  :method :post
  :path "/api/v1/exports"
  :summary "Create an authorized JSON Lines export and optionally publish it to local IPFS"
  :tags '("exports" "documents")
  :authority :authenticated
  :scopes '("documents:read")
  :request-schema +export-create-request-schema+
  :responses
  (append
   (list (response 201 "Export completed." +export-receipt-schema+))
   (standard-errors))))

(upsert-http-operation
 (make-http-operation
  :id "users.me.get"
  :client-name "user-me"
  :method :get
  :path "/api/v1/users/me"
  :summary "Read metadata for the authenticated principal and matching human user"
  :tags '("users" "auth")
  :authority :authenticated
  :scopes '("identity:read")
  :responses
  (append
   (list (response 200 "Authenticated self metadata." +self-user-schema+))
   (standard-errors))))

(upsert-http-operation
 (make-http-operation
  :id "users.me.billing.preview"
  :client-name "billing-preview"
  :method :post
  :path "/api/v1/users/me/billing/preview"
  :summary "Calculate a deterministic billing example without charging or mutating balances"
  :tags '("users" "billing")
  :authority :authenticated
  :scopes '("identity:read")
  :request-schema +billing-preview-request-schema+
  :responses
  (append
   (list (response 200 "Non-charging billing preview." +billing-preview-response-schema+))
   (standard-errors))))
