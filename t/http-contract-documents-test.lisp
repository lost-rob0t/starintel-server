(in-package :star-server-tests)

(def-suite http-contract-documents-tests
  :description "Versioned document operations of the shared HTTP contract registry")

(in-suite http-contract-documents-tests)

(defun document-operation-case-values (operation)
  (list
   (star.http.contract:http-operation-method operation)
   (star.http.contract:http-operation-path operation)
   (star.http.contract:http-operation-authority operation)
   (star.http.contract:http-operation-scopes operation)))

(test registry-exposes-versioned-document-operations
  (dolist (case
           '(("documents.create"
              :post "/api/v1/documents" "documents:write")
             ("documents.bulk.create"
              :post "/api/v1/documents/bulk" "documents:bulk")
             ("documents.get"
              :get "/api/v1/documents/:id" "documents:read")
             ("documents.update"
              :put "/api/v1/documents/:id" "documents:write")
             ("documents.delete"
              :delete "/api/v1/documents/:id" "documents:delete")
             ("documents.search"
              :get "/api/v1/documents/search" "search:read")))
    (destructuring-bind (id method path scope) case
      (is (equal (list method path :authenticated (list scope))
                 (document-operation-case-values
                  (star.http.contract:find-http-operation id)))
          "operation ~a has unexpected contract metadata" id))))

(test registry-versioned-document-path-parameters
  (dolist (id '("documents.get" "documents.update" "documents.delete"))
    (is (equal '("id")
               (star.http.contract:http-operation-path-parameters
                (star.http.contract:find-http-operation id)))
        "operation ~a must expose the id path parameter" id))
  (dolist (id '("documents.create" "documents.bulk.create"
                "documents.search"))
    (is (null (star.http.contract:http-operation-path-parameters
               (star.http.contract:find-http-operation id)))
        "operation ~a must not declare path parameters" id)))

(test registry-document-request-shapes-are-strict-v09
  (flet ((document-request-schema (operation-id)
           (star.http.contract:http-operation-request-schema
            (star.http.contract:find-http-operation operation-id))))
    (is-true (document-request-schema "documents.create"))
    (is-true (document-request-schema "documents.bulk.create"))
    (is-true (document-request-schema "documents.update"))
    (is (equal '("_id" "dataset" "dtype" "schema_version")
               (jsown:val-safe (document-request-schema "documents.create")
                               "required"))
        "document create request schema must require the v0.9 envelope")
    (is-true (jsown:val-safe (document-request-schema "documents.bulk.create")
                             "items")
             "bulk request schema must be an array of documents")
    (is (search "deep-merge"
                (or (jsown:val-safe (document-request-schema "documents.update")
                                    "description")
                    ""))
        "update request schema must document the deep-merge patch semantics")))

(defun manifest-operation (document operation-id)
  (find operation-id
        (jsown:val document "operations")
        :key (lambda (operation) (jsown:val operation "operation_id"))
        :test #'string=))

(test openapi-emits-versioned-document-operations
  (let* ((document
           (jsown:with-injective-reader
             (jsown:parse (star.http.contract:openapi-json))))
         (paths (jsown:val document "paths")))
    (let ((create (jsown:val-safe paths "/api/v1/documents")))
      (is-true create)
      (is-true (jsown:val-safe create "post"))
      (is (string= "documents.create"
                   (jsown:val (jsown:val create "post")
                              "x-starintel-operation-id")))
      (let ((security (jsown:val (jsown:val create "post") "security")))
        (is (= 1 (length security)))
        (is (jsown:keyp (first security) "bearerAuth"))))
    (let ((bulk (jsown:val-safe paths "/api/v1/documents/bulk")))
      (is-true bulk)
      (is-true (jsown:val-safe bulk "post"))
      (is (equal '("documents:bulk")
                 (jsown:val (jsown:val bulk "post")
                            "x-starintel-scopes"))))
    (let ((id-path (jsown:val-safe paths "/api/v1/documents/{id}")))
      (is-true id-path)
      (dolist (method '("get" "put" "delete"))
        (is-true (jsown:val-safe id-path method)
                 "openapi must document ~a on /api/v1/documents/{id}" method))
      (let ((parameters (jsown:val (jsown:val id-path "get") "parameters")))
        (is (string= "id" (jsown:val (first parameters) "name")))
        (is (string= "path" (jsown:val (first parameters) "in")))))
    (let ((search (jsown:val-safe paths "/api/v1/documents/search")))
      (is-true search)
      (is-true (jsown:val-safe search "get"))
      (let* ((parameters (jsown:val (jsown:val search "get") "parameters"))
             (query
               (find "q" parameters
                     :key (lambda (parameter) (jsown:val parameter "name"))
                     :test #'string=)))
        (is-true query)
        (is (string= "query" (jsown:val query "in")))
        (is (eq :true (jsown:val query "required")))))))

(test client-manifest-emits-versioned-document-operations
  (let ((document
          (jsown:with-injective-reader
            (jsown:parse (star.http.contract:client-manifest-json)))))
    (let ((get-operation (manifest-operation document "documents.get")))
      (is-true get-operation)
      (is (string= "get" (jsown:val get-operation "method")))
      (is (string= "/api/v1/documents/:id"
                   (jsown:val get-operation "path")))
      (is (string= "/api/v1/documents/{id}"
                   (jsown:val get-operation "openapi_path")))
      (is (equal '("documents:read") (jsown:val get-operation "scopes"))))
    (let ((search (manifest-operation document "documents.search")))
      (is-true search)
      (let ((query-parameters (jsown:val search "query_parameters")))
        (is (= 6 (length query-parameters)))
        (let ((q (first query-parameters)))
          (is (string= "q" (jsown:val q "name")))
          (is (eq :true (jsown:val q "required"))))))
    (let ((update (manifest-operation document "documents.update")))
      (is-true (jsown:val-safe update "request_schema"))
      (is (string= "put" (jsown:val update "method"))))))

(test generated-client-document-wrappers-exist
  (dolist (name '("REQUEST-DOCUMENT-CREATE"
                  "REQUEST-DOCUMENT-BULK-CREATE"
                  "REQUEST-DOCUMENT-GET"
                  "REQUEST-DOCUMENT-UPDATE"
                  "REQUEST-DOCUMENT-DELETE"
                  "REQUEST-DOCUMENT-SEARCH"))
    (multiple-value-bind (symbol status)
        (find-symbol name :star.api.client)
      (is (eq :external status))
      (is-true (fboundp symbol)))))

(test generated-document-get-wrapper-uses-contracted-uri
  (let ((captured nil))
    (let* ((transport
             (star.api.client:make-function-transport
              (lambda (request)
                (setf captured request)
                (fake-json-response
                 200
                 "{\"_id\":\"doc-1\",\"dtype\":\"host\",\"schema_version\":\"0.9.0\"}"))))
           (client (star.api.client:make-star-client
                    :base-url "http://example.test"
                    :transport transport)))
      (star.api.client:call-operation
       client "documents.get"
       :path-parameters (list (cons "id" "doc-1")))
      (is (eq :get (star.api.client:client-request-method captured)))
      (is (string= "http://example.test/api/v1/documents/doc-1"
                   (star.api.client:client-request-uri captured)))
      (is (string= "documents.get"
                   (star.api.client:client-request-operation-id captured))))))

(test hardened-boundary-authorizes-versioned-document-routes
  (flet ((action (method path)
           (star.frontends.http-api::route-action method path)))
    (is (string= "documents:write" (action :post "/api/v1/documents")))
    (is (string= "documents:bulk" (action :post "/api/v1/documents/bulk")))
    (is (string= "documents:read" (action :get "/api/v1/documents/doc-1")))
    (is (string= "documents:write" (action :put "/api/v1/documents/doc-1")))
    (is (string= "documents:delete" (action :delete "/api/v1/documents/doc-1")))
    (is (string= "search:read" (action :get "/api/v1/documents/search")))))

(defun boundary-headers-table (alist)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair alist table)
      (setf (gethash (string-downcase (car pair)) table) (cdr pair)))))

(defun boundary-env (method path &key body content-type headers)
  (list :request-method method
        :path-info path
        :url-scheme "http"
        :server-name "127.0.0.1"
        :server-port 5000
        :remote-addr "127.0.0.1"
        :content-type (or content-type
                          (and headers
                               (cdr (assoc "content-type"
                                           headers
                                           :test #'string-equal))))
        :raw-body (and body
                       (flexi-streams:make-in-memory-input-stream
                        (babel:string-to-octets body :encoding :utf-8)))
        :content-length (and body
                             (length (babel:string-to-octets body
                                                             :encoding :utf-8)))
        :headers (boundary-headers-table headers)))

(defun boundary-status (response)
  (first response))

(defun boundary-body (response)
  (let ((body (third response)))
    (if (consp body)
        (apply #'concatenate 'string body)
        body)))

(defun boundary-header (response name)
  (let ((headers (second response)))
    (cond
      ((and (consp headers) (consp (first headers)))
       (cdr (assoc name headers :test #'string-equal)))
      ((consp headers)
       (getf headers (intern (string-upcase name) :keyword))))))

(defun boundary-correlation-id (response)
  (boundary-header response "x-correlation-id"))

(defun boundary-response-code (response)
  (let ((parsed (ignore-errors (jsown:parse (boundary-body response)))))
    (and parsed (jsown:val-safe parsed "code"))))

(test capabilities-discovery-exposes-versioned-document-endpoints
  (let* ((document (star.frontends.http-api::capabilities-document))
         (endpoints (jsown:val (jsown:val document "data") "endpoints")))
    (flet ((endpoint (id)
             (find id endpoints
                   :key (lambda (entry) (jsown:val entry "id"))
                   :test #'string=)))
      (let ((create (endpoint "document_create_v1")))
        (is (string= "POST" (jsown:val create "method")))
        (is (string= "/api/v1/documents" (jsown:val create "path")))
        (is (eq :false (jsown:val create "legacy")))
        (is (equal '("documents:write") (jsown:val create "scopes"))))
      (let ((search (endpoint "document_search_v1")))
        (is (string= "GET" (jsown:val search "method")))
        (is (string= "/api/v1/documents/search"
                     (jsown:val search "path"))))
      (is-true (endpoint "document_read_v1"))
      (is-true (endpoint "document_update_v1"))
      (is-true (endpoint "document_delete_v1"))
      (is-true (endpoint "document_bulk_create_v1")))))

(test versioned-document-routes-are-mounted-from-the-registry
  (dolist (case '((:post "/api/v1/documents")
                  (:post "/api/v1/documents/bulk")
                  (:get "/api/v1/documents/:id")
                  (:put "/api/v1/documents/:id")
                  (:delete "/api/v1/documents/:id")
                  (:get "/api/v1/documents/search")))
    (destructuring-bind (method path) case
      (is (functionp
           (ningle:route star.frontends.http-api:*app* path :method method))
          "route ~a ~a must be mounted" method path))))

(test document-search-dispatches-before-id-capture
  ;; myway dispatches in registration order, so the static search route must
  ;; be registered before the :id route or it would be captured as an id.
  ;; The search handler requires q; the :id handler would report a missing
  ;; path parameter instead.
  (let ((response
          (lack.component:call
           star.frontends.http-api:*app*
           (boundary-env :get "/api/v1/documents/search"))))
    (is (string= "missing_query_parameter"
                 (boundary-response-code response)))))

(test unauthenticated-versioned-document-create-is-rejected-with-401
  (let ((star:*auth-mode* "api-key"))
    (let ((response
            (lack.component:call
             star.frontends.http-api::*server*
             (boundary-env
              :post "/api/v1/documents"
              :body "{\"_id\":\"doc-401\"}"
              :headers (list (cons "Content-Type" "application/json"))))))
      (is (= 401 (boundary-status response)))
      (is (string= "invalid_credential" (boundary-response-code response)))
      (is-true (boundary-correlation-id response))
      (is-true (jsown:val-safe (jsown:parse (boundary-body response))
                               "correlation_id")))))

(test unauthorized-versioned-document-create-is-rejected-with-403
  (let* ((star:*auth-pepper* "documents-contract-test-pepper")
         (star:*auth-mode* "api-key")
         (store (star.auth:make-memory-credential-store)))
    (multiple-value-bind (record raw-key)
        (star.auth:create-api-key
         "documents-reader" "api_client" '("documents:read")
         :store store)
      (declare (ignore record))
      (let ((star.auth:*credential-store* store))
        (let ((response
                (lack.component:call
                 star.frontends.http-api::*server*
                 (boundary-env
                  :post "/api/v1/documents"
                  :body (jsown:to-json
                         (make-boundary-document :id "doc-403"))
                  :headers (list (cons "Content-Type" "application/json")
                                 (cons "Authorization"
                                       (format nil "Bearer ~a" raw-key)))))))
          (is (= 403 (boundary-status response)))
          (is (string= "access_denied" (boundary-response-code response)))
          (is-true (boundary-correlation-id response)))))))

(test invalid-versioned-document-create-is-rejected-with-422-envelope
  (let ((star:*auth-mode* "disabled")
        (star:*auth-dev-bypass* t))
    (let ((response
            (lack.component:call
             star.frontends.http-api::*server*
             (boundary-env
              :post "/api/v1/documents"
              :body "{\"_id\":\"doc-422\",\"dtype\":\"host\",\"dataset\":\"testing\"}"
              :content-type "application/json"))))
      (is (= 422 (boundary-status response)))
      (is (string= "schema_version_required"
                   (boundary-response-code response)))
      (is-true (boundary-correlation-id response))
      (let ((parsed (jsown:parse (boundary-body response))))
        (is (string= "error" (jsown:val parsed "status")))
        (is-true (jsown:val-safe parsed "correlation_id"))))))

(test malformed-json-on-versioned-document-create-is-rejected-with-400
  ;; The framework request parser rejects malformed JSON bodies with a bare
  ;; 400 before the application boundary is reached (pre-existing behavior
  ;; for every route, legacy included). The structured malformed_json
  ;; envelope of the hardened boundary applies to framework-parseable bodies
  ;; and is covered by t/http-boundary-test.lisp.
  (let ((star:*auth-mode* "disabled")
        (star:*auth-dev-bypass* t))
    (let ((response
            (lack.component:call
             star.frontends.http-api::*server*
             (boundary-env
              :post "/api/v1/documents"
              :body "{\"_id\":"
              :content-type "application/json"))))
      (is (= 400 (boundary-status response)))
      (is (null (search "internal" (string (boundary-body response))))))))

(test dataset-size-is-defined-exactly-once-in-http-api-source
  ;; README known gap: the second /dataset-size definition in http-api.lisp
  ;; replaces the first. The dead definition must stay deleted.
  (let* ((directory (asdf:system-source-directory :starintel-gserver))
         (source (uiop:read-file-string
                  (merge-pathnames "frontends/http-api.lisp" directory)))
         (needle "(setf (ningle:route *app* \"/dataset-size\" :method :get)")
         (matches 0))
    (loop for position = (search needle source)
          then (search needle source :start2 (1+ position))
          while position
          do (incf matches))
    (is (= 1 matches)
        "http-api.lisp must define /dataset-size exactly once")))
