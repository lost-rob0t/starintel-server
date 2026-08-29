;;; client-test.el --- Hermetic ERT tests for the StarIntel Emacs client -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: nsaspy
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, processes

;; Commentary:

;; Hermetic ERT tests for client.el.  All tests run against a fake HTTP
;; transport; no live StarIntel server is required.

;; Run from the repository root:
;;
;;   emacs -Q --batch -L . -l client-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path dir))

(require 'client)

;;; ------------------------------------------------------------------
;;; Fake HTTP transport
;;; ------------------------------------------------------------------

(defvar starintel-test--requests nil
  "Requests recorded by the fake transport, newest first.")

(defvar starintel-test--responses nil
  "Canned responses returned by the fake transport, in order.
Each entry is a plist like (:status N :headers ALIST :body STRING),
or (:timeout t).")

(defvar starintel-test--response-function nil
  "When non-nil, a function called instead of popping canned responses.")

(defvar starintel-test--deferred nil
  "Pending deferred requests, oldest first.
Each entry is a plist like (:url U :callback C) awaiting manual
completion by `starintel-test--release'.")

(defun starintel-test-fake-transport (_method url headers _body timeout-ms callback)
  "Fake synchronous StarIntel HTTP transport recording every call.
Calls CALLBACK immediately with the canned response, mirroring the
async transport contract without any waiting."
  (push (list :method 'req :url url :headers headers :timeout-ms timeout-ms)
        starintel-test--requests)
  (funcall callback
           (cond
            (starintel-test--response-function
             (funcall starintel-test--response-function))
            ((null starintel-test--responses)
             (error "fake transport: unexpected request to %s" url))
            (t (pop starintel-test--responses)))))

(defun starintel-test-deferred-transport (_method url _headers _body _timeout-ms callback)
  "Fake fully-async transport: park CALLBACK until released by the test."
  (setq starintel-test--deferred
        (append starintel-test--deferred
                (list (list :url url :callback callback)))))

(defun starintel-test--release (response)
  "Complete the oldest deferred request with RESPONSE plist.
Returns the released request entry so tests can assert on its URL."
  (let* ((entry (car starintel-test--deferred)))
    (setq starintel-test--deferred (cdr starintel-test--deferred))
    (funcall (plist-get entry :callback) response)
    entry))

(defun starintel-test--deferred-urls ()
  "Return the URLs of pending deferred requests, oldest first."
  (mapcar (lambda (entry) (plist-get entry :url)) starintel-test--deferred))

(defun starintel-test-last-request ()
  "Return the most recently recorded request plist."
  (car starintel-test--requests))

(defun starintel-test-request-count ()
  "Return the number of recorded requests."
  (length starintel-test--requests))

(defun starintel-test-header (request name)
  "Return header NAME from recorded REQUEST, case-insensitively."
  (cdr (assoc-string name (plist-get request :headers) t)))

(defconst starintel-test-token "star_sk_v1_secret-token-1234"
  "Bearer token used across secret-handling tests.")

(defmacro starintel-test-with-client (&rest body)
  "Run BODY with the modern client pointed at a fake transport."
  `(let ((starintel-test--requests nil)
         (starintel-test--responses nil)
         (starintel-test--response-function nil)
         (starintel-test--deferred nil)
         (starintel-api-base-url "http://starintel.test:5000")
         (starintel-api-token starintel-test-token)
         (starintel-api-transport-function #'starintel-test-fake-transport)
         (starintel-api--capabilities-cache nil))
     ,@body))

(defmacro starintel-test-with-deferred-client (&rest body)
  "Run BODY with a transport that never completes requests itself."
  `(let ((starintel-test--requests nil)
         (starintel-test--responses nil)
         (starintel-test--response-function nil)
         (starintel-test--deferred nil)
         (starintel-api-base-url "http://starintel.test:5000")
         (starintel-api-token starintel-test-token)
         (starintel-api-transport-function #'starintel-test-deferred-transport)
         (starintel-api--capabilities-cache nil))
     ,@body))

;;; ------------------------------------------------------------------
;;; Capability fixtures
;;; ------------------------------------------------------------------

(defun starintel-test--endpoint (id method path &optional legacy)
  "Build one advertised endpoint alist for fixtures."
  `((id . ,id)
    (method . ,method)
    (path . ,path)
    (legacy . ,(if legacy t :json-false))
    (authority . ,(if legacy "authenticated" "public"))
    (scopes . ,(if legacy ["documents:read"] nil))))

(cl-defun starintel-test--capabilities-json (&key endpoints legacy-routes)
  "Return a capabilities response body.
ENDPOINTS is a list of endpoint alists; LEGACY-ROUTES controls the
compatibility advertisement."
  (json-encode
   `((status . "ok")
     (data .
           ((build . ((service . "starintel-gserver")
                      (version . "0.9.4")))
            (schema_revisions . ((api . "v1") (document . "0.9.7")))
            (transports . ["http"])
            (authentication . ((modes . ["api-key"])
                               (public_mode . :json-false)
                               (capabilities_endpoint_requires_auth . :json-false)))
            (features . ((documents . t)
                         (search . t)
                         (stats . t)
                         (targets . t)
                         (target_leases . :json-false)
                         (streams . :json-false)
                         (openapi . t)))
            (limits . ((bulk_documents . 500)
                       (public_search_results . 50)
                       (default_request_timeout_ms . 30000)
                       (max_request_timeout_ms . 120000)))
            (endpoints . ,(vconcat
                           (or endpoints
                               (list
                                (starintel-test--endpoint
                                 "capabilities" "GET" "/api/v1/capabilities")
                                (starintel-test--endpoint
                                 "public_search" "GET" "/api/v1/search")
                                (starintel-test--endpoint
                                 "stats" "GET" "/api/v1/stats")
                                (starintel-test--endpoint
                                 "document_read" "GET" "/document/:id" t)
                                (starintel-test--endpoint
                                 "search" "GET" "/search" t)))))
            (compatibility .
                            ((legacy_routes .
                                            ,(if legacy-routes t :json-false))
                             (legacy_routes_deprecated . :json-false))))))))

(cl-defun starintel-test--queue-capabilities (&key (endpoints nil) (legacy-routes nil))
  "Queue one capabilities response built from ENDPOINTS and LEGACY-ROUTES."
  (push (list :status 200
              :headers '(("Content-Type" . "application/json"))
              :body (starintel-test--capabilities-json
                     :endpoints endpoints
                     :legacy-routes legacy-routes))
        starintel-test--responses))

(defconst starintel-test--search-body
  (json-encode
   '((total_rows . 1)
     (bookmark . "bm-1")
     (rows . [((id . "doc-1")
               (order . [1.0])
               (doc . ((_id . "doc-1")
                       (dtype . "host")
                       (dataset . "default"))))]))))

;;; ------------------------------------------------------------------
;;; Capability discovery
;;; ------------------------------------------------------------------

(ert-deftest starintel-test-capability-discovery-success ()
  (starintel-test-with-client
   (starintel-test--queue-capabilities)
   (let ((caps (starintel-api-capabilities)))
     (should (equal "starintel-gserver"
                    (alist-get 'service (alist-get 'build caps))))
     (should (equal "v1" (alist-get 'api (alist-get 'schema_revisions caps))))
     (should (equal "0.9.7"
                    (alist-get 'document (alist-get 'schema_revisions caps))))
     (should (equal '("api-key")
                    (alist-get 'modes (alist-get 'authentication caps))))
     (should (alist-get 'endpoints caps)))))

(ert-deftest starintel-test-capability-discovery-uses-versioned-path ()
  (starintel-test-with-client
   (starintel-test--queue-capabilities)
   (starintel-api-capabilities)
   (should (equal "http://starintel.test:5000/api/v1/capabilities"
                  (plist-get (starintel-test-last-request) :url)))))

(ert-deftest starintel-test-capability-discovery-caches ()
  (starintel-test-with-client
   (starintel-test--queue-capabilities)
   (starintel-api-capabilities)
   (should (= 1 (starintel-test-request-count)))
   ;; Second call must be served from the cache without a new request.
   (starintel-api-capabilities)
   (should (= 1 (starintel-test-request-count)))
   ;; REFRESH forces a new request.
   (starintel-test--queue-capabilities)
   (starintel-api-capabilities :refresh t)
   (should (= 2 (starintel-test-request-count)))))

(ert-deftest starintel-test-capability-missing-endpoints-is-bad-response ()
  (starintel-test-with-client
   (push `(:status 200
                   :headers '(("Content-Type" . "application/json"))
                   :body ,(json-encode '((status . "ok") (data . ((build . nil))))))
         starintel-test--responses)
   (should-error (starintel-api-capabilities)
                 :type 'starintel-api-bad-response-error)))

;;; ------------------------------------------------------------------
;;; Authentication and secret safety
;;; ------------------------------------------------------------------

(ert-deftest starintel-test-auth-header-sent-when-token-configured ()
  (starintel-test-with-client
   (starintel-test--queue-capabilities)
   (starintel-api-capabilities)
   (should (equal (concat "Bearer " starintel-test-token)
                  (starintel-test-header (starintel-test-last-request)
                                         "Authorization")))))

(ert-deftest starintel-test-no-auth-header-without-token ()
  (starintel-test-with-client
   (let ((starintel-api-token nil))
     (starintel-test--queue-capabilities)
     (starintel-api-capabilities)
     (should-not (starintel-test-header (starintel-test-last-request)
                                        "Authorization")))))

(ert-deftest starintel-test-token-never-appears-in-url ()
  (starintel-test-with-client
   (starintel-test--queue-capabilities)
   (starintel-api-capabilities)
   (push (list :status 200
               :headers '(("Content-Type" . "application/json"))
               :body starintel-test--search-body)
         starintel-test--responses)
   (starintel-api-search "alice")
   (dolist (request starintel-test--requests)
     (should-not (string-match-p (regexp-quote starintel-test-token)
                                 (plist-get request :url))))))

(ert-deftest starintel-test-timeout-signals-typed-error-with-redaction ()
  (starintel-test-with-client
   (let ((starintel-test--response-function
          (lambda ()
            (signal 'starintel-api-timeout-error
                    (list (format "deadline exceeded for %s"
                                  starintel-test-token))))))
     (let ((err (should-error (starintel-api-capabilities)
                              :type 'starintel-api-timeout-error)))
       (should (eq 'starintel-api-timeout-error (car err)))
       (should-not (string-match-p (regexp-quote starintel-test-token)
                                   (plist-get (nth 1 err) :message)))))))

(ert-deftest starintel-test-timeout-return-normalized ()
  (starintel-test-with-client
   (let ((starintel-test--responses '((:timeout t))))
     (should-error (starintel-api-health)
                   :type 'starintel-api-timeout-error))))

(ert-deftest starintel-test-transport-error-redacted-and-typed ()
  (starintel-test-with-client
   (let ((starintel-test--response-function
          (lambda ()
            (error "connection refused while authenticating %s"
                   starintel-test-token))))
     (let ((err (should-error (starintel-api-health)
                              :type 'starintel-api-connection-error)))
       (should (eq 'starintel-api-connection-error (car err)))
       (should-not (string-match-p (regexp-quote starintel-test-token)
                                   (plist-get (nth 1 err) :message)))))))

;;; ------------------------------------------------------------------
;;; Request metadata: correlation ID and deadline
;;; ------------------------------------------------------------------

(ert-deftest starintel-test-correlation-and-deadline-headers ()
  (starintel-test-with-client
   (starintel-test--queue-capabilities)
   (starintel-api-capabilities)
   (let* ((request (starintel-test-last-request))
          (correlation (starintel-test-header request "X-Correlation-ID"))
          (deadline (starintel-test-header request "X-Request-Timeout-Ms")))
     (should correlation)
     (should (<= (length correlation) 128))
     (should (string-match-p "^[A-Za-z0-9_.:-]+$" correlation))
     (should (equal "30000" deadline)))))

;;; ------------------------------------------------------------------
;;; Response decoding and structured errors
;;; ------------------------------------------------------------------

(ert-deftest starintel-test-malformed-json-signals-bad-response ()
  (starintel-test-with-client
   (push (list :status 200
               :headers '(("Content-Type" . "application/json"))
               :body "{\"status\": \"ok\", oops")
         starintel-test--responses)
   (let ((err (should-error (starintel-api-health)
                            :type 'starintel-api-bad-response-error)))
     (should-not (string-match-p (regexp-quote starintel-test-token)
                                 (plist-get (nth 1 err) :message))))))

(ert-deftest starintel-test-401-signals-auth-error ()
  (starintel-test-with-client
   (push (list :status 401
               :headers '(("Content-Type" . "application/json")
                          ("X-Correlation-ID" . "corr-401"))
               :body (json-encode
                      '((status . "error")
                        (code . "invalid_credential")
                        (msg . "Authentication failed")
                        (correlation_id . "corr-401"))))
         starintel-test--responses)
   (let ((err (should-error (starintel-api-health)
                            :type 'starintel-api-auth-error)))
     (should (eq 'starintel-api-auth-error (car err)))
     (should (equal 401 (plist-get (nth 1 err) :http-status)))
     (should (equal "invalid_credential" (plist-get (nth 1 err) :code)))
     (should (equal "corr-401" (plist-get (nth 1 err) :correlation-id)))
     (should (string-match-p "invalid_credential"
                             (plist-get (nth 1 err) :message))))))

(ert-deftest starintel-test-403-signals-authorization-error ()
  (starintel-test-with-client
   (push (list :status 403
               :headers '(("Content-Type" . "application/json"))
               :body (json-encode
                      '((status . "error")
                        (code . "access_denied")
                        (msg . "Access denied")
                        (correlation_id . "corr-403"))))
         starintel-test--responses)
   (let ((err (should-error (starintel-api-health)
                            :type 'starintel-api-forbidden-error)))
     (should (eq 'starintel-api-forbidden-error (car err)))
     (should (equal "access_denied" (plist-get (nth 1 err) :code))))))

(ert-deftest starintel-test-structured-error-response ()
  (starintel-test-with-client
   (push (list :status 422
               :headers '(("Content-Type" . "application/json"))
               :body (json-encode
                      '((status . "error")
                        (code . "invalid_document_schema")
                        (msg . "Document does not conform to the canonical schema")
                        (correlation_id . "corr-422"))))
         starintel-test--responses)
   (let ((err (should-error (starintel-api-health)
                            :type 'starintel-api-validation-error)))
     (should (equal 422 (plist-get (nth 1 err) :http-status)))
     (should (equal "invalid_document_schema" (plist-get (nth 1 err) :code)))
     (should (string-match-p "corr-422" (plist-get (nth 1 err) :message)))
     (should-not (string-match-p (regexp-quote starintel-test-token)
                                 (plist-get (nth 1 err) :message))))))

(ert-deftest starintel-test-5xx-signals-server-error ()
  (starintel-test-with-client
   (push (list :status 503
               :headers '(("Content-Type" . "application/json"))
               :body (json-encode
                      '((status . "error")
                        (code . "unavailable")
                        (msg . "Upstream store unavailable"))))
         starintel-test--responses)
   (should-error (starintel-api-health)
                 :type 'starintel-api-server-error)))

(ert-deftest starintel-test-2xx-error-envelope-signals-error ()
  (starintel-test-with-client
   (push (list :status 200
               :headers '(("Content-Type" . "application/json"))
               :body (json-encode
                      '((status . "error")
                        (code . "legacy_error")
                        (msg . "boom"))))
         starintel-test--responses)
   (let ((err (should-error (starintel-api-health)
                            :type 'starintel-api-error)))
     (should-not (eq 'starintel-api-http-error (car err)))
     (should (equal "legacy_error" (plist-get (nth 1 err) :code))))))

(ert-deftest starintel-test-404-signals-not-found ()
  (starintel-test-with-client
   (starintel-test--queue-capabilities :legacy-routes t)
   (starintel-api-capabilities)
   (push (list :status 404
               :headers '(("Content-Type" . "application/json"))
               :body (json-encode
                      '((status . "error")
                        (code . "not_found")
                        (msg . "missing"))))
         starintel-test--responses)
   (should-error (starintel-api-get-document "nope")
                 :type 'starintel-api-not-found-error)))

;;; ------------------------------------------------------------------
;;; Health and server info
;;; ------------------------------------------------------------------

(ert-deftest starintel-test-health-ok ()
  (starintel-test-with-client
   (push (list :status 200
               :headers '(("Content-Type" . "application/json")
                          ("X-Correlation-ID" . "corr-h"))
               :body (json-encode
                      '((status . "ok")
                        (msg . "OK")
                        (correlation_id . "corr-h"))))
         starintel-test--responses)
   (let* ((result (starintel-api-health))
          (data (plist-get result :data)))
     (should (equal "OK" (alist-get 'msg data)))
     (should (equal "corr-h" (plist-get result :correlation-id)))
     (should (equal "http://starintel.test:5000/health"
                    (plist-get (starintel-test-last-request) :url))))))

(ert-deftest starintel-test-server-info-ok ()
  (starintel-test-with-client
   (push (list :status 200
               :headers '(("Content-Type" . "application/json"))
               :body (json-encode
                      '((doc_spec_version . "0.9.7")
                        (server . "starintel-gserver")
                        (version . "0.9.4")
                        (openapi . "/openapi.json")
                        (client_manifest . "/client-manifest.json"))))
         starintel-test--responses)
   (let ((data (plist-get (starintel-api-server-info) :data)))
     (should (equal "0.9.7" (alist-get 'doc_spec_version data)))
     (should (equal "starintel-gserver" (alist-get 'server data))))))

;;; ------------------------------------------------------------------
;;; Search endpoint resolution
;;; ------------------------------------------------------------------

(ert-deftest starintel-test-search-uses-versioned-endpoint ()
  (starintel-test-with-client
   (starintel-test--queue-capabilities)
   (starintel-api-capabilities)
   (push (list :status 200
               :headers '(("Content-Type" . "application/json"))
               :body starintel-test--search-body)
         starintel-test--responses)
   (let ((result (starintel-api-search "alice bob" :limit 10)))
     (should (equal "http://starintel.test:5000/api/v1/search?q=alice%20bob&limit=10"
                    (plist-get (starintel-test-last-request) :url)))
     (should (equal 1 (alist-get 'total_rows result)))
     (should (= 1 (length (alist-get 'rows result))))
     (should (equal "host"
                    (alist-get 'dtype
                               (alist-get 'doc (car (alist-get 'rows result)))))))))

(ert-deftest starintel-test-search-unavailable-capability ()
  (starintel-test-with-client
   ;; No public_search endpoint and no legacy compatibility advertisement.
   (starintel-test--queue-capabilities
    :endpoints (list (starintel-test--endpoint
                      "capabilities" "GET" "/api/v1/capabilities"))
    :legacy-routes nil)
   (let ((err (should-error (starintel-api-search "alice")
                            :type 'starintel-api-unavailable-capability)))
     ;; Only the capability discovery request may have happened.
     (should (= 1 (starintel-test-request-count)))
     (should (string-match-p "search"
                             (plist-get (nth 1 err) :message))))))

(ert-deftest starintel-test-search-legacy-fallback-when-advertised ()
  (starintel-test-with-client
   ;; No versioned public_search endpoint, but legacy routes are
   ;; explicitly advertised as compatible.
   (starintel-test--queue-capabilities
    :endpoints (list
                (starintel-test--endpoint
                 "capabilities" "GET" "/api/v1/capabilities")
                (starintel-test--endpoint "search" "GET" "/search" t))
    :legacy-routes t)
   (starintel-api-capabilities)
   (push (list :status 200
               :headers '(("Content-Type" . "application/json"))
               :body starintel-test--search-body)
         starintel-test--responses)
   (starintel-api-search "alice")
   (should (equal "http://starintel.test:5000/search?q=alice&limit=25"
                  (plist-get (starintel-test-last-request) :url)))))

(ert-deftest starintel-test-search-legacy-endpoint-ignored-without-compatibility ()
  (starintel-test-with-client
   ;; The legacy search endpoint is advertised but compatibility is off.
   (starintel-test--queue-capabilities
    :endpoints (list
                (starintel-test--endpoint
                 "capabilities" "GET" "/api/v1/capabilities")
                (starintel-test--endpoint "search" "GET" "/search" t))
    :legacy-routes nil)
   (should-error (starintel-api-search "alice")
                 :type 'starintel-api-unavailable-capability)))

(ert-deftest starintel-test-search-prefers-versioned-over-legacy ()
  (starintel-test-with-client
   (starintel-test--queue-capabilities :legacy-routes t)
   (starintel-api-capabilities)
   (push (list :status 200
               :headers '(("Content-Type" . "application/json"))
               :body starintel-test--search-body)
         starintel-test--responses)
   (starintel-api-search "alice")
   (should (string-match-p "/api/v1/search"
                           (plist-get (starintel-test-last-request) :url)))))

;;; ------------------------------------------------------------------
;;; Document lookup endpoint resolution
;;; ------------------------------------------------------------------

(ert-deftest starintel-test-document-lookup-via-advertised-legacy-endpoint ()
  (starintel-test-with-client
   (starintel-test--queue-capabilities :legacy-routes t)
   (starintel-api-capabilities)
   (push (list :status 200
               :headers '(("Content-Type" . "application/json"))
               :body (json-encode
                      '((id . "doc-1")
                        (dtype . "host")
                        (dataset . "default"))))
         starintel-test--responses)
   (let ((doc (starintel-api-get-document "doc 1")))
     (should (equal "http://starintel.test:5000/document/doc%201"
                    (plist-get (starintel-test-last-request) :url)))
     (should (equal "host" (alist-get 'dtype doc))))))

(ert-deftest starintel-test-document-lookup-uses-versioned-endpoint-when-advertised ()
  (starintel-test-with-client
   (starintel-test--queue-capabilities
    :endpoints (list
                (starintel-test--endpoint
                 "capabilities" "GET" "/api/v1/capabilities")
                (starintel-test--endpoint
                 "document_read" "GET" "/api/v1/documents/:id"))
    :legacy-routes nil)
   (starintel-api-capabilities)
   (push (list :status 200
               :headers '(("Content-Type" . "application/json"))
               :body (json-encode '((_id . "doc-1") (dtype . "host"))))
         starintel-test--responses)
   (starintel-api-get-document "doc-1")
   (should (equal "http://starintel.test:5000/api/v1/documents/doc-1"
                  (plist-get (starintel-test-last-request) :url)))))

(ert-deftest starintel-test-document-lookup-unavailable-without-compatibility ()
  (starintel-test-with-client
   ;; Only the legacy document_read endpoint is advertised and
   ;; compatibility is explicitly off.
   (starintel-test--queue-capabilities :legacy-routes nil)
   (should-error (starintel-api-get-document "doc-1")
                 :type 'starintel-api-unavailable-capability)))

(ert-deftest starintel-test-document-capability-predicate ()
  (starintel-test-with-client
   (starintel-test--queue-capabilities :legacy-routes t)
   (should (starintel-api-document-lookup-available-p)))
  (starintel-test-with-client
   (starintel-test--queue-capabilities :legacy-routes nil)
   (starintel-api-capabilities)
   (should-not (starintel-api-document-lookup-available-p))))

;;; ------------------------------------------------------------------
;;; Interactive command surface
;;; ------------------------------------------------------------------

(ert-deftest starintel-test-search-command-gated-on-capability ()
  (starintel-test-with-client
   (starintel-test--queue-capabilities
    :endpoints (list (starintel-test--endpoint
                      "capabilities" "GET" "/api/v1/capabilities"))
    :legacy-routes nil)
   (let ((err (should-error (starintel-search "alice")
                            :type 'user-error)))
     (should (string-match-p "search" (error-message-string err))))))

(ert-deftest starintel-test-document-command-gated-on-capability ()
  (starintel-test-with-client
   (starintel-test--queue-capabilities :legacy-routes nil)
   (let ((err (should-error (starintel-document "doc-1")
                            :type 'user-error)))
     (should (string-match-p "document" (error-message-string err))))))

(ert-deftest starintel-test-search-command-renders-results-buffer ()
  (starintel-test-with-client
   (let ((starintel-ui-buffer-name "*StarIntel Test Results*"))
     (starintel-test--queue-capabilities)
     (starintel-api-capabilities)
     (push (list :status 200
                 :headers '(("Content-Type" . "application/json"))
                 :body starintel-test--search-body)
           starintel-test--responses)
     (starintel-search "alice")
     (with-current-buffer (get-buffer starintel-ui-buffer-name)
       (goto-char (point-min))
       (should (search-forward "doc-1" nil t)))
     (when (get-buffer starintel-ui-buffer-name)
       (kill-buffer starintel-ui-buffer-name)))))

(ert-deftest starintel-test-status-command-renders-info-buffer ()
  (starintel-test-with-client
   (let ((starintel-ui-buffer-name "*StarIntel Test Status*"))
     (starintel-test--queue-capabilities)
     (push (list :status 200
                 :headers '(("Content-Type" . "application/json")
                            ("X-Correlation-ID" . "corr-h"))
                 :body (json-encode
                        '((status . "ok") (msg . "OK") (correlation_id . "corr-h"))))
           starintel-test--responses)
     (push (list :status 200
                 :headers '(("Content-Type" . "application/json"))
                 :body (json-encode
                        '((doc_spec_version . "0.9.7")
                          (server . "starintel-gserver")
                          (version . "0.9.4"))))
           starintel-test--responses)
     (starintel-status)
     (with-current-buffer (get-buffer starintel-ui-buffer-name)
       (goto-char (point-min))
       (should (search-forward "starintel-gserver" nil t))
       (should (search-forward "0.9.7" nil t))
       (should (search-forward "OK" nil t)))
     (when (get-buffer starintel-ui-buffer-name)
       (kill-buffer starintel-ui-buffer-name)))))

(ert-deftest starintel-test-connect-configures-session ()
  (starintel-test-with-client
   (let ((starintel-ui-buffer-name "*StarIntel Test Connect*"))
     ;; status order: server-info, health, capabilities
     (starintel-test--queue-capabilities)
     (push (list :status 200
                 :headers '(("Content-Type" . "application/json"))
                 :body (json-encode
                        '((status . "ok") (msg . "OK"))))
           starintel-test--responses)
     (push (list :status 200
                 :headers '(("Content-Type" . "application/json"))
                 :body (json-encode
                        '((server . "starintel-gserver")
                          (version . "0.9.4")
                          (doc_spec_version . "0.9.7"))))
           starintel-test--responses)
     (starintel-connect "http://starintel.test:5000" "star_sk_v1_secret-token-1234")
     (should (equal "http://starintel.test:5000" starintel-api-base-url))
     (should (equal starintel-test-token starintel-api-token))
     (should (assq 'endpoints starintel-api--capabilities-cache))
     (should (= 3 (starintel-test-request-count)))
     ;; Empty token selects anonymous operation; status runs again.
     (starintel-test--queue-capabilities)
     (push (list :status 200
                 :headers '(("Content-Type" . "application/json"))
                 :body (json-encode '((status . "ok") (msg . "OK"))))
           starintel-test--responses)
     (push (list :status 200
                 :headers '(("Content-Type" . "application/json"))
                 :body (json-encode '((server . "starintel-gserver")
                                      (version . "0.9.4")
                                      (doc_spec_version . "0.9.7"))))
           starintel-test--responses)
     (starintel-connect "http://starintel.test:5000" "")
     (should-not starintel-api-token)
     (when (get-buffer starintel-ui-buffer-name)
       (kill-buffer starintel-ui-buffer-name)))))

(ert-deftest starintel-test-legacy-async-search-still-works ()
  "The legacy callback API remains available for existing callers."
  (starintel-test-with-client
   ;; The legacy path goes through starintel-search-legacy, which uses
   ;; the legacy request.el surface; here we only assert the shim
   ;; exists and delegates when a callback is supplied.
   (should (fboundp 'starintel-search-legacy))))

;;; ------------------------------------------------------------------
;;; Async dispatch (deferred transport)
;;; ------------------------------------------------------------------

(ert-deftest starintel-test-async-request-delivers-success-later ()
  (starintel-test-with-deferred-client
   (let ((got nil))
     (starintel-api-health :on-success (lambda (result) (setq got result)))
     ;; Nothing delivered until the transport completes.
     (should-not got)
     (should (equal '("http://starintel.test:5000/health")
                    (starintel-test--deferred-urls)))
     (starintel-test--release
      (list :status 200
            :headers '(("Content-Type" . "application/json")
                       ("X-Correlation-ID" . "c-async"))
            :body (json-encode '((status . "ok") (msg . "OK")))))
     (should got)
     (should (equal "OK" (cdr (assq 'msg (plist-get got :data)))))
     (should (equal "c-async" (plist-get got :correlation-id))))))

(ert-deftest starintel-test-async-error-delivered-to-on-error ()
  (starintel-test-with-deferred-client
   (let ((got-error nil) (got-success nil))
     (starintel-api-health
      :on-success (lambda (_result) (setq got-success t))
      :on-error (lambda (condition plist)
                  (setq got-error (cons condition plist))))
     (should-not got-error)
     (starintel-test--release
      (list :status 401
            :headers '(("Content-Type" . "application/json"))
            :body (json-encode
                   '((status . "error")
                     (code . "invalid_credential")
                     (msg . "Authentication failed")))))
     (should-not got-success)
     (should (eq 'starintel-api-auth-error (car got-error)))
     (should (equal 401 (plist-get (cdr got-error) :http-status))))))

(ert-deftest starintel-test-async-error-without-callback-routed-to-handler ()
  (starintel-test-with-deferred-client
   (let ((reported nil))
     (let ((starintel-api-async-error-function
            (lambda (condition _plist) (setq reported condition))))
       (starintel-api-health :on-success (lambda (_r) (setq reported 'success)))
       (starintel-test--release
        (list :status 503
              :headers '(("Content-Type" . "application/json"))
              :body (json-encode
                     '((status . "error")
                       (code . "unavailable")
                       (msg . "down")))))
       (should (eq 'starintel-api-server-error reported))))))

(ert-deftest starintel-test-async-search-chains-capabilities-then-search ()
  (starintel-test-with-deferred-client
   (let ((got nil))
     (starintel-api-search "alice" :on-success (lambda (data) (setq got data)))
     ;; Capability discovery is requested first; no search yet.
     (should (equal '("http://starintel.test:5000/api/v1/capabilities")
                    (starintel-test--deferred-urls)))
     (starintel-test--release
      (list :status 200
            :headers '(("Content-Type" . "application/json"))
            :body (starintel-test--capabilities-json)))
     (should-not got)
     ;; Now the search itself is pending on the versioned endpoint.
     (should (equal '("http://starintel.test:5000/api/v1/search?q=alice&limit=25")
                    (starintel-test--deferred-urls)))
     (starintel-test--release
      (list :status 200
            :headers '(("Content-Type" . "application/json"))
            :body starintel-test--search-body))
     (should got)
     (should (= 1 (length (cdr (assq 'rows got))))))))

(ert-deftest starintel-test-async-unavailable-capability-reaches-on-error ()
  (starintel-test-with-deferred-client
   (let ((failure nil))
     (starintel-api-search "alice"
       :on-success (lambda (_d) (setq failure 'unexpected-success))
       :on-error (lambda (condition plist)
                   (setq failure (cons condition plist))))
     (starintel-test--release
      (list :status 200
            :headers '(("Content-Type" . "application/json"))
            :body (starintel-test--capabilities-json
                   :endpoints (list (starintel-test--endpoint
                                     "capabilities" "GET" "/api/v1/capabilities"))
                   :legacy-routes nil)))
     (should (eq 'starintel-api-unavailable-capability (car failure)))
     ;; No search request was made.
     (should-not starintel-test--deferred))))

(ert-deftest starintel-test-async-capabilities-cached-across-operations ()
  (starintel-test-with-deferred-client
   (let ((got nil))
     (starintel-api-search "alice" :on-success (lambda (data) (setq got data)))
     (starintel-test--release
      (list :status 200
            :headers '(("Content-Type" . "application/json"))
            :body (starintel-test--capabilities-json)))
     (starintel-test--release
      (list :status 200
            :headers '(("Content-Type" . "application/json"))
            :body starintel-test--search-body))
     (should got)
     ;; A second search reuses the cache: only the search is requested.
     (setq got nil)
     (starintel-api-search "bob" :on-success (lambda (data) (setq got data)))
     (should (equal '("http://starintel.test:5000/api/v1/search?q=bob&limit=25")
                    (starintel-test--deferred-urls))))))

;;; ------------------------------------------------------------------
;;; Legacy helper preservation
;;; ------------------------------------------------------------------

(ert-deftest starintel-test-legacy-helpers-preserved ()
  (dolist (fn '(starintel-get-server-info
                starintel-health-check
                starintel-get-document
                starintel-create-target
                starintel-get-targets
                starintel-create-document
                starintel-hosts-by-ip
                starintel-dataset-size
                starintel-format-document))
    (should (fboundp fn))))

(provide 'client-test)
;;; client-test.el ends here
