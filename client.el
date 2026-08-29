;;; client.el --- StarIntel API Client -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: nsaspy
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, processes
;; URL: https://github.com/lost-rob0t/starintel-server

;;; Commentary:

;; An Emacs client for the StarIntel server.
;;
;; The client is an adapter over the StarIntel public HTTP contract:
;;
;; - `starintel-connect' configures the server base URL and bearer
;;   credential, discovers `GET /api/v1/capabilities', and verifies
;;   health.  Interactive commands: `starintel-connect',
;;   `starintel-status', `starintel-search', `starintel-document'.
;;
;; - The `starintel-api-*' layer is a fully asynchronous pure
;;   transport/contract layer and returns plain data.  Every operation
;;   accepts `:on-success'/`:on-error' callbacks and dispatches without
;;   blocking Emacs; without callbacks the same operations run through
;;   a bounded synchronous facade for scripts and tests.  The layer
;;   performs capability discovery, bearer authentication through
;;   headers only, explicit request deadlines (X-Request-Timeout-Ms
;;   plus local enforcement), correlation IDs, and secret-safe typed
;;   errors.  Later integrations (Hackmode, Nyxt bridges) should
;;   consume these functions directly.
;;
;; - The `starintel-ui-*' layer renders results into the *StarIntel*
;;   buffer.  Capability discovery decides which interactive commands
;;   are usable; unsupported operations fail fast with a clear message
;;   instead of probing routes.
;;
;; - Versioned endpoints advertised by the server are preferred.  Legacy
;;   routes are only used when the server capability document
;;   explicitly advertises `compatibility.legacy_routes'.
;;
;; The legacy callback API (starintel-get-document, starintel-search
;; with a CALLBACK argument, the view-query helpers, and
;; starintel-create-target/-document) is preserved for compatibility
;; and still requires the third-party `request' package at runtime.
;; The legacy document-construction helpers additionally require the
;; `starintel-doc' library.  The modern layer needs neither.

;;; Code:

(require 'json)
(require 'url)
(require 'url-util)
(require 'let-alist)
(require 'cl-lib)
;; Legacy dependencies are soft: the modern contract layer is
;; self-contained on top of built-in libraries.
(require 'request nil t)
(require 'starintel-doc nil t)

;;; Customization

(defgroup starintel nil
  "StarIntel API client for Emacs."
  :group 'tools
  :prefix "starintel-")

(defcustom starintel-host "localhost"
  "StarIntel server hostname."
  :type 'string
  :group 'starintel)

(defcustom starintel-port 5000
  "StarIntel server port."
  :type 'integer
  :group 'starintel)

(defcustom starintel-scheme "http"
  "StarIntel server scheme (http or https)."
  :type 'string
  :group 'starintel)

(defcustom starintel-default-limit 50
  "Default limit for query results."
  :type 'integer
  :group 'starintel)

(defcustom starintel-request-timeout 30
  "Timeout for legacy API requests in seconds."
  :type 'integer
  :group 'starintel)

;;; ------------------------------------------------------------------
;;; Modern contract layer (API v1)
;;; ------------------------------------------------------------------

(defgroup starintel-api nil
  "Capability-discovered StarIntel HTTP contract for Emacs."
  :group 'starintel
  :prefix "starintel-api-")

(defcustom starintel-api-base-url nil
  "Base URL of the StarIntel server, e.g. \"http://127.0.0.1:5000\".
When nil it is derived from the legacy `starintel-host' and
`starintel-port' settings."
  :type '(choice (const :tag "Derive from legacy host settings" nil)
                 string)
  :group 'starintel-api)

(defvar starintel-api-token nil
  "Session bearer token for the StarIntel API.
Set it with `starintel-connect' (or `setq'); it is never persisted
through Customize and never embedded in URLs, buffers, or errors.")

(defcustom starintel-api-token-function nil
  "Function returning the bearer token for each request, or nil.
Use this to fetch credentials from `auth-source' or another secret
provider.  The return value is used exactly like `starintel-api-token'."
  :type '(choice (const nil) function)
  :group 'starintel-api)

(defcustom starintel-api-timeout-ms 30000
  "Per-request deadline in milliseconds.
Sent as X-Request-Timeout-Ms and enforced locally by the transport."
  :type 'natnum
  :group 'starintel-api)

(defcustom starintel-api-user-agent "starintel-emacs-client/2.0"
  "User-Agent header for modern contract requests."
  :type 'string
  :group 'starintel-api)

(defvar starintel-api-transport-function #'starintel-api-url-transport
  "Async transport function used by the modern contract layer.
Called as (METHOD URL HEADERS BODY TIMEOUT-MS CALLBACK).  It must
arrange for CALLBACK to be funcalled exactly once with a response
plist: (:status N :headers ALIST :body STRING) once the HTTP exchange
finished, (:timeout t [:error TEXT]) when the deadline expired, or
(:error TEXT) for connection-level failures.  The callback may run
synchronously (before the transport returns) or asynchronously;
transports must not signal across the boundary.  Tests substitute a
fake transport here.")

(defcustom starintel-api-async-error-function #'starintel-api-message-error
  "Handler for async StarIntel errors that have no ON-ERROR callback.
Called with the typed error condition symbol and its plist.  The
default messages the redacted error without interrupting Emacs."
  :type 'function
  :group 'starintel-api)

;;;; Typed errors

(define-error 'starintel-api-error "StarIntel API error")
(define-error 'starintel-api-transport-error "StarIntel transport failure"
  'starintel-api-error)
(define-error 'starintel-api-connection-error "StarIntel connection failure"
  'starintel-api-transport-error)
(define-error 'starintel-api-timeout-error "StarIntel request deadline exceeded"
  'starintel-api-transport-error)
(define-error 'starintel-api-bad-response-error "Malformed StarIntel server response"
  'starintel-api-error)
(define-error 'starintel-api-unavailable-capability "StarIntel capability not advertised"
  'starintel-api-error)
(define-error 'starintel-api-http-error "StarIntel HTTP error response"
  'starintel-api-error)
(define-error 'starintel-api-auth-error "StarIntel authentication failed"
  'starintel-api-http-error)
(define-error 'starintel-api-forbidden-error "StarIntel authorization denied"
  'starintel-api-http-error)
(define-error 'starintel-api-not-found-error "StarIntel resource not found"
  'starintel-api-http-error)
(define-error 'starintel-api-validation-error "StarIntel rejected request input"
  'starintel-api-http-error)
(define-error 'starintel-api-server-error "StarIntel server error"
  'starintel-api-http-error)

(defun starintel-api--token ()
  "Resolve the current bearer token, or nil.
The token is only ever used to build the Authorization header and to
scrub error text."
  (let ((token (or (and (stringp starintel-api-token)
                        (not (string= starintel-api-token ""))
                        starintel-api-token)
                   (and (functionp starintel-api-token-function)
                        (ignore-errors
                          (let ((value (funcall starintel-api-token-function)))
                            (and (stringp value)
                                 (not (string= value ""))
                                 value)))))))
    token))

(defun starintel-api--redact (string)
  "Return STRING with bearer credentials removed."
  (let ((result (if (stringp string) string (format "%s" string)))
        (token (starintel-api--token)))
    (when (and token (not (string= token "")))
      (setq result
            (replace-regexp-in-string (regexp-quote token) "REDACTED"
                                      result t t)))
    (setq result
          (replace-regexp-in-string "Bearer[ \t]+[A-Za-z0-9._~+/=-]+"
                                    "Bearer REDACTED" result t t))
    result))

(defun starintel-api--error-text (err)
  "Return a redacted description of a raw signaled error ERR."
  (starintel-api--redact
   (condition-case nil
       (error-message-string err)
     (error (format "%S" err)))))

(defun starintel-api--signal (condition plist)
  "Signal CONDITION with PLIST after redacting its :message."
  (let ((message (plist-get plist :message)))
    (when message
      (setq plist (plist-put plist :message (starintel-api--redact message)))))
  (signal condition (list plist)))

(defun starintel-api-error-message (err)
  "Return the redacted message of a starintel-api error ERR.
ERR is the condition data as captured by `condition-case' or
`should-error': (SYMBOL . DATA)."
  (let ((data (cdr err)))
    (cond
     ((and (listp data) data (plist-member (car data) :message))
      (plist-get (car data) :message))
     ((stringp data) (starintel-api--redact data))
     (t (starintel-api--redact (format "%S" data))))))

;;;; URL construction

(defun starintel-api--base-url ()
  "Return the normalized server base URL."
  (let ((url (or starintel-api-base-url (starintel--base-url))))
    (if (and (stringp url) (string-match-p "\\`https?://" url))
        (replace-regexp-in-string "/+\\'" "" url)
      (starintel-api--signal
       'starintel-api-error
       `(:message ,(format "Invalid StarIntel server base URL: %S" url))))))

(defun starintel-api--url (path &optional query)
  "Build the absolute URL for PATH with QUERY parameter alist."
  (let ((url (concat (starintel-api--base-url) path)))
    (when query
      (setq url
            (concat url "?"
                    (mapconcat
                     (lambda (pair)
                       (format "%s=%s"
                               (url-hexify-string (symbol-name (car pair)))
                               (url-hexify-string (format "%s" (cdr pair)))))
                     query "&"))))
    url))

(defun starintel-api--expand-path (path path-params)
  "Substitute :name parameters in advertised PATH from PATH-PARAMS.
Values are URL-encoded; unmatched :name segments are left untouched."
  (let ((segments (split-string path "/" t)))
    (concat (and (string-prefix-p "/" path) "/")
            (mapconcat
             (lambda (segment)
               (if (string-prefix-p ":" segment)
                   (let ((cell (assoc-string (substring segment 1)
                                             path-params t)))
                     (if cell
                         (url-hexify-string (format "%s" (cdr cell)))
                       segment))
                 segment))
             segments "/"))))

;;;; Request plumbing

(defun starintel-api--new-correlation-id ()
  "Return a fresh correlation ID the server contract accepts.
Correlation IDs are bounded to 1..128 chars of [A-Za-z0-9_.:-]."
  (format "emacs-%d-%06d" (floor (float-time)) (random 999999)))

(defun starintel-api--auth-headers ()
  "Return the Authorization header when a token is configured."
  (let ((token (starintel-api--token)))
    (when token
      `(("Authorization" . ,(concat "Bearer " token))))))

(defun starintel-api--call-transport (method url headers body timeout-ms callback)
  "Invoke the configured async transport for one exchange.
The transport may complete CALLBACK synchronously (before returning)
or asynchronously.  A transport that signals has its failure
converted into an error response so nothing escapes the boundary:
`starintel-api-timeout-error' becomes (:timeout t ...) and any other
error becomes (:error TEXT).  Responses completed while the transport
is still on the stack are delivered only after it returns, so errors
raised by CALLBACK are never mistaken for transport failures.
Returns non-nil when CALLBACK already ran."
  (let* ((ran nil)
         (completed nil)
         (pending nil)
         (oneway (lambda (response)
                   (unless ran
                     (setq ran t)
                     (if completed
                         (funcall callback response)
                       (setq pending response))))))
    (condition-case err
        (funcall starintel-api-transport-function
                 method url headers body timeout-ms oneway)
      (starintel-api-timeout-error
       (unless ran
         (setq ran t)
         (setq pending (list :timeout t
                             :error (starintel-api--error-text err)))))
      (error
       (unless ran
         (setq ran t)
         (setq pending (list :error (starintel-api--error-text err))))))
    (setq completed t)
    (when pending
      (funcall callback pending))
    ran))

(defun starintel-api--sync (timeout-ms run)
  "Run RUN, a function of (ON-SUCCESS ON-ERROR), to completion.
This is the synchronous facade over the async machinery: when the
transport completes synchronously the value is returned immediately;
otherwise blocks in `accept-process-output' until the exchange
completes, bounded by TIMEOUT-MS (plus slack).  Returns the success
value, or signals the typed error delivered to ON-ERROR."
  (let* ((done nil)
         (value nil)
         (failure nil)
         (deadline (+ (float-time) (/ (+ timeout-ms 2000) 1000.0))))
    (funcall run
             (lambda (v) (setq done t value v))
             (lambda (condition plist)
               (setq done t failure (cons condition plist))))
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (cond
     (failure (signal (car failure) (list (cdr failure))))
     (done value)
     (t (signal 'starintel-api-timeout-error
                `(:message ,(format "request did not complete within %d ms"
                                    timeout-ms)))))))

(defun starintel-api--request-async (method path query body timeout-ms
                                            on-success on-error)
  "Dispatch one async request; never blocks, never signals.
ON-SUCCESS receives the result plist; ON-ERROR (when present)
receives a typed condition symbol and its plist."
  (let* ((timeout (or timeout-ms starintel-api-timeout-ms))
         (correlation-id (starintel-api--new-correlation-id))
         (url (starintel-api--url path query))
         (headers `(("Accept" . "application/json")
                    ("User-Agent" . ,starintel-api-user-agent)
                    ("X-Correlation-ID" . ,correlation-id)
                    ("X-Request-Timeout-Ms" . ,(number-to-string timeout))
                    ,@(when body '(("Content-Type" . "application/json")))
                    ,@(starintel-api--auth-headers))))
    (starintel-api--call-transport
     method url headers (when body (json-encode body)) timeout
     (lambda (response)
       (starintel-api--finish response on-success on-error method path)))))

(cl-defun starintel-api--request (method path &key query body timeout-ms on-success on-error)
  "Perform one request against the StarIntel contract.
Async when ON-SUCCESS is non-nil: dispatches, returns nil, and later
delivers the result plist to ON-SUCCESS or a typed error to ON-ERROR
(or `starintel-api-async-error-function' when ON-ERROR is nil).
Without ON-SUCCESS, blocks until completion and returns the result
plist, signaling typed errors.

The result plist has the shape (:status :headers :body :data
:correlation-id)."
  (if on-success
      (starintel-api--request-async method path query body timeout-ms
                                    on-success on-error)
    (starintel-api--sync (or timeout-ms starintel-api-timeout-ms)
      (lambda (on-ok on-err)
        (starintel-api--request-async method path query body timeout-ms
                                      on-ok on-err)))))

;;;; Response handling

(defun starintel-api--response-header (headers name)
  "Look up NAME in response HEADERS alist, case-insensitively."
  (cdr (assoc-string name headers t)))

(defun starintel-api--decode-json-safe (text context)
  "Decode JSON TEXT for CONTEXT without signaling.
Returns (:ok DATA) or (:error MESSAGE)."
  (if (string-match-p "\\`[[:space:]]*\\'" (or text ""))
      '(:ok nil)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-false :json-false))
      (condition-case nil
          `(:ok ,(json-read-from-string text))
        (error
         `(:error ,(format "Malformed JSON in %s response: %s"
                           context
                           (substring text 0 (min (length text) 120)))))))))

(defun starintel-api--error-envelope-p (data)
  "Return non-nil when decoded DATA is a StarIntel error envelope."
  (and (listp data)
       (not (vectorp data))
       (equal "error" (cdr (assq 'status data)))))

(defun starintel-api--http-error-condition (status)
  "Map HTTP STATUS to a typed client error condition."
  (cond ((= status 401) 'starintel-api-auth-error)
        ((= status 403) 'starintel-api-forbidden-error)
        ((= status 404) 'starintel-api-not-found-error)
        ((= status 429) 'starintel-api-http-error)
        ((and (>= status 400) (< status 500))
         'starintel-api-validation-error)
        ((>= status 500) 'starintel-api-server-error)
        (t 'starintel-api-http-error)))

(defun starintel-api--error-result (condition message &rest keys)
  "Build an (:error CONDITION PLIST) result with a redacted MESSAGE."
  `(:error ,condition
    ,(apply #'list :message (starintel-api--redact message) keys)))

(defun starintel-api--validate-response (response method path)
  "Validate a transport RESPONSE; never signals.
Returns (:ok RESULT-PLIST) or (:error CONDITION PLIST)."
  (let ((context (format "%s %s" method path)))
    (cond
     ((and (plist-get response :timeout) (not (plist-get response :status)))
      (starintel-api--error-result
       'starintel-api-timeout-error
       (or (plist-get response :error)
           (format "StarIntel request deadline exceeded for %s" context))))
     ((plist-get response :error)
      (starintel-api--error-result
       'starintel-api-connection-error
       (plist-get response :error)))
     ((not (natnump (plist-get response :status)))
      (starintel-api--error-result
       'starintel-api-bad-response-error
       "StarIntel transport returned no HTTP status"))
     (t
      (let* ((status (plist-get response :status))
             (headers (plist-get response :headers))
             (body (or (plist-get response :body) ""))
             (correlation-id (starintel-api--response-header
                              headers "X-Correlation-ID"))
             (decoded (starintel-api--decode-json-safe body context)))
        (cond
         ((plist-get decoded :error)
          (starintel-api--error-result
           'starintel-api-bad-response-error
           (plist-get decoded :error)))
         ((and (<= 200 status) (< status 300))
          (let ((data (plist-get decoded :ok)))
            (if (starintel-api--error-envelope-p data)
                (starintel-api--error-result
                 'starintel-api-error
                 (format "%s returned an error envelope: %s %s"
                         context
                         (or (cdr (assq 'code data)) "error")
                         (or (cdr (assq 'msg data)) ""))
                 :http-status status
                 :code (cdr (assq 'code data))
                 :correlation-id (or correlation-id
                                     (cdr (assq 'correlation_id data))))
               `(:ok (:status ,status
                      :headers ,headers
                      :body ,body
                      :data ,data
                      :correlation-id ,correlation-id)))))
         (t
          (let* ((data (plist-get decoded :ok))
                 (code (cdr (assq 'code data)))
                 (msg (or (cdr (assq 'msg data))
                          (cdr (assq 'message data))
                          (cdr (assq 'detail data))))
                 (corr (or correlation-id (cdr (assq 'correlation_id data)))))
            (starintel-api--error-result
             (starintel-api--http-error-condition status)
             (concat (format "HTTP %d" status)
                     (and code (format " (%s)" code))
                     ": "
                     (or msg (format "%s request rejected" context))
                     (and corr (format " [correlation %s]" corr)))
             :http-status status
             :code code
             :correlation-id corr)))))))))

(defun starintel-api--deliver-error (on-error condition plist)
  "Deliver a typed error to ON-ERROR, or the default async handler."
  (if on-error
      (funcall on-error condition plist)
    (funcall starintel-api-async-error-function condition plist)))

(defun starintel-api--finish (response on-success on-error method path)
  "Validate RESPONSE and deliver it to ON-SUCCESS/ON-ERROR."
  (let ((result (starintel-api--validate-response response method path)))
    (if (plist-get result :ok)
        (funcall on-success (plist-get result :ok))
      (starintel-api--deliver-error
       on-error
       (plist-get result :error)
       (nth 2 result)))))

(defun starintel-api-message-error (condition plist)
  "Default async error handler: message the redacted error."
  (message "StarIntel: %s"
           (starintel-api-error-message (cons condition (list plist)))))

;;;; Default url.el transport

(defun starintel-api--parse-http-headers ()
  "Collect HTTP header lines into an alist; move point to the body."
  (let ((headers nil))
    (catch 'done
      (while t
        (when (eobp) (throw 'done nil))
        (let ((start (line-beginning-position))
              (end (line-end-position)))
          (if (= start end)
              (progn (forward-line 1) (throw 'done nil))
            (let ((line (buffer-substring-no-properties start end)))
              (when (string-match "^\\([^:]+\\):[ \t]*\\(.*\\)" line)
                (push (cons (match-string 1 line)
                            (replace-regexp-in-string "\r\\'" ""
                                                      (match-string 2 line)))
                      headers))
              (forward-line 1))))))
    (nreverse headers)))

(defun starintel-api--parse-http-buffer (buffer)
  "Parse HTTP response BUFFER into (:status :headers :body)."
  (with-current-buffer buffer
    (goto-char (point-min))
    (if (not (re-search-forward "\\`HTTP/[0-9.]+[ \t]+\\([0-9]+\\)" nil t))
        (signal 'starintel-api-connection-error
                '("server response did not contain an HTTP status line"))
      (let* ((status (string-to-number (match-string 1)))
             (headers (starintel-api--parse-http-headers))
             (body (buffer-substring-no-properties (point) (point-max))))
        `(:status ,status
          :headers ,headers
          :body ,(decode-coding-string body 'utf-8))))))

(defun starintel-api--kill-response-buffer (buffer)
  "Silently dispose of a url.el response BUFFER.
The process sentinel and filter are detached before deletion so that
truncated-response parsing inside url.el can neither prompt nor
signal past the transport boundary."
  (when (buffer-live-p buffer)
    (let ((process (get-buffer-process buffer)))
      (when (process-live-p process)
        (set-process-sentinel process #'ignore)
        (set-process-filter process #'ignore)
        (delete-process process)))
    (let ((kill-buffer-query-functions nil)
          (kill-buffer-hook nil))
      (kill-buffer buffer))))

(defun starintel-api-url-transport (method url headers body timeout-ms callback)
  "Default asynchronous transport built on url.el with a hard deadline.
METHOD, URL, HEADERS and BODY make up the request; TIMEOUT-MS bounds
the whole exchange.  Calls CALLBACK exactly once with (:status
:headers :body), (:timeout t :error TEXT), or (:error TEXT); it never
signals across the async boundary."
  (let* ((url-request-method method)
         (url-request-extra-headers headers)
         (url-request-data (and body (encode-coding-string body 'utf-8)))
         (done nil)
         (buffer nil)
         (timer nil)
         (finish
          (lambda (response)
            (when timer (cancel-timer timer))
            (ignore-errors (starintel-api--kill-response-buffer buffer))
            (unless done
              (setq done t)
              (funcall callback response))))
         (report
          (lambda (&rest _)
            (setq buffer (if (buffer-live-p buffer)
                             buffer
                           (current-buffer)))
            (let ((parsed
                   (when (buffer-live-p buffer)
                     (ignore-errors
                       (starintel-api--parse-http-buffer buffer)))))
              (funcall finish
                       (or parsed
                           (list :error
                                 (format "invalid HTTP response from %s %s"
                                         method url))))))))
    (let ((response-buffer
           (condition-case err
               (url-retrieve url report)
             (error
              (funcall finish
                       (list :error
                             (format "url-retrieve failed for %s %s: %s"
                                     method url (error-message-string err))))
              nil))))
      (setq buffer (or buffer response-buffer))
      (when (and (not buffer) (not done))
        (funcall finish (list :error "url-retrieve returned no buffer"))))
    (unless done
      (setq timer
            (run-at-time
             (/ (max timeout-ms 1) 1000.0) nil
             (lambda ()
               (funcall finish
                        (list :timeout t
                              :error (format
                                      "deadline of %d ms exceeded for %s %s"
                                      timeout-ms method url)))))))
    nil))

;;;; Capability discovery

(defconst starintel-api--capabilities-path "/api/v1/capabilities"
  "Server capability discovery path.")

(defvar starintel-api--capabilities-cache nil
  "Cached capability document data from GET /api/v1/capabilities.")

(defun starintel-api-clear-capabilities ()
  "Drop the cached capability document."
  (interactive)
  (setq starintel-api--capabilities-cache nil))

(defun starintel-api--capabilities-extract (result)
  "Extract capability data from a capabilities RESULT plist.
Returns (:ok DATA) or (:error CONDITION PLIST)."
  (let* ((envelope (plist-get result :data))
         (data (cdr (assq 'data envelope))))
    (if (and (listp data) (assq 'endpoints data))
        `(:ok ,data)
      '(:error starintel-api-bad-response-error
        (:message "capabilities response did not advertise any endpoints")))))

(defun starintel-api--capabilities-async (on-success on-error)
  "Fetch, validate and cache capabilities; deliver DATA to ON-SUCCESS."
  (starintel-api--request
   "GET" starintel-api--capabilities-path
   :on-success
   (lambda (result)
     (let ((extracted (starintel-api--capabilities-extract result)))
       (if (plist-get extracted :ok)
           (progn
             (setq starintel-api--capabilities-cache
                   (plist-get extracted :ok))
             (funcall on-success (plist-get extracted :ok)))
         (starintel-api--deliver-error
          on-error
          (plist-get extracted :error)
          (nth 2 extracted)))))
   :on-error on-error))

(cl-defun starintel-api-capabilities (&key refresh on-success on-error)
  "Return the advertised server capability data, caching the result.
With REFRESH non-nil, re-fetch even when a cached document exists.

Async when ON-SUCCESS is given (called with the capability data);
otherwise blocks and returns the data, signaling typed errors."
  (cond
   ((and (not refresh) starintel-api--capabilities-cache)
    (let ((caps starintel-api--capabilities-cache))
      (if on-success (funcall on-success caps) caps)))
   (on-success
    (starintel-api--capabilities-async on-success on-error))
   (t
    (let* ((result (starintel-api--request
                    "GET" starintel-api--capabilities-path))
           (extracted (starintel-api--capabilities-extract result)))
      (if (plist-get extracted :ok)
          (setq starintel-api--capabilities-cache
                (plist-get extracted :ok))
        (starintel-api--signal (plist-get extracted :error)
                               (nth 2 extracted)))))))

(defun starintel-api--endpoints (&optional caps)
  "Return the advertised endpoint list from CAPS (default: cache)."
  (cdr (assq 'endpoints (or caps starintel-api--capabilities-cache))))

(defun starintel-api-endpoint-by-id (id &optional caps)
  "Return the advertised endpoint with ID, or nil."
  (seq-find (lambda (endpoint)
              (equal id (cdr (assq 'id endpoint))))
            (starintel-api--endpoints caps)))

(defun starintel-api--endpoint-legacy-p (endpoint)
  "Return non-nil when ENDPOINT is explicitly marked as legacy."
  (eq t (cdr (assq 'legacy endpoint))))

(defun starintel-api-legacy-compatible-p (&optional caps)
  "Return non-nil when legacy-route compatibility is explicitly advertised."
  (eq t (cdr (assq 'legacy_routes
                   (assq 'compatibility
                         (or caps starintel-api--capabilities-cache))))))

(defun starintel-api-resolve-endpoint (canonical-id &optional legacy-id caps)
  "Resolve the advertised endpoint to use for one operation.
Prefer the non-legacy endpoint CANONICAL-ID.  When it is missing (or
itself legacy-marked), fall back to LEGACY-ID (or CANONICAL-ID) only
if the server advertises compatibility.legacy_routes.  Returns the
endpoint alist or nil when the operation is unavailable."
  (let* ((caps (or caps starintel-api--capabilities-cache))
         (endpoint (starintel-api-endpoint-by-id canonical-id caps)))
    (cond
     ((and endpoint (not (starintel-api--endpoint-legacy-p endpoint)))
      endpoint)
     ((starintel-api-legacy-compatible-p caps)
      (or (and legacy-id (starintel-api-endpoint-by-id legacy-id caps))
          endpoint))
     (t nil))))

(defun starintel-api--unavailable-error (feature canonical-id)
  "Build an (:error ...) result for an unadvertised FEATURE."
  (starintel-api--error-result
   'starintel-api-unavailable-capability
   (format "%s is not advertised by %s: no versioned `%s' endpoint and legacy-route compatibility is not advertised"
           feature (starintel-api--base-url) canonical-id)
   :capability feature))

(defun starintel-api--with-capabilities (on-error ready)
  "Call READY with capabilities, discovering them first when cold.
Discovery failures follow the async error protocol via ON-ERROR."
  (if starintel-api--capabilities-cache
      (funcall ready starintel-api--capabilities-cache)
    (starintel-api-capabilities
     :on-success ready
     :on-error on-error)))

;;;; Contract operations

(cl-defun starintel-api-health (&key on-success on-error)
  "Check process health via the contracted GET /health route.
Async when ON-SUCCESS is given (called with the result plist);
otherwise blocks and returns the result plist
(:status :headers :body :data :correlation-id)."
  (starintel-api--request "GET" "/health"
                          :on-success on-success :on-error on-error))

(cl-defun starintel-api-server-info (&key on-success on-error)
  "Fetch server metadata via the contracted GET / route.
Async when ON-SUCCESS is given (called with the result plist);
otherwise blocks and returns the result plist whose :data holds
server, version and document spec information."
  (starintel-api--request "GET" "/"
                          :on-success on-success :on-error on-error))

(defconst starintel-api--search-canonical-id "public_search"
  "Capability id of the versioned search endpoint.")
(defconst starintel-api--search-legacy-id "search"
  "Capability id of the legacy search endpoint.")

(defun starintel-api-search-available-p ()
  "Return non-nil when the connected server advertises search.
Blocking convenience around capability discovery."
  (and (starintel-api-capabilities)
       (starintel-api-resolve-endpoint
        starintel-api--search-canonical-id
        starintel-api--search-legacy-id)))

(defun starintel-api--search-async (query limit bookmark on-success on-error)
  "Async search core: resolve the endpoint, then fetch results."
  (starintel-api--with-capabilities
   on-error
   (lambda (_caps)
     (let ((endpoint (starintel-api-resolve-endpoint
                      starintel-api--search-canonical-id
                      starintel-api--search-legacy-id)))
       (if (null endpoint)
           (starintel-api--deliver-error
            on-error
            (plist-get (starintel-api--unavailable-error
                        "search" starintel-api--search-canonical-id)
                       :error)
            (nth 2 (starintel-api--unavailable-error
                    "search" starintel-api--search-canonical-id)))
         (let ((params `((q . ,query)
                         (limit . ,(or limit 25)))))
           (when bookmark
             (setq params (append params `((bookmark . ,bookmark)))))
           (starintel-api--request
            "GET"
            (starintel-api--expand-path (cdr (assq 'path endpoint)) nil)
            :query params
            :on-success (lambda (result)
                          (funcall on-success (plist-get result :data)))
            :on-error on-error)))))))

(cl-defun starintel-api-search (query &key limit bookmark on-success on-error)
  "Search documents for QUERY over the capability-resolved endpoint.
LIMIT defaults to 25; BOOKMARK continues a previous result page.

Async when ON-SUCCESS is given (called with the decoded search
document); otherwise blocks and returns it, signaling typed errors."
  (if on-success
      (starintel-api--search-async query limit bookmark on-success on-error)
    (starintel-api--sync starintel-api-timeout-ms
      (lambda (on-ok on-err)
        (starintel-api--search-async query limit bookmark on-ok on-err)))))

(defconst starintel-api--document-read-id "document_read"
  "Capability id of the document lookup endpoint.")

(defun starintel-api-document-lookup-available-p ()
  "Return non-nil when the connected server advertises document lookup.
Blocking convenience around capability discovery."
  (and (starintel-api-capabilities)
       (starintel-api-resolve-endpoint starintel-api--document-read-id)))

(defun starintel-api--document-async (id on-success on-error)
  "Async document lookup core: resolve the endpoint, then fetch."
  (starintel-api--with-capabilities
   on-error
   (lambda (_caps)
     (let ((endpoint (starintel-api-resolve-endpoint
                      starintel-api--document-read-id)))
       (if (null endpoint)
           (let ((result (starintel-api--unavailable-error
                          "document lookup" starintel-api--document-read-id)))
             (starintel-api--deliver-error
              on-error
              (plist-get result :error)
              (nth 2 result)))
         (starintel-api--request
          "GET"
          (starintel-api--expand-path
           (cdr (assq 'path endpoint)) `(("id" . ,id)))
          :on-success (lambda (result)
                        (funcall on-success (plist-get result :data)))
          :on-error on-error))))))

(cl-defun starintel-api-get-document (id &key on-success on-error)
  "Fetch the document with ID over the capability-resolved endpoint.
Async when ON-SUCCESS is given (called with the decoded document
alist); otherwise blocks and returns it, signaling typed errors."
  (if on-success
      (starintel-api--document-async id on-success on-error)
    (starintel-api--sync starintel-api-timeout-ms
      (lambda (on-ok on-err)
        (starintel-api--document-async id on-ok on-err)))))

;;; ------------------------------------------------------------------
;;; Legacy utility functions (compatibility)
;;; ------------------------------------------------------------------

;;; Internal Variables

(defvar starintel--server-info nil
  "Cached server information.")

(defvar starintel--last-error nil
  "Last error from API request.")

;;; Utility Functions

(defun starintel--base-url ()
  "Return the base URL for StarIntel API."
  (format "%s://%s:%d" starintel-scheme starintel-host starintel-port))

(defun starintel--make-url (path)
  "Construct full URL from PATH."
  (concat (starintel--base-url) path))

(defun starintel--encode-params (params)
  "Encode PARAMS as URL query string."
  (when params
    (concat "?"
            (mapconcat
             (lambda (pair)
               (format "%s=%s"
                       (url-hexify-string (symbol-name (car pair)))
                       (url-hexify-string (format "%s" (cdr pair)))))
             params
             "&"))))

(defun starintel--handle-response (response success error)
  "Handle API RESPONSE, calling SUCCESS or ERROR callbacks."
  (let ((status-code (request-response-status-code response))
        (data (request-response-data response)))
    (if (and status-code (>= status-code 200) (< status-code 300))
        (condition-case err
            (let* ((parsed (if (stringp data)
                               (json-read-from-string data)
                             data))
                   ;; Check if this is an error response
                   (is-error (and (listp parsed)
                                  (assoc 'status parsed)
                                  (string= "error" (cdr (assoc 'status parsed))))))
              (if is-error
                  (progn
                    (setq starintel--last-error (cdr (assoc 'msg parsed)))
                    (when error
                      (funcall error (cdr (assoc 'msg parsed)))))
                (when success
                  (funcall success parsed))))
          (error
           (setq starintel--last-error (format "JSON parse error: %s" err))
           (when error
             (funcall error (format "Failed to parse response: %s" err)))))
      (setq starintel--last-error (format "HTTP %s: %s" status-code data))
      (when error
        (funcall error (format "Request failed with status %s" status-code))))))

(defun starintel--request (method path &optional params data success error)
  "Make HTTP request to StarIntel API.
METHOD is the HTTP method (:GET, :POST, etc).
PATH is the API endpoint path.
PARAMS is an alist of query parameters.
DATA is the request body (will be JSON encoded).
SUCCESS is callback for successful response.
ERROR is callback for error response."
  (unless (fboundp 'request)
    (error "Legacy StarIntel functions require the third-party `request' package; the modern starintel-api layer works without it"))
  (let ((url (concat (starintel--make-url path)
                     (starintel--encode-params params))))
    (request url
      :type (substring (symbol-name method) 1)
      :headers '(("Content-Type" . "application/json"))
      :data (when data (json-encode data))
      :parser 'buffer-string
      :timeout starintel-request-timeout
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (starintel--handle-response response success error)))
      :error (cl-function
              (lambda (&key error-thrown response &allow-other-keys)
                (setq starintel--last-error (format "Request error: %s" error-thrown))
                (when error
                  (funcall error (format "Request failed: %s" error-thrown))))))))

;;; Core API Functions

;;;; Server Metadata

(defun starintel-get-server-info (&optional callback)
  "Get server metadata and information.
If CALLBACK is provided, call it with the result."
  (interactive)
  (starintel--request
   :GET "/"
   nil nil
   (lambda (data)
     (setq starintel--server-info data)
     (when (called-interactively-p 'any)
       (message "StarIntel Server v%s (spec v%s)"
                (alist-get 'version data)
                (alist-get 'doc_spec_version data)))
     (when callback
       (funcall callback data)))
   (lambda (err)
     (message "Failed to get server info: %s" err))))

(defun starintel-health-check (&optional callback)
  "Check server health status.
If CALLBACK is provided, call it with the result."
  (interactive)
  (starintel--request
   :GET "/health"
   nil nil
   (lambda (data)
     (when (called-interactively-p 'any)
       (message "Server status: %s" (alist-get 'msg data)))
     (when callback
       (funcall callback data)))
   (lambda (err)
     (message "Health check failed: %s" err))))

;;;; Document Operations

(defun starintel-get-document (id callback)
  "Retrieve document by ID and call CALLBACK with result."
  (starintel--request
   :GET (format "/document/%s" id)
   nil nil
   callback
   (lambda (err)
     (message "Failed to get document %s: %s" id err))))

(defun starintel-search-legacy (query &optional limit bookmark callback)
  "Search documents with QUERY string using the legacy routes.
LIMIT is max results (default 25).
BOOKMARK is pagination token.
CALLBACK is called with search results."
  (let ((params `((q . ,query)
                  (limit . ,(or limit 25)))))
    (when bookmark
      (push `(bookmark . ,bookmark) params))
    (starintel--request
     :GET "/search"
     params nil
     callback
     (lambda (err)
       (message "Search failed: %s" err)))))

;;;; Target Operations

(defun starintel-create-target (actor target-data &optional callback)
  "Create a new target for ACTOR with TARGET-DATA.
ACTOR is one of: nmap, subfinder, httpx, etc.
TARGET-DATA is an alist with target details (target, delay, recurring, transient).
CALLBACK is called with the created target."
  (starintel--request
   :POST (format "/new/target/%s" actor)
   nil target-data
   (or callback
       (lambda (data)
         (message "Target created: %s" (alist-get '_id data))))
   (lambda (err)
     (message "Failed to create target: %s" err))))

(defun starintel-get-targets (actor callback)
  "Get all targets for ACTOR and call CALLBACK with results."
  (starintel--request
   :GET (format "/targets/%s" actor)
   nil nil
   callback
   (lambda (err)
     (message "Failed to get targets: %s" err))))

;;;; Document Creation

(defun starintel-create-document (dtype data &optional callback)
  "Create a new document of type DTYPE with DATA.
DTYPE is the document type (host, email, domain, user, etc).
DATA is an alist with document fields.
CALLBACK is called with the created document."
  (starintel--request
   :POST (format "/new/document/%s" dtype)
   nil data
   (or callback
       (lambda (data)
         (message "Document created: %s" (alist-get '_id data))))
   (lambda (err)
     (message "Failed to create document: %s" err))))

;;;; Host Queries

(defun starintel-hosts-by-ip (ip &optional limit callback)
  "Query hosts by IP address.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/hosts/by-ip"
   `((ip . ,ip)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query hosts by IP: %s" err))))

(defun starintel-hosts-by-port (port &optional limit callback)
  "Query hosts by PORT number.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/hosts/by-port"
   `((port . ,port)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query hosts by port: %s" err))))

(defun starintel-hosts-by-service (service &optional limit callback)
  "Query hosts by SERVICE name.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/hosts/by-service"
   `((service . ,service)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query hosts by service: %s" err))))

;;;; Email Queries

(defun starintel-emails-by-email (email &optional limit callback)
  "Query emails by EMAIL address.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/emails/by-email"
   `((email . ,email)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query emails: %s" err))))

(defun starintel-emails-by-domain (domain &optional limit callback)
  "Query emails by DOMAIN.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/emails/by-domain"
   `((domain . ,domain)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query emails by domain: %s" err))))

(defun starintel-emails-with-password (&optional limit callback)
  "Query emails that have passwords.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/emails/with-password"
   `((limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query emails with passwords: %s" err))))

;;;; Domain Queries

(defun starintel-domains-by-record (record &optional limit callback)
  "Query domains by RECORD name.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/domains/by-record"
   `((record . ,record)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query domains by record: %s" err))))

(defun starintel-domains-by-resolved-address (ip &optional limit callback)
  "Query domains by resolved IP address.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/domains/by-resolved-address"
   `((ip . ,ip)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query domains by resolved address: %s" err))))

;;;; User Queries

(defun starintel-users-by-name (name &optional limit callback)
  "Query users by NAME.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/users/by-name"
   `((name . ,name)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query users by name: %s" err))))

(defun starintel-users-by-platform (platform &optional limit callback)
  "Query users by PLATFORM.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/users/by-platform"
   `((platform . ,platform)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query users by platform: %s" err))))

;;;; Network Queries

(defun starintel-networks-by-asn (asn &optional limit callback)
  "Query networks by ASN number.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/networks/by-asn"
   `((asn . ,asn)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query networks by ASN: %s" err))))

(defun starintel-networks-by-org (org &optional limit callback)
  "Query networks by organization name.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/networks/by-org"
   `((org . ,org)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query networks by org: %s" err))))

;;;; URL Queries

(defun starintel-urls-by-url (url &optional limit callback)
  "Query URLs by exact URL string.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/urls/by-url"
   `((url . ,url)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query URLs: %s" err))))

(defun starintel-urls-by-domain (domain &optional limit callback)
  "Query URLs by DOMAIN.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/urls/by-domain"
   `((domain . ,domain)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query URLs by domain: %s" err))))

;;;; Breach Queries

(defun starintel-breaches-by-size (&optional limit descending callback)
  "Query breaches sorted by size.
LIMIT is max results.
DESCENDING if non-nil sorts largest first.
CALLBACK is called with results."
  (let ((params `((limit . ,(or limit starintel-default-limit)))))
    (when descending
      (push '(descending . "true") params))
    (starintel--request
     :GET "/documents/breaches/by-size"
     params nil
     callback
     (lambda (err)
       (message "Failed to query breaches: %s" err)))))

;;;; Message Queries

(defun starintel-messages-by-user (user &optional limit callback)
  "Query messages by USER.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/messages/by-user"
   `((user . ,user)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query messages by user: %s" err))))

(defun starintel-messages-by-channel (channel group &optional limit callback)
  "Query messages by CHANNEL and GROUP.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/messages/by-channel"
   `((channel . ,channel)
     (group . ,group)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query messages by channel: %s" err))))

(defun starintel-messages-by-platform (platform &optional limit callback)
  "Query messages by PLATFORM.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/messages/by-platform"
   `((platform . ,platform)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query messages by platform: %s" err))))

(defun starintel-message-groups (&optional limit callback)
  "Query all message groups.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/messages/groups"
   `((limit . ,(or limit 100)))
   nil
   callback
   (lambda (err)
     (message "Failed to query message groups: %s" err))))

;;;; Social Post Queries

(defun starintel-social-posts-by-user (user &optional limit callback)
  "Query social media posts by USER.
LIMIT is max results.
CALLBACK is called with results."
  (starintel--request
   :GET "/documents/socialmpost/by-user"
   `((user . ,user)
     (limit . ,(or limit starintel-default-limit)))
   nil
   callback
   (lambda (err)
     (message "Failed to query social posts by user: %s" err))))

;;;; Dataset Operations

(defun starintel-dataset-size (&optional dataset callback)
  "Get size of DATASET (or all datasets if nil).
CALLBACK is called with results."
  (let ((params (when dataset
                  `((dataset . ,dataset)
                    (reduce . "true")))))
    (starintel--request
     :GET "/dataset-size"
     params nil
     callback
     (lambda (err)
       (message "Failed to get dataset size: %s" err)))))

;;; Helper Functions

(defun starintel-make-target (actor target-value &optional delay recurring transient dataset)
  "Create a target data structure following StarIntel spec.
ACTOR is the scanner/tool (nmap, subfinder, etc).
TARGET-VALUE is the target address (IP, domain, etc).
DELAY is scan delay in seconds.
RECURRING if non-nil makes this a recurring target.
TRANSIENT if non-nil marks as transient (not persisted).
DATASET is the dataset name (default: 'default')."
  (unless (fboundp 'starintel-doc-to-json)
    (error "starintel-make-target requires the `starintel-doc' package"))
  (let ((target-obj (target
                     :dtype "target"
                     :dataset (or dataset "default")
                     :date-added (round (time-to-seconds (current-time)))
                     :date-updated 0
                     :actor actor
                     :target target-value
                     :delay (or delay 0)
                     :recurring (if recurring t nil)
                     :options nil)))
    ;; Convert to alist for JSON encoding using spec method
    (let ((data (starintel-doc-to-json target-obj)))
      (when transient
        (push `(transient . t) data))
      data)))

(defun starintel-format-document (doc)
  "Format document DOC for display."
  (let-alist doc
    (format "[%s] %s (added: %s)"
            (or .dtype "unknown")
            (or ._id "no-id")
            (if .dateAdded
                (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time .dateAdded))
              "unknown"))))

(defun starintel-document-transient-p (doc)
  "Return non-nil if document DOC is marked as transient."
  (let ((transient (alist-get 'transient doc)))
    (and transient (not (eq transient :json-false)))))

;;; ------------------------------------------------------------------
;;; Presentation layer
;;; ------------------------------------------------------------------

(defgroup starintel-ui nil
  "Presentation of StarIntel results in Emacs buffers."
  :group 'starintel
  :prefix "starintel-ui-")

(defcustom starintel-ui-buffer-name "*StarIntel*"
  "Name of the buffer used to present StarIntel results."
  :type 'string
  :group 'starintel-ui)

(defface starintel-ui-heading-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for StarIntel result headings."
  :group 'starintel-ui)

(defun starintel-ui--report-error (condition plist)
  "Report an async StarIntel failure to the operator.
When the exchange completed synchronously (fake transports, cached
capabilities) this signals a `user-error'; in a real async callback
Emacs displays the error message from the sentinel instead."
  (user-error "StarIntel: %s"
              (starintel-api-error-message (cons condition (list plist)))))

(defun starintel-ui--buffer ()
  "Return a clean presentation buffer named by `starintel-ui-buffer-name'."
  (let ((buffer (get-buffer-create starintel-ui-buffer-name)))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    buffer))

(defun starintel-ui--insert-heading (title)
  "Insert TITLE as a section heading."
  (insert (propertize title 'face 'starintel-ui-heading-face) "\n")
  (insert (make-string (min 72 (max (length title) 8)) ?-) "\n\n"))

(defun starintel-ui--insert-field (label value)
  "Insert LABEL/VALUE pair on one line."
  (insert (format "%-22s %s\n" label (or value "?"))))

(defun starintel-ui--feature-summary (caps)
  "Return a plain description of advertised features in CAPS."
  (mapconcat
   (lambda (feature)
     (format "%s:%s" (car feature)
             (if (eq t (cdr feature)) "on" "off")))
   (cdr (assq 'features caps)) "  "))

(defun starintel-ui--render-status (info health caps)
  "Render server INFO, HEALTH and CAPS in the StarIntel buffer."
  (let ((info-data (plist-get info :data))
        (health-data (plist-get health :data))
        (search-endpoint (starintel-api-resolve-endpoint
                          starintel-api--search-canonical-id
                          starintel-api--search-legacy-id caps))
        (document-endpoint (starintel-api-resolve-endpoint
                            starintel-api--document-read-id nil caps)))
    (with-current-buffer (starintel-ui--buffer)
      (let ((inhibit-read-only t))
        (starintel-ui--insert-heading
         (format "StarIntel server %s" (starintel-api--base-url)))
        (starintel-ui--insert-field
         "server"
         (cdr (assq 'server info-data)))
        (starintel-ui--insert-field
         "server version"
         (cdr (assq 'version info-data)))
        (starintel-ui--insert-field
         "document spec"
         (cdr (assq 'doc_spec_version info-data)))
        (starintel-ui--insert-field
         "api revision"
         (cdr (assq 'api (assq 'schema_revisions caps))))
        (starintel-ui--insert-field
         "health"
         (if (equal "ok" (cdr (assq 'status health-data)))
             (or (cdr (assq 'msg health-data)) "ok")
           (cdr (assq 'msg health-data))))
        (starintel-ui--insert-field
         "auth modes"
         (mapconcat #'identity
                    (cdr (assq 'modes (assq 'authentication caps))) ", "))
        (insert "\n")
        (starintel-ui--insert-heading "Advertised endpoints in use")
        (starintel-ui--insert-field
         "search"
         (if search-endpoint
             (format "%s%s"
                     (cdr (assq 'path search-endpoint))
                     (if (starintel-api--endpoint-legacy-p search-endpoint)
                         " (legacy compatibility)" ""))
           "not advertised"))
        (starintel-ui--insert-field
         "document lookup"
         (if document-endpoint
             (format "%s%s"
                     (cdr (assq 'path document-endpoint))
                     (if (starintel-api--endpoint-legacy-p document-endpoint)
                         " (legacy compatibility)" ""))
           "not advertised"))
        (starintel-ui--insert-field
         "legacy routes"
         (if (starintel-api-legacy-compatible-p caps)
             "advertised" "not advertised"))
        (insert "\n")
        (starintel-ui--insert-heading "Features")
        (insert (starintel-ui--feature-summary caps) "\n")
        (goto-char (point-min)))
      (pop-to-buffer (current-buffer)))))

(defun starintel-ui--render-search (query data)
  "Render search results DATA for QUERY in the StarIntel buffer."
  (let ((rows (cdr (assq 'rows data)))
        (bookmark (cdr (assq 'bookmark data))))
    (with-current-buffer (starintel-ui--buffer)
      (let ((inhibit-read-only t))
        (starintel-ui--insert-heading (format "Search: %s" query))
        (insert (format "%d result(s)%s\n\n"
                        (length rows)
                        (if bookmark
                            (format "  [bookmark: %s]" bookmark)
                          "")))
        (dolist (row rows)
          (let ((doc (cdr (assq 'doc row))))
            (insert
             (format "[%s] %s\n"
                     (or (cdr (assq 'dtype doc)) "?")
                     (or (cdr (assq '_id doc)) (cdr (assq 'id row)) "?")))
            (when doc
              (starintel-ui--insert-field
               "dataset" (cdr (assq 'dataset doc)))
              (insert "\n"))))
        (goto-char (point-min)))
      (pop-to-buffer (current-buffer)))))

(defun starintel-ui--render-document (id data)
  "Render document DATA for ID in the StarIntel buffer."
  (with-current-buffer (starintel-ui--buffer)
    (let ((inhibit-read-only t))
      (starintel-ui--insert-heading (format "Document: %s" id))
      (insert (json-encode data))
      (ignore-errors (json-pretty-print (point-min) (point-max)))
      (goto-char (point-min)))
    (pop-to-buffer (current-buffer))))

;;; ------------------------------------------------------------------
;;; Interactive commands
;;; ------------------------------------------------------------------

(defun starintel-connect (base-url token)
  "Connect to the StarIntel server at BASE-URL with bearer TOKEN.
TOKEN may be empty for public/anonymous servers.  The token lives
only in the Emacs session: it is sent as an Authorization header and
is never written into URLs, results, or persisted customization.
Performs capability discovery and a health check, then shows the
status buffer."
  (interactive
   (let* ((default (or starintel-api-base-url (starintel--base-url)))
          (url (read-string "StarIntel server URL: " default))
          (token (read-passwd "API token (empty for public/anonymous): ")))
     (list url token)))
  (setq starintel-api-base-url base-url)
  (setq starintel-api-token (and token (not (string= token "")) token))
  (starintel-api-clear-capabilities)
  (starintel-status))

(defun starintel-status ()
  "Show server metadata, health and capability summary.
Renders into the buffer named by `starintel-ui-buffer-name'.  The
exchange is fully asynchronous: the buffer is rendered when the
server answers."
  (interactive)
  (message "StarIntel: contacting %s..." (starintel-api--base-url))
  (starintel-api-server-info
   :on-success
   (lambda (info)
     (starintel-api-health
      :on-success
      (lambda (health)
        (starintel-api-capabilities
         :refresh t
         :on-success
         (lambda (caps)
           (message nil)
           (starintel-ui--render-status info health caps))
         :on-error #'starintel-ui--report-error))
      :on-error #'starintel-ui--report-error))
   :on-error #'starintel-ui--report-error))

(defun starintel-search (query &optional limit bookmark callback)
  "Search StarIntel documents for QUERY and render the results.
Uses the capability-resolved search endpoint: the versioned route
when advertised, the legacy route only when the server explicitly
advertises legacy-route compatibility.  LIMIT bounds the result page.
The exchange is fully asynchronous; results render when the server
answers.  When CALLBACK is non-nil, the legacy asynchronous behavior
is preserved and CALLBACK is invoked with the raw response instead."
  (interactive "sStarIntel search: ")
  (if callback
      (starintel-search-legacy query limit bookmark callback)
    (message "StarIntel: searching for %s..." query)
    (starintel-api-search
     query :limit (or limit 25) :bookmark bookmark
     :on-success (lambda (data)
                   (message nil)
                   (starintel-ui--render-search query data))
     :on-error #'starintel-ui--report-error)))

(defun starintel-document (id)
  "Fetch and render the StarIntel document with ID.
Uses the capability-resolved document endpoint and fails with a
clear message when the server does not advertise document lookup.
The exchange is fully asynchronous."
  (interactive "sStarIntel document ID: ")
  (message "StarIntel: fetching document %s..." id)
  (starintel-api-get-document
   id
   :on-success (lambda (doc)
                 (message nil)
                 (starintel-ui--render-document id doc))
   :on-error #'starintel-ui--report-error))

;;; Legacy Interactive Commands

(defun starintel-test-connection ()
  "Test connection to StarIntel server."
  (interactive)
  (message "Testing connection to %s..." (starintel--base-url))
  (starintel-health-check
   (lambda (data)
     (message "Connection successful! Server: %s" (alist-get 'msg data)))))

(defun starintel-quick-search (query)
  "Perform quick search with QUERY and display results in minibuffer."
  (interactive "sSearch query: ")
  (starintel-search-legacy
   query 10 nil
   (lambda (data)
     (let ((docs (alist-get 'rows data)))
       (if docs
           (message "Found %d results:\n%s"
                    (length docs)
                    (mapconcat
                     (lambda (row)
                       (starintel-format-document (alist-get 'doc row)))
                     (seq-take docs 5)
                     "\n"))
         (message "No results found for: %s" query))))))

(provide 'client)
;;; client.el ends here
