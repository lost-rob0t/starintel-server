;;; client.el --- StarIntel API Client -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: unseen
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (request "0.3.2") (json "1.5") (starintel-doc "0.7.3"))
;; Keywords: tools, processes
;; URL: https://github.com/nsaspy/starintel

;;; Commentary:

;; A fully-featured Emacs client for the StarIntel server API.
;; Provides functions to interact with all StarIntel HTTP endpoints.

;;; Code:

(require 'request)
(require 'json)
(require 'url-util)
(require 'starintel-doc)

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
  "Timeout for API requests in seconds."
  :type 'integer
  :group 'starintel)

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

(defun starintel-search (query &optional limit bookmark callback)
  "Search documents with QUERY string.
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

;;; Interactive Commands

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
  (starintel-search
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
