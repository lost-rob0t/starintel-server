;;; ui-client.el --- StarIntel UI Client -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: unseen
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (transient "0.3.0") (client "1.0.0") (starintel-doc "0.7.3"))
;; Keywords: tools, processes
;; URL: https://github.com/nsaspy/starintel

;;; Commentary:

;; Modern UI client for StarIntel with transient menus, async operations,
;; and document browsing capabilities.

;;; Code:

(require 'transient)
(require 'client)
(require 'json)
(require 'hl-line)
(require 'starintel-doc)

;;; Customization

(defgroup starintel-ui nil
  "UI settings for StarIntel client."
  :group 'starintel
  :prefix "starintel-ui-")

(defcustom starintel-ui-buffer-name "*StarIntel*"
  "Name of the main StarIntel buffer."
  :type 'string
  :group 'starintel-ui)

(defcustom starintel-ui-auto-refresh t
  "Automatically refresh buffer after operations."
  :type 'boolean
  :group 'starintel-ui)

(defcustom starintel-ui-default-actor "nmap"
  "Default actor for target creation."
  :type 'string
  :group 'starintel-ui)

(defcustom starintel-ui-truncate-length 80
  "Maximum length for truncated fields."
  :type 'integer
  :group 'starintel-ui)

;;; Faces

(defface starintel-ui-header-face
  '((t :inherit font-lock-keyword-face :weight bold :height 1.2))
  "Face for section headers."
  :group 'starintel-ui)

(defface starintel-ui-document-id-face
  '((t :inherit font-lock-constant-face))
  "Face for document IDs."
  :group 'starintel-ui)

(defface starintel-ui-dtype-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for document types."
  :group 'starintel-ui)

(defface starintel-ui-transient-face
  '((t :inherit warning))
  "Face for transient indicators."
  :group 'starintel-ui)

(defface starintel-ui-key-face
  '((t :inherit font-lock-variable-name-face))
  "Face for field keys."
  :group 'starintel-ui)

(defface starintel-ui-value-face
  '((t :inherit default))
  "Face for field values."
  :group 'starintel-ui)

;;; Buffer-Local Variables

(defvar-local starintel-ui--current-view nil
  "Current view type (search, document, targets, etc).")

(defvar-local starintel-ui--current-data nil
  "Current data displayed in buffer.")

(defvar-local starintel-ui--current-query nil
  "Current search query.")

(defvar-local starintel-ui--current-bookmark nil
  "Current search bookmark for pagination.")

;;; Mode Definition

(defvar starintel-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'starintel-ui-refresh)
    (define-key map (kbd "s") 'starintel-ui-search)
    (define-key map (kbd "n") 'starintel-ui-next-page)
    (define-key map (kbd "p") 'starintel-ui-previous-page)
    (define-key map (kbd "RET") 'starintel-ui-view-document-at-point)
    (define-key map (kbd "t") 'starintel-ui-toggle-transient)
    (define-key map (kbd "T") 'starintel-ui-targets-menu)
    (define-key map (kbd "c") 'starintel-ui-create-document-menu)
    (define-key map (kbd "Q") 'starintel-ui-query-menu)
    (define-key map (kbd "?") 'starintel-ui-help)
    map)
  "Keymap for StarIntel UI mode.")

(define-derived-mode starintel-ui-mode special-mode "StarIntel"
  "Major mode for StarIntel UI.

\\{starintel-ui-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (hl-line-mode 1))

;;; Utility Functions

(defun starintel-ui--truncate (str &optional len)
  "Truncate STR to LEN characters (default `starintel-ui-truncate-length')."
  (let ((max-len (or len starintel-ui-truncate-length)))
    (if (> (length str) max-len)
        (concat (substring str 0 (- max-len 3)) "...")
      str)))

(defun starintel-ui--format-timestamp (timestamp)
  "Format TIMESTAMP as human-readable string."
  (if (and timestamp (> timestamp 0))
      (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time timestamp))
    "N/A"))

(defun starintel-ui--get-buffer ()
  "Get or create the StarIntel UI buffer."
  (get-buffer-create starintel-ui-buffer-name))

(defun starintel-ui--insert-header (text)
  "Insert TEXT as a header."
  (insert (propertize text 'face 'starintel-ui-header-face) "\n")
  (insert (propertize (make-string (length text) ?─) 'face 'starintel-ui-header-face) "\n\n"))

(defun starintel-ui--insert-field (key value &optional no-truncate)
  "Insert a field with KEY and VALUE.
If NO-TRUNCATE is non-nil, don't truncate value."
  (insert (propertize (format "%-20s: " key) 'face 'starintel-ui-key-face))
  (let ((val-str (format "%s" (or value "N/A"))))
    (insert (propertize (if no-truncate val-str (starintel-ui--truncate val-str))
                        'face 'starintel-ui-value-face))
    (insert "\n")))

(defun starintel-ui--render-document (doc &optional detailed)
  "Render document DOC.
If DETAILED is non-nil, show all fields."
  (let-alist doc
    (let ((transient-p (starintel-document-transient-p doc)))
      (insert (propertize (format "[%s]" (or .dtype "unknown"))
                          'face 'starintel-ui-dtype-face))
      (when transient-p
        (insert " " (propertize "[TRANSIENT]" 'face 'starintel-ui-transient-face)))
      (insert "\n")

      (when ._id
        (insert (propertize ._id 'face 'starintel-ui-document-id-face
                            'starintel-document-id ._id)
                "\n"))

      (when detailed
        (when .dataset
          (starintel-ui--insert-field "Dataset" .dataset))
        (when .dateAdded
          (starintel-ui--insert-field "Date Added" (starintel-ui--format-timestamp .dateAdded)))
        (when .dateUpdated
          (starintel-ui--insert-field "Date Updated" (starintel-ui--format-timestamp .dateUpdated)))

        ;; Type-specific fields
        (pcase .dtype
          ("target"
           (starintel-ui--insert-field "Actor" .actor)
           (starintel-ui--insert-field "Target" .target)
           (starintel-ui--insert-field "Delay" .delay)
           (starintel-ui--insert-field "Recurring" .recurring))

          ("host"
           (starintel-ui--insert-field "IP" .ip)
           (starintel-ui--insert-field "Hostname" .hostname)
           (starintel-ui--insert-field "OS" .os)
           (when .ports
             (starintel-ui--insert-field "Ports"
               (mapconcat (lambda (p)
                           (format "%s/%s"
                                   (alist-get 'port p)
                                   (alist-get 'name p)))
                         .ports ", "))))

          ("email"
           (starintel-ui--insert-field "User" .user)
           (starintel-ui--insert-field "Domain" .domain)
           (when .password
             (starintel-ui--insert-field "Password" (make-string (length .password) ?*))))

          ("domain"
           (starintel-ui--insert-field "Record" .record)
           (starintel-ui--insert-field "Record Type" .recordType)
           (when .resolvedAddresses
             (starintel-ui--insert-field "Resolved IPs"
               (mapconcat 'identity .resolvedAddresses ", "))))

          ("user"
           (starintel-ui--insert-field "Name" .name)
           (starintel-ui--insert-field "Platform" .platform)
           (starintel-ui--insert-field "URL" .url)
           (when .bio
             (starintel-ui--insert-field "Bio" .bio)))

          ("network"
           (starintel-ui--insert-field "ASN" .asn)
           (starintel-ui--insert-field "Organization" .org)
           (starintel-ui--insert-field "Subnet" .subnet))

          ("url"
           (starintel-ui--insert-field "URL" .url t)
           (starintel-ui--insert-field "Path" .path))

          ("message"
           (starintel-ui--insert-field "User" .user)
           (starintel-ui--insert-field "Platform" .platform)
           (starintel-ui--insert-field "Channel" .channel)
           (starintel-ui--insert-field "Group" .group)
           (when .message
             (starintel-ui--insert-field "Message" .message)))

          ("breach"
           (starintel-ui--insert-field "URL" .url t)
           (starintel-ui--insert-field "Total Records" .total)
           (when .description
             (starintel-ui--insert-field "Description" .description)))))

      (insert "\n"))))

;;; Display Functions

(defun starintel-ui--display (view data &optional query)
  "Display DATA in StarIntel buffer with VIEW type.
QUERY is the optional search query."
  (let ((buffer (starintel-ui--get-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (starintel-ui-mode)
        (setq starintel-ui--current-view view)
        (setq starintel-ui--current-data data)
        (setq starintel-ui--current-query query)

        (pcase view
          ('search
           (starintel-ui--render-search-results data query))
          ('document
           (starintel-ui--render-document-detail data))
          ('targets
           (starintel-ui--render-targets data))
          ('query-results
           (starintel-ui--render-query-results data)))

        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun starintel-ui--render-search-results (data query)
  "Render search results DATA for QUERY."
  (starintel-ui--insert-header (format "Search Results: \"%s\"" query))

  (let-alist data
    (when .bookmark
      (setq starintel-ui--current-bookmark .bookmark)
      (insert (propertize (format "Bookmark: %s\n\n" .bookmark)
                          'face 'font-lock-comment-face)))

    (let ((docs (or .rows .docs data)))
      (if (null docs)
          (insert "No results found.\n")
        (insert (format "Found %d results:\n\n" (length docs)))
        (dolist (item docs)
          (let ((doc (if (alist-get 'doc item)
                        (alist-get 'doc item)
                      item)))
            (starintel-ui--render-document doc t)
            (insert (make-string 80 ?─) "\n\n")))))))

(defun starintel-ui--render-document-detail (doc)
  "Render detailed view of DOC."
  (starintel-ui--insert-header "Document Details")
  (starintel-ui--render-document doc t)
  (insert "\n")
  (insert (propertize "Raw JSON:\n" 'face 'starintel-ui-header-face))
  (insert (json-encode doc)))

(defun starintel-ui--render-targets (targets)
  "Render TARGETS list."
  (starintel-ui--insert-header "Targets")
  (if (null targets)
      (insert "No targets found.\n")
    (insert (format "Total: %d targets\n\n" (length targets)))
    (dolist (target targets)
      (starintel-ui--render-document target t)
      (insert (make-string 80 ?─) "\n\n"))))

(defun starintel-ui--render-query-results (results)
  "Render query RESULTS."
  (starintel-ui--insert-header "Query Results")
  (if (null results)
      (insert "No results found.\n")
    (insert (format "Total: %d documents\n\n" (length results)))
    (dolist (doc results)
      (starintel-ui--render-document doc t)
      (insert (make-string 80 ?─) "\n\n"))))

;;; Interactive Commands

(defun starintel-ui-refresh ()
  "Refresh current view."
  (interactive)
  (message "Refreshing...")
  (pcase starintel-ui--current-view
    ('search
     (when starintel-ui--current-query
       (starintel-ui-search starintel-ui--current-query)))
    ('targets
     (starintel-ui-show-targets starintel-ui-default-actor))
    (_ (message "Nothing to refresh"))))

(defun starintel-ui-search (query)
  "Search for documents matching QUERY."
  (interactive "sSearch query: ")
  (message "Searching for: %s..." query)
  (starintel-search
   query nil nil
   (lambda (data)
     (starintel-ui--display 'search data query)
     (message "Search complete."))))

(defun starintel-ui-next-page ()
  "Load next page of search results."
  (interactive)
  (if (and starintel-ui--current-bookmark starintel-ui--current-query)
      (progn
        (message "Loading next page...")
        (starintel-search
         starintel-ui--current-query nil starintel-ui--current-bookmark
         (lambda (data)
           (starintel-ui--display 'search data starintel-ui--current-query)
           (message "Next page loaded."))))
    (message "No more pages or no active search.")))

(defun starintel-ui-previous-page ()
  "Navigate to previous page (not implemented)."
  (interactive)
  (message "Previous page navigation not yet implemented."))

(defun starintel-ui-view-document-at-point ()
  "View full document at point."
  (interactive)
  (let ((id (get-text-property (point) 'starintel-document-id)))
    (if id
        (progn
          (message "Loading document %s..." id)
          (starintel-get-document
           id
           (lambda (data)
             (starintel-ui--display 'document data)
             (message "Document loaded."))))
      (message "No document at point."))))

(defun starintel-ui-toggle-transient ()
  "Toggle transient flag on document at point."
  (interactive)
  (let ((id (get-text-property (point) 'starintel-document-id)))
    (if id
        (message "Toggling transient flag not yet implemented for existing documents.")
      (message "No document at point."))))

(defun starintel-ui-show-targets (actor)
  "Show targets for ACTOR."
  (interactive
   (list (read-string "Actor: " starintel-ui-default-actor)))
  (message "Loading targets for %s..." actor)
  (starintel-get-targets
   actor
   (lambda (data)
     (starintel-ui--display 'targets data)
     (message "Targets loaded."))))

(defun starintel-ui-help ()
  "Show help for StarIntel UI."
  (interactive)
  (describe-mode))

;;; Transient Menus

;;;; Main Menu

;;;###autoload
(transient-define-prefix starintel-ui-main-menu ()
  "Main menu for StarIntel."
  ["StarIntel"
   ["Search & Query"
    ("s" "Search documents" starintel-ui-search)
    ("Q" "Query menu" starintel-ui-query-menu)]
   ["Create"
    ("t" "Create target" starintel-ui-targets-menu)
    ("c" "Create document" starintel-ui-create-document-menu)]
   ["View"
    ("T" "Show targets" starintel-ui-show-targets)
    ("i" "Server info" starintel-get-server-info)
    ("h" "Health check" starintel-health-check)]
   ["Quit"
    ("q" "Quit" transient-quit-one)]])

;;;; Target Creation Menu

(transient-define-argument starintel-ui--target-actor ()
  :description "Actor"
  :class 'transient-option
  :key "a"
  :argument "--actor="
  :choices '("nmap" "subfinder" "httpx" "masscan" "nuclei" "amass"))

(transient-define-argument starintel-ui--target-address ()
  :description "Target address"
  :class 'transient-option
  :key "t"
  :argument "--target=")

(transient-define-argument starintel-ui--target-delay ()
  :description "Delay (seconds)"
  :class 'transient-option
  :key "d"
  :argument "--delay="
  :reader 'transient-read-number-N+)

(transient-define-argument starintel-ui--target-recurring ()
  :description "Recurring"
  :class 'transient-switch
  :key "r"
  :argument "--recurring")

(transient-define-argument starintel-ui--target-transient ()
  :description "Transient (don't persist)"
  :class 'transient-switch
  :key "T"
  :argument "--transient")

(defun starintel-ui--parse-target-args (args)
  "Parse target creation ARGS into data structure."
  (let ((actor (transient-arg-value "--actor=" args))
        (target (transient-arg-value "--target=" args))
        (delay (transient-arg-value "--delay=" args))
        (recurring (transient-arg-value "--recurring" args))
        (transient (transient-arg-value "--transient" args)))
    (list actor
          (starintel-make-target
           actor
           target
           (when delay (string-to-number delay))
           recurring
           transient))))

(transient-define-suffix starintel-ui--create-target (args)
  "Create target with ARGS."
  :description "Create target"
  (interactive (list (transient-args transient-current-command)))
  (let* ((parsed (starintel-ui--parse-target-args args))
         (actor (car parsed))
         (target-data (cadr parsed)))
    (if (and actor (alist-get 'target target-data))
        (progn
          (message "Creating target for %s: %s..." actor (alist-get 'target target-data))
          (starintel-create-target
           actor target-data
           (lambda (data)
             (message "Target created: %s" (alist-get '_id data))
             (when starintel-ui-auto-refresh
               (starintel-ui-show-targets actor)))))
      (user-error "Actor and target address are required!"))))

;;;###autoload
(transient-define-prefix starintel-ui-targets-menu ()
  "Menu for target operations."
  ["Target Configuration"
   (starintel-ui--target-actor)
   (starintel-ui--target-address)
   (starintel-ui--target-delay)
   (starintel-ui--target-recurring)
   (starintel-ui--target-transient)]
  ["Actions"
   ("c" "Create target" starintel-ui--create-target)
   ("v" "View targets" starintel-ui-show-targets)
   ("q" "Quit" transient-quit-one)])

;;;; Document Creation Menu

(transient-define-argument starintel-ui--doc-type ()
  :description "Document type"
  :class 'transient-option
  :key "t"
  :argument "--dtype="
  :choices '("host" "email" "domain" "user" "network" "url" "message" "breach"))

(transient-define-argument starintel-ui--doc-dataset ()
  :description "Dataset"
  :class 'transient-option
  :key "d"
  :argument "--dataset=")

(transient-define-argument starintel-ui--doc-transient ()
  :description "Transient"
  :class 'transient-switch
  :key "T"
  :argument "--transient")


(defun starintel-ui--create-document-interactive (args)
  "Interactively create document with base ARGS."
  (let ((dtype (transient-arg-value "--dtype=" args))
        (dataset (or (transient-arg-value "--dataset=" args) "default"))
        (transient-flag (transient-arg-value "--transient" args))
        (timestamp (round (time-to-seconds (current-time)))))
    (unless dtype
      (user-error "Document type is required!"))

    ;; Create document using spec classes
    (let ((doc-obj
           (pcase dtype
             ("host"
              (host :dtype "host"
                    :dataset dataset
                    :date-added timestamp
                    :date-updated 0
                    :hostname (read-string "Hostname: ")
                    :ip (read-string "IP address: ")
                    :os (read-string "OS (optional): ")
                    :ports nil))

             ("email"
              (email :dtype "email"
                     :dataset dataset
                     :date-added timestamp
                     :date-updated 0
                     :email-username (read-string "Email user: ")
                     :email-domain (read-string "Email domain: ")
                     :email-password (read-string "Password (optional): ")
                     :data-breach nil))

             ("domain"
              (domain :dtype "domain"
                      :dataset dataset
                      :date-added timestamp
                      :date-updated 0
                      :record (read-string "Domain record: ")
                      :record-type (read-string "Record type (A/AAAA/CNAME/etc): ")
                      :resolved-addresses nil))

             ("user"
              (username :dtype "user"
                        :dataset dataset
                        :date-added timestamp
                        :date-updated 0
                        :username (read-string "Username: ")
                        :platform (read-string "Platform: ")
                        :url (read-string "URL (optional): ")
                        :bio (read-string "Bio (optional): ")
                        :misc nil))

             ("url"
              (url :dtype "url"
                   :dataset dataset
                   :date-added timestamp
                   :date-updated 0
                   :url (read-string "URL: ")
                   :path ""
                   :content ""))

             (_ (user-error "Interactive creation for %s not implemented" dtype)))))

      ;; Use spec's to-json method
      (let ((data (starintel-doc-to-json doc-obj)))
        (when transient-flag
          (push '(transient . t) data))

        (message "Creating %s document..." dtype)
        (starintel-create-document
         dtype data
         (lambda (response)
           (message "Document created: %s" (alist-get '_id response))))))))

(transient-define-suffix starintel-ui--create-doc (args)
  "Create document with ARGS."
  :description "Create document"
  (interactive (list (transient-args transient-current-command)))
  (starintel-ui--create-document-interactive args))

;;;###autoload
(transient-define-prefix starintel-ui-create-document-menu ()
  "Menu for document creation."
  ["Document Configuration"
   (starintel-ui--doc-type)
   (starintel-ui--doc-dataset)
   (starintel-ui--doc-transient)]
  ["Actions"
   ("c" "Create document" starintel-ui--create-doc)
   ("q" "Quit" transient-quit-one)])

;;;; Query Menu

(transient-define-argument starintel-ui--query-limit ()
  :description "Result limit"
  :class 'transient-option
  :key "l"
  :argument "--limit="
  :reader 'transient-read-number-N+)

;;;###autoload
(transient-define-prefix starintel-ui-query-menu ()
  "Menu for querying documents."
  ["Query Options"
   (starintel-ui--query-limit)]
  ["Host Queries"
   ("hi" "By IP" starintel-ui-query-hosts-by-ip)
   ("hp" "By port" starintel-ui-query-hosts-by-port)
   ("hs" "By service" starintel-ui-query-hosts-by-service)]
  ["Email Queries"
   ("ee" "By email" starintel-ui-query-emails-by-email)
   ("ed" "By domain" starintel-ui-query-emails-by-domain)
   ("ep" "With password" starintel-ui-query-emails-with-password)]
  ["Domain Queries"
   ("dr" "By record" starintel-ui-query-domains-by-record)
   ("di" "By IP" starintel-ui-query-domains-by-ip)]
  ["User Queries"
   ("un" "By name" starintel-ui-query-users-by-name)
   ("up" "By platform" starintel-ui-query-users-by-platform)]
  ["Other"
   ("q" "Quit" transient-quit-one)])

;; Query command implementations

(defun starintel-ui-query-hosts-by-ip (ip &optional args)
  "Query hosts by IP address with ARGS."
  (interactive (list (read-string "IP address: ")
                     (transient-args 'starintel-ui-query-menu)))
  (let ((limit (or (transient-arg-value "--limit=" args) starintel-default-limit)))
    (message "Querying hosts by IP %s..." ip)
    (starintel-hosts-by-ip
     ip (string-to-number limit)
     (lambda (data)
       (starintel-ui--display 'query-results data)
       (message "Query complete: %d results" (length data))))))

(defun starintel-ui-query-hosts-by-port (port &optional args)
  "Query hosts by PORT with ARGS."
  (interactive (list (read-number "Port: ")
                     (transient-args 'starintel-ui-query-menu)))
  (let ((limit (or (transient-arg-value "--limit=" args) starintel-default-limit)))
    (message "Querying hosts by port %s..." port)
    (starintel-hosts-by-port
     port (string-to-number limit)
     (lambda (data)
       (starintel-ui--display 'query-results data)
       (message "Query complete: %d results" (length data))))))

(defun starintel-ui-query-hosts-by-service (service &optional args)
  "Query hosts by SERVICE with ARGS."
  (interactive (list (read-string "Service: ")
                     (transient-args 'starintel-ui-query-menu)))
  (let ((limit (or (transient-arg-value "--limit=" args) starintel-default-limit)))
    (message "Querying hosts by service %s..." service)
    (starintel-hosts-by-service
     service (string-to-number limit)
     (lambda (data)
       (starintel-ui--display 'query-results data)
       (message "Query complete: %d results" (length data))))))

(defun starintel-ui-query-emails-by-email (email &optional args)
  "Query emails by EMAIL address with ARGS."
  (interactive (list (read-string "Email: ")
                     (transient-args 'starintel-ui-query-menu)))
  (let ((limit (or (transient-arg-value "--limit=" args) starintel-default-limit)))
    (message "Querying emails by %s..." email)
    (starintel-emails-by-email
     email (string-to-number limit)
     (lambda (data)
       (starintel-ui--display 'query-results data)
       (message "Query complete: %d results" (length data))))))

(defun starintel-ui-query-emails-by-domain (domain &optional args)
  "Query emails by DOMAIN with ARGS."
  (interactive (list (read-string "Domain: ")
                     (transient-args 'starintel-ui-query-menu)))
  (let ((limit (or (transient-arg-value "--limit=" args) starintel-default-limit)))
    (message "Querying emails by domain %s..." domain)
    (starintel-emails-by-domain
     domain (string-to-number limit)
     (lambda (data)
       (starintel-ui--display 'query-results data)
       (message "Query complete: %d results" (length data))))))

(defun starintel-ui-query-emails-with-password (&optional args)
  "Query emails with passwords with ARGS."
  (interactive (list (transient-args 'starintel-ui-query-menu)))
  (let ((limit (or (transient-arg-value "--limit=" args) starintel-default-limit)))
    (message "Querying emails with passwords...")
    (starintel-emails-with-password
     (string-to-number limit)
     (lambda (data)
       (starintel-ui--display 'query-results data)
       (message "Query complete: %d results" (length data))))))

(defun starintel-ui-query-domains-by-record (record &optional args)
  "Query domains by RECORD with ARGS."
  (interactive (list (read-string "Domain record: ")
                     (transient-args 'starintel-ui-query-menu)))
  (let ((limit (or (transient-arg-value "--limit=" args) starintel-default-limit)))
    (message "Querying domains by record %s..." record)
    (starintel-domains-by-record
     record (string-to-number limit)
     (lambda (data)
       (starintel-ui--display 'query-results data)
       (message "Query complete: %d results" (length data))))))

(defun starintel-ui-query-domains-by-ip (ip &optional args)
  "Query domains by resolved IP with ARGS."
  (interactive (list (read-string "IP address: ")
                     (transient-args 'starintel-ui-query-menu)))
  (let ((limit (or (transient-arg-value "--limit=" args) starintel-default-limit)))
    (message "Querying domains by IP %s..." ip)
    (starintel-domains-by-resolved-address
     ip (string-to-number limit)
     (lambda (data)
       (starintel-ui--display 'query-results data)
       (message "Query complete: %d results" (length data))))))

(defun starintel-ui-query-users-by-name (name &optional args)
  "Query users by NAME with ARGS."
  (interactive (list (read-string "Username: ")
                     (transient-args 'starintel-ui-query-menu)))
  (let ((limit (or (transient-arg-value "--limit=" args) starintel-default-limit)))
    (message "Querying users by name %s..." name)
    (starintel-users-by-name
     name (string-to-number limit)
     (lambda (data)
       (starintel-ui--display 'query-results data)
       (message "Query complete: %d results" (length data))))))

(defun starintel-ui-query-users-by-platform (platform &optional args)
  "Query users by PLATFORM with ARGS."
  (interactive (list (read-string "Platform: ")
                     (transient-args 'starintel-ui-query-menu)))
  (let ((limit (or (transient-arg-value "--limit=" args) starintel-default-limit)))
    (message "Querying users by platform %s..." platform)
    (starintel-users-by-platform
     platform (string-to-number limit)
     (lambda (data)
       (starintel-ui--display 'query-results data)
       (message "Query complete: %d results" (length data))))))

;;; Entry Point

;;;###autoload
(defun starintel ()
  "Launch StarIntel UI."
  (interactive)
  (starintel-ui-main-menu))

(provide 'ui-client)
;;; ui-client.el ends here
