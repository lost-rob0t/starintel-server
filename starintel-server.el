;;; starintel-server.el --- StarIntel server profiles -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: nsaspy
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, processes

;; Commentary:

;; Named server profiles over the StarIntel API layer.
;;
;; A profile gives one StarIntel deployment a stable identity used in
;; `star://' URIs, so references never resolve an ID against the wrong
;; server.  Activating a profile configures the transport layer base
;; URL and credential resolution and clears the capability cache.
;;
;; Secrets never live in plain configuration: prefer :auth-source
;; (:host HOST :user USER) which resolves through `auth-source-search'
;; per request.  A session :token is accepted for transient use and is
;; never persisted through Customize.

;;; Code:

(require 'cl-lib)
(require 'client)

(defgroup starintel-server nil
  "StarIntel server profiles."
  :group 'starintel
  :prefix "starintel-server-")

(defvar starintel-server-current-name nil
  "Name (symbol) of the active server profile, or nil.
When nil the un-profiled API layer settings are in effect and URIs
use the synthetic server name \"default\".")

(defcustom starintel-servers nil
  "Named StarIntel server profiles.
An alist of (NAME . PLIST).  PLIST keys:
  :url         base URL of the deployment (required)
  :token       session bearer token (transient; prefer :auth-source)
  :auth-source plist (:host HOST :user USER) for `auth-source-search'
Example:
  (setq starintel-servers
        (quote ((local  :url \"http://127.0.0.1:5000\")
                (remote :url \"https://si.example.com\"
                        :auth-source (:host \"starintel-remote\" :user \"api\")))))"
  :type '(repeat (cons symbol plist))
  :group 'starintel-server)

(defun starintel-server-profile-names ()
  "Return the profile names, as symbols, in `starintel-servers'."
  (mapcar #'car starintel-servers))

(defun starintel-server--spec (name)
  "Return the cons spec for profile NAME, or signal a user error."
  (or (assq name starintel-servers)
      (user-error "StarIntel: no server profile named `%s'" name)))

(defun starintel-server--plist (name)
  "Return the plist of profile NAME."
  (cdr (starintel-server--spec name)))

(defun starintel-server--activate-token (name)
  "Configure credential resolution for profile NAME.
Tokens from :auth-source are resolved per request through
`auth-source-search'; plain :token values are stored in the session
variable only."
  (let ((plist (starintel-server--plist name)))
    (if (plist-get plist :auth-source)
        (let ((host (plist-get (plist-get plist :auth-source) :host))
              (user (plist-get (plist-get plist :auth-source) :user)))
          (setq starintel-api-token nil)
          (setq starintel-api-token-function
                (lambda ()
                  (let ((entry (car (ignore-errors
                                      (auth-source-search
                                       :max 1 :host host :user user :require '(:secret))))))
                    (when entry
                      (let ((secret (plist-get entry :secret)))
                        (cond
                         ((functionp secret) (funcall secret))
                         (secret secret))))))))
      (setq starintel-api-token-function nil)
      (setq starintel-api-token (plist-get plist :token)))))

(defun starintel-server-activate (name)
  "Activate the server profile NAME.
Sets the API layer base URL and credential resolution, clears the
capability cache, and records NAME as the current server.  Returns
NAME."
  (let ((url (plist-get (starintel-server--plist name) :url)))
    (unless url
      (user-error "StarIntel: profile `%s' has no :url" name))
    (setq starintel-api-base-url url)
    (starintel-server--activate-token name)
    (starintel-api-clear-capabilities)
    (setq starintel-server-current-name name)
    name))

(defun starintel-server-uri-name ()
  "Return the server identity used in `star://' URIs.
The active profile name when one is active, otherwise \"default\"."
  (if starintel-server-current-name
      (symbol-name starintel-server-current-name)
    "default"))

(defun starintel-server-switch (&optional name)
  "Switch to the StarIntel server profile NAME.
Uses completion when called interactively.  Activates the profile and
shows the status buffer."
  (interactive)
  (let* ((names (mapcar #'symbol-name (starintel-server-profile-names)))
         (choice (or name
                     (completing-read "StarIntel server: " names nil t))))
    (starintel-server-activate (intern choice))
    (starintel-status)))

(defun starintel-server-status ()
  "Show the status of the current StarIntel server."
  (interactive)
  (starintel-status))

(provide 'starintel-server)
;;; starintel-server.el ends here
