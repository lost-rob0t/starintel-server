;;; starintel-uri.el --- star:// URIs for StarIntel objects -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: nsaspy
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, processes

;; Commentary:

;; The `star://' URI scheme gives every remote StarIntel object a
;; stable, copyable identity:
;;
;;     star://SERVER/KIND/ID
;;
;; SERVER is a `starintel-servers' profile name (empty means the
;; current server).  KIND is an open vocabulary (document, person,
;; org, target, relation, search, ...).  ID is the remainder of the
;; URI; IDs may themselves contain slashes, and space and percent
;; characters are percent-encoded for round-tripping.
;;
;; URIs open through `starintel-uri-open', which dispatches on KIND
;; and is also registered as the follower of the Org \"star\" link
;; when Org is available.  Handlers can be extended by adding to
;; `starintel-uri-kind-handlers'.

;;; Code:

(require 'url-util)
(require 'cl-lib)
(require 'client)
(require 'starintel-server)

(declare-function org-link-set-parameters "org" (type &rest parameters))
(declare-function org-link-store-props "org" (&rest properties))
(declare-function starintel-object-open "starintel-object" (kind id))

;; Declared in starintel-object.el; resolved when the Org completion
;; handler loads that module.
(defvar starintel-object-recent-uris)

(defgroup starintel-uri nil
  "star:// URIs for StarIntel objects."
  :group 'starintel
  :prefix "starintel-uri-")

(defconst starintel-uri-scheme "star"
  "URI scheme for StarIntel object references.")

(defvar starintel-uri-kind-handlers nil
  "Alist of (KIND . FUNCTION) for opening star:// URIs.
FUNCTION receives (SERVER KIND ID); the default handler fetches the
document with ID and renders a generic object buffer.  Register
handlers for kinds that need specialized behavior, such as search.")

(defun starintel-uri-parse (uri)
  "Parse a star:// URI into a plist (:server :kind :id).
SERVER is nil when the URI omits it.  Returns nil when URI is not a
parseable star URI.  Accepts the Org link path form (\"//S/K/I\")."
  (when (stringp uri)
    (when (string-prefix-p "//" uri)
      (setq uri (concat starintel-uri-scheme ":" uri)))
    (when (string-match
           (concat "\\`" starintel-uri-scheme
                   "://\\([^/]*\\)/\\([^/]+\\)\\(?:/\\(.*\\)\\)?\\'") uri)
      (list :server (let ((name (match-string 1 uri)))
                      (and (not (string= name "")) name))
            :kind (downcase (match-string 2 uri))
            :id (url-unhex-string (or (match-string 3 uri) ""))))))

(defun starintel-uri--encode (value)
  "Encode VALUE for safe embedding in a star:// URI.
Percent characters are encoded first so the space encoding survives."
  (replace-regexp-in-string
   " " "%20"
   (replace-regexp-in-string "%" "%25" (format "%s" value))))

(defun starintel-uri-format (server kind id)
  "Format a star:// URI.
SERVER is a profile name or nil for the current server; KIND and ID
form the object path."
  (concat starintel-uri-scheme
          "://"
          (or server (starintel-server-uri-name))
          "/" (format "%s" kind)
          "/" (starintel-uri--encode id)))

;;;###autoload
(defun starintel-uri-open (uri)
  "Open the StarIntel object referenced by URI.
Dispatches on the URI kind.  When the URI names a server profile
other than the active one, that profile is activated first."
  (interactive "sOpen star:// URI: ")
  (let ((parsed (starintel-uri-parse uri)))
    (unless parsed
      (user-error "StarIntel: %S is not a %s:// URI" uri starintel-uri-scheme))
    (let* ((server (plist-get parsed :server))
           (kind (plist-get parsed :kind))
           (id (plist-get parsed :id)))
      (when (and server
                 (not (string= server (starintel-server-uri-name))))
        (starintel-server-activate (intern server)))
      (let ((handler (or (cdr (assoc-string kind starintel-uri-kind-handlers))
                         #'starintel-uri--open-document)))
        (funcall handler server kind id)))))

(defun starintel-uri--open-document (_server kind id)
  "Default star:// handler: fetch the document with ID.
KIND only labels the expectation; the rendered object uses the dtype
the server returns.  Kinds without an ID cannot name a document."
  (if (or (null id) (string= id ""))
      (user-error "StarIntel: cannot open star:// URI kind `%s' with no id" kind)
    (starintel-object-open kind id)))

;; Org integration: the "star" link type follows and stores URIs.
(with-eval-after-load 'org
  (org-link-set-parameters
   starintel-uri-scheme
   :follow #'starintel-uri-open
   :store #'starintel-uri--org-store
   :complete #'starintel-uri--org-complete))

(defun starintel-uri--org-store ()
  "Store the current object as an Org \"star\" link, when applicable."
  (let ((uri (get-text-property (point) 'starintel-uri)))
    (when uri
      (org-link-store-props :type starintel-uri-scheme :link uri))))

(defun starintel-uri--org-complete ()
  "Complete a star:// URI from the recent-objects history."
  (require 'starintel-object)
  (let ((uri (completing-read
              "StarIntel object URI: "
              (delete-dups (append starintel-object-recent-uris nil))
              nil nil)))
    (concat starintel-uri-scheme ":" uri)))

(provide 'starintel-uri)
;;; starintel-uri.el ends here
