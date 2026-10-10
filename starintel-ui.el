;;; starintel.el --- StarIntel OSINT workbench for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: nsaspy
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, processes

;; Commentary:

;; The StarIntel workbench entry point: `M-x starintel'.
;;
;; This is the cockpit command set over the authoritative StarIntel
;; server.  Modules:
;;
;;   `starintel-server' - named server profiles and identity
;;   `starintel-uri'    - star:// object URIs
;;   `starintel-object' - generic object buffers
;;   `starintel-search' - actionable search results
;;
;; When `transient' is available (bundled with Emacs 28+) the entry
;; command opens a discoverable menu; otherwise it opens the server
;; status buffer.  Workbench areas that the connected deployment does
;; not provide render as clearly unavailable rather than crashing.

;;; Code:

(require 'client)
(require 'starintel-server)
(require 'starintel-uri)
(require 'starintel-object)
(require 'starintel-search)

(declare-function transient-define-prefix "ext:transient" (&rest _args))

(defun starintel ()
  "Open the StarIntel OSINT workbench."
  (interactive)
  (if (and (featurep 'transient) (fboundp 'starintel-workbench))
      (funcall 'starintel-workbench)
    (starintel-status)))

(when (featurep 'transient)
  (transient-define-prefix starintel-workbench ()
    "StarIntel OSINT workbench menu."
    [["Search and objects"
      ("s" "Search" starintel-search-open)
      ("d" "Open document by ID" starintel-document)
      ("u" "Open star:// URI" starintel-uri-open)]
     ["Server"
      ("S" "Server status" starintel-status)
      ("P" "Switch server profile" starintel-server-switch)
      ("R" "Refresh capabilities" starintel-api-clear-capabilities)]]))

(provide 'starintel)
;;; starintel.el ends here
