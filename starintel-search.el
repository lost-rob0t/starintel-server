;;; starintel-search.el --- StarIntel search results workbench -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: nsaspy
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, processes

;; Commentary:

;; First-class search over the capability-resolved StarIntel search
;; endpoint.  Results render in a `tabulated-list-mode' buffer with
;; actionable rows: open the object, copy its star:// URI, mark rows
;; for bulk actions, and page forward with the server bookmark.
;;
;; The server stays authoritative: this buffer only renders rows the
;; server returned and keeps no local database.

;;; Code:

(require 'tabulated-list)
(require 'cl-lib)
(require 'client)
(require 'starintel-server)
(require 'starintel-uri)
(require 'starintel-object)

(defgroup starintel-search nil
  "StarIntel search results buffers."
  :group 'starintel
  :prefix "starintel-search-")

(defcustom starintel-search-limit 25
  "Default number of results per search page."
  :type 'integer
  :group 'starintel-search)

(defcustom starintel-search-buffer-name "*StarIntel Search*"
  "Name of the search results buffer."
  :type 'string
  :group 'starintel-search)

(defface starintel-search-heading-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for search buffer headings."
  :group 'starintel-search)

(defvar starintel-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "g" #'starintel-search-refresh)
    (define-key map "n" #'starintel-search-next-page)
    (define-key map "w" #'starintel-search-copy-uri)
    (define-key map "m" #'starintel-search-toggle-mark)
    (define-key map "a" #'starintel-search-add-marked-to-investigation)
    map)
  "Keymap for `starintel-search-mode'.")

(defvar-local starintel-search--query nil
  "Query currently rendered in this search buffer.")

(defvar-local starintel-search--bookmark nil
  "Server bookmark for the next page, or nil.")

(defvar-local starintel-search--docs nil
  "Alist of (DOCUMENT-ID . DOC-ALIST) for the rendered rows.")

(defvar-local starintel-search--marked nil
  "Alist of marked document IDs, keyed by ID string.")

(define-derived-mode starintel-search-mode tabulated-list-mode "StarIntel-Search"
  "Major mode for StarIntel search result buffers.
\\{starintel-search-mode-map}"
  ;; Reserve a column for mark tags (required by `tabulated-list-put-tag').
  (setq tabulated-list-padding 1))

(defun starintel-search--object-from-doc (doc)
  "Build a `starintel-object' reference from a search result DOC."
  (require 'starintel-object)
  (starintel-object-from-doc doc))

(defun starintel-search--entry (row)
  "Build one tabulated-list entry from a search ROW.
ROW carries `doc' and optionally a score."
  (let* ((doc (cdr (assq 'doc row)))
         (id (or (and doc (cdr (assq '_id doc)))
                 (cdr (assq 'id row))
                 "?"))
         (obj (and doc (starintel-search--object-from-doc doc)))
         (title (if obj
                    (starintel-object-title obj)
                  id))
         (dtype (and doc (cdr (assq 'dtype doc))))
         (dataset (and doc (cdr (assq 'dataset doc))))
         (date-added (and doc (cdr (assq 'dateAdded doc))))
         (date (if (numberp date-added)
                   (format-time-string "%Y-%m-%d" (seconds-to-time date-added))
                 "")))
    (list id
          (vector (or (format "%s" dtype) "?")
                  title
                  (or dataset "")
                  date
                  (propertize id 'starintel-id id
                              'starintel-uri (and obj (starintel-object-uri obj)))))))

(defun starintel-search--render (query data)
  "Render the search document DATA for QUERY."
  (let* ((rows (cdr (assq 'rows data)))
         (bookmark (cdr (assq 'bookmark data)))
         (entries (mapcar #'starintel-search--entry (append rows nil)))
         (docs (mapcar (lambda (row)
                         (let ((doc (cdr (assq 'doc row))))
                           (cons (cdr (assq '_id doc)) doc)))
                       (append rows nil))))
    (with-current-buffer (get-buffer-create starintel-search-buffer-name)
      (starintel-search-mode)
      (setq starintel-search--query query
            starintel-search--bookmark bookmark
            starintel-search--docs docs
            starintel-search--marked nil)
      (setq tabulated-list-entries entries)
      (setq tabulated-list-format
            [("Type" 14 t)
             ("Title" 40 t)
             ("Dataset" 16 t)
             ("Date" 10 t)
             ("ID" 30 t)])
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer))
      (message "StarIntel: %d result(s) for %S%s"
               (length entries) query
               (if bookmark "  [more pages available: n]" "")))))

(cl-defun starintel-search-open (query &key limit bookmark)
  "Search StarIntel for QUERY and render actionable results.
LIMIT bounds the page size; BOOKMARK continues a previous page.  The
exchange is asynchronous against the capability-resolved search
endpoint.  Unavailable search capabilities surface as a clean typed
error instead of breaking the workbench."
  (interactive "sStarIntel search: ")
  (starintel-api-search
   query
   :limit (or limit starintel-search-limit)
   :bookmark bookmark
   :on-success (lambda (data) (starintel-search--render query data))
   :on-error #'starintel-ui--report-error))

(defun starintel-search-refresh ()
  "Re-run the current query."
  (interactive)
  (if starintel-search--query
      (starintel-search-open starintel-search--query)
    (user-error "StarIntel: no query in this search buffer")))

(defun starintel-search-next-page ()
  "Render the next page using the server bookmark."
  (interactive)
  (if starintel-search--bookmark
      (starintel-search-open starintel-search--query
                             :bookmark starintel-search--bookmark)
    (user-error "StarIntel: no further pages")))

(defun starintel-search--entry-object ()
  "Return the `starintel-object' reference for the entry at point."
  (let* ((entry (tabulated-list-get-entry))
         (id (and entry (vectorp entry)
                  (>= (length entry) 5)
                  (get-text-property 0 'starintel-id (aref entry 4)))))
    (and id
         (let ((doc (cdr (assoc id starintel-search--docs))))
           (and doc (starintel-search--object-from-doc doc))))))

(defun starintel-search-copy-uri ()
  "Copy the star:// URI of the entry at point."
  (interactive)
  (let ((obj (starintel-search--entry-object)))
    (if obj
        (progn
          (kill-new (starintel-object-uri obj))
          (message "StarIntel: copied %s" (starintel-object-uri obj)))
      (user-error "StarIntel: no StarIntel object at point"))))

(defun starintel-search-toggle-mark ()
  "Toggle the mark on the entry at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((id (tabulated-list-get-id)))
      (when id
        (if (assoc id starintel-search--marked)
            (progn
              (setq starintel-search--marked
                    (assq-delete-all id starintel-search--marked))
              (tabulated-list-put-tag " " nil))
          (push (cons id t) starintel-search--marked)
          (tabulated-list-put-tag "*" nil))))))

(defun starintel-search-add-marked-to-investigation ()
  "Add marked (or current) entries to the current Org investigation.
Investigation workbenches are not implemented yet; this command says
so instead of silently doing nothing."
  (interactive)
  (user-error "StarIntel: investigation integration is not implemented yet"))

(defun starintel-search--uri-open (_server _kind query)
  "star:// handler for the search kind: open QUERY as a search."
  (starintel-search-open query))

(add-to-list 'starintel-uri-kind-handlers
             (cons "search" #'starintel-search--uri-open))

(provide 'starintel-search)
;;; starintel-search.el ends here
