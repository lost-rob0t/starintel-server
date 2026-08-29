;;; starintel-object.el --- StarIntel object identity and views -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: nsaspy
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, processes

;; Commentary:

;; Canonical StarIntel object identity and the generic object buffer.
;;
;; A `starintel-object' is a reference to one server-side document:
;; the owning server profile, the dtype, the document ID, and the
;; derived `star://' URI.  It never copies authoritative state; the
;; raw document is kept only to render the view.
;;
;; The generic object buffer renders dtype-known titles and data
;; fields (rendered dynamically from the document's `data' section;
;; no fields are invented), server-owned fields, and provenance from
;; `extensions.star_server' when present.  Raw JSON stays one key
;; away with `$'.  Features the server does not provide yet (graph
;; rendering, actor runs) are simply absent instead of emulated.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'subr-x)
(require 'client)
(require 'starintel-server)
(require 'starintel-uri)

(declare-function starintel-ui--report-error "client" (condition plist))

(defvar starintel-object--current)

(defgroup starintel-object nil
  "StarIntel object identity and views."
  :group 'starintel
  :prefix "starintel-object-")

(cl-defstruct starintel-object
  "Reference to one authoritative StarIntel document.
SERVER is the server profile name used in URIs, DTYPE the document
type, ID the document identity, DATASET the logical corpus, URI the
derived star:// URI, and DOC the raw document alist for rendering."
  server dtype id dataset uri doc)

(defvar starintel-object-recent-uris nil
  "Recent object URIs opened this session (client-side convenience).")

(defun starintel-object--doc-field (doc field)
  "Return FIELD from DOC, checking the nested 0.9 `data' section
before legacy flat top-level keys."
  (let ((data (cdr (assq 'data doc))))
    (or (and data (cdr (assq field data)))
        (cdr (assq field doc)))))

(defun starintel-object--title-for-dtype (dtype doc)
  "Return a human title for a DTYPE document DOC.
Only fields the StarIntel schema actually provides are used."
    (cl-case (intern dtype)
      (person
       (let ((parts nil))
         (dolist (field '(fname mname lname))
           (let ((v (starintel-object--doc-field doc field)))
             (when (and (stringp v) (not (string= v "")))
               (push v parts))))
         (let ((name (mapconcat #'identity (nreverse parts) " ")))
           (and (not (string= name "")) name))))
      (org (starintel-object--doc-field doc 'name))
      (domain (starintel-object--doc-field doc 'record))
      (host (let ((hostname (starintel-object--doc-field doc 'hostname))
                  (ip (starintel-object--doc-field doc 'ip)))
              (cond ((and hostname ip) (format "%s (%s)" hostname ip))
                    (hostname hostname))))
      (url (starintel-object--doc-field doc 'url))
      (user (let ((name (starintel-object--doc-field doc 'name))
                  (platform (starintel-object--doc-field doc 'platform)))
              (and name (if platform (format "@%s (%s)" name platform) name))))
      (email (let ((user (starintel-object--doc-field doc 'user))
                   (domain (starintel-object--doc-field doc 'domain)))
               (and user domain (format "%s@%s" user domain))))
      (message (let ((content (starintel-object--doc-field doc 'content)))
                 (and content (substring content 0 (min 60 (length content))))))
      (socialmpost (or (starintel-object--doc-field doc 'title)
                       (starintel-object--doc-field doc 'content)))
      (phone (starintel-object--doc-field doc 'number))
      (breach (starintel-object--doc-field doc 'description))
      (target (let ((value (starintel-object--doc-field doc 'target))
                    (actor (starintel-object--doc-field doc 'actor)))
                (and value (if actor (format "%s @ %s" value actor) value))))
      (relation
       (let ((source (starintel-object--doc-field doc 'source))
             (target-id (starintel-object--doc-field doc 'target))
             (predicate (starintel-object--doc-field doc 'predicate)))
         (and source target-id predicate
              (format "%s -%s-> %s" source predicate target-id))))
      (address (or (starintel-object--doc-field doc 'street)
                   (starintel-object--doc-field doc 'city)))
      (t nil)))

(defun starintel-object-title (obj)
  "Return a human-readable title for OBJ.
Falls back to \"dtype short-id\" when no schema field matches."
  (let ((title (starintel-object--title-for-dtype
                (starintel-object-dtype obj) (starintel-object-doc obj))))
    (or (and (stringp title) (not (string= title "")) title)
        (format "%s %s"
                (starintel-object-dtype obj)
                (substring (starintel-object-id obj)
                           0 (min 8 (length (starintel-object-id obj))))))))

(defun starintel-object--kind (dtype)
  "Map a document DTYPE to its star:// kind.
Known entity dtypes get specific kinds; everything else opens as a
plain document."
  (cl-case (intern dtype)
    (person "person")
    (org "org")
    (target "target")
    (relation "relation")
    (t "document")))

(defun starintel-object-from-doc (doc &optional server)
  "Build a `starintel-object' from the document alist DOC.
SERVER defaults to the current server profile name."
  (let* ((dtype (format "%s" (or (cdr (assq 'dtype doc)) "document")))
         (id (format "%s" (cdr (assq '_id doc))))
         (server (or server (starintel-server-uri-name)))
         (kind (starintel-object--kind dtype)))
    (make-starintel-object
     :server server :dtype dtype :id id
     :dataset (cdr (assq 'dataset doc))
     :uri (starintel-uri-format server kind id)
     :doc doc)))

(defun starintel-object--buffer-name (obj)
  "Return the stable buffer name for OBJ."
  (format "*StarIntel: %s*" (starintel-object-title obj)))

;;; Generic object buffer

(defgroup starintel-object-view nil
  "Generic StarIntel object buffers."
  :group 'starintel-object
  :prefix "starintel-object-")

(defface starintel-object-heading-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for object buffer headings."
  :group 'starintel-object-view)

(defvar starintel-object-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" #'starintel-object-refresh)
    (define-key map "w" #'starintel-object-copy-id)
    (define-key map "u" #'starintel-object-copy-uri)
    (define-key map "$" #'starintel-object-show-raw)
    map)
  "Keymap for `starintel-object-mode'.")

(define-derived-mode starintel-object-mode special-mode "StarIntel-Object"
  "Major mode for generic StarIntel object views.
\\{starintel-object-mode-map}")

(defun starintel-object--buffer (obj)
  "Return the clean object buffer for OBJ."
  (let ((buffer (get-buffer-create (starintel-object--buffer-name obj))))
    (with-current-buffer buffer
      (starintel-object-mode)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq-local starintel-object--current obj))
    buffer))

(defun starintel-object--insert-field (label value)
  "Insert LABEL/VALUE pair on one line."
  (insert (format "%-22s %s\n" label (or value "-"))))

(defun starintel-object--insert-heading (title)
  "Insert TITLE as a section heading."
  (insert "\n" (propertize title 'face 'starintel-object-heading-face) "\n"))

(defun starintel-object--insert-id-button (id uri)
  "Insert ID as a button opening URI when URI is non-nil."
  (if uri
      (insert-button id
                     'starintel-uri uri
                     'action (lambda (_button)
                               (starintel-uri-open uri))
                     'follow-link t)
    (insert id)))

(defun starintel-object--insert-section-data (doc)
  "Render the document's `data' section from DOC."
  (let ((data (cdr (assq 'data doc))))
    (when data
      (starintel-object--insert-heading "Data")
      (dolist (field data)
        (starintel-object--insert-field
         (car field) (starintel-object--format-value (cdr field)))))))

(defun starintel-object--format-value (value)
  "Render VALUE readably; lists are joined, nil renders as \"-\"."
  (cond
   ((null value) "-")
   ((eq value :json-false) "false")
   ((eq value t) "true")
   ((listp value) (mapconcat #'starintel-object--format-value value ", "))
   (t (format "%s" value))))

(defun starintel-object--insert-section-provenance (doc)
  "Render provenance from `extensions.star_server' in DOC."
  (let* ((extensions (cdr (assq 'extensions doc)))
         (star (cdr (assq 'star_server extensions))))
    (when star
      (starintel-object--insert-heading "Provenance")
      (dolist (field star)
        (starintel-object--insert-field
         (car field) (starintel-object--format-value (cdr field)))))))

(defun starintel-object--render (obj)
  "Render OBJ into its buffer."
  (let* ((doc (starintel-object-doc obj))
         (sources (cdr (assq 'sources doc))))
    (with-current-buffer (starintel-object--buffer obj)
      (let ((inhibit-read-only t))
        (insert (propertize (starintel-object-title obj)
                            'face 'starintel-object-heading-face)
                "\n")
        (starintel-object--insert-heading "Identity")
        (starintel-object--insert-field "dtype" (starintel-object-dtype obj))
        (starintel-object--insert-field "dataset" (starintel-object-dataset obj))
        (starintel-object--insert-field "server" (starintel-object-server obj))
        (insert "id                     ")
        (starintel-object--insert-id-button (starintel-object-id obj) nil)
        (insert "\nuri                    ")
        (starintel-object--insert-id-button (starintel-object-uri obj)
                                            (starintel-object-uri obj))
        (insert "\n")
        (when sources
          (starintel-object--insert-heading "Sources")
          (starintel-object--insert-field
           "sources" (starintel-object--format-value sources)))
        (starintel-object--insert-section-data doc)
        (starintel-object--insert-section-provenance doc)
        (goto-char (point-min))
        (setq buffer-read-only t))
      (pop-to-buffer (current-buffer)))))

;;; Public operations

(defun starintel-object-open (_kind id)
  "Fetch and render the StarIntel document with ID.
KIND is the star:// kind that led here; the rendered object uses the
dtype the server actually returns.  The exchange is asynchronous;
typed errors surface through the usual StarIntel error reporting."
  (starintel-api-get-document
   id
   :on-success
   (lambda (doc)
     (let ((obj (starintel-object-from-doc doc)))
       (add-to-list 'starintel-object-recent-uris (starintel-object-uri obj))
       (starintel-object--render obj)))
   :on-error #'starintel-ui--report-error))

(defun starintel-object-refresh ()
  "Refresh the object buffer at point from the server."
  (interactive)
  (if-let ((uri (and (boundp 'starintel-object--current)
                     (starintel-object-uri starintel-object--current))))
      (progn
        (starintel-uri-open uri)
        (message "StarIntel: refreshed %s" uri))
    (user-error "StarIntel: buffer does not reference a StarIntel object")))

(defun starintel-object-copy-id ()
  "Copy the current object's ID."
  (interactive)
  (let ((obj starintel-object--current))
    (kill-new (starintel-object-id obj))
    (message "StarIntel: copied %s" (starintel-object-id obj))))

(defun starintel-object-copy-uri ()
  "Copy the current object's star:// URI."
  (interactive)
  (let ((obj starintel-object--current))
    (kill-new (starintel-object-uri obj))
    (message "StarIntel: copied %s" (starintel-object-uri obj))))

(defun starintel-object-show-raw ()
  "Show the raw JSON document of the current object."
  (interactive)
  (let* ((obj starintel-object--current)
         (name (format "*StarIntel raw: %s*" (starintel-object-id obj)))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (json-encode (starintel-object-doc obj)))
        (ignore-errors (json-pretty-print (point-min) (point-max)))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(provide 'starintel-object)
;;; starintel-object.el ends here
