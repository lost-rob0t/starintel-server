(in-package :cl-user)

(defpackage :star-cli
  (:use :cl :star.api.client)
  (:import-from :starintel
                #:encode
                #:set-meta
                #:new-host
                #:new-url
                #:new-domain)
  (:export #:main))

(in-package :star-cli)

;;; BBP Import functions
(defun parse-bbp-file (filepath)
  "Read a file and return a list of non-empty lines (removing comments and whitespace)."
  (with-open-file (stream filepath :direction :input :if-does-not-exist nil)
    (unless stream
      (error "File not found: ~a" filepath))
    (loop for line = (read-line stream nil nil)
          while line
          for trimmed = (string-trim '(#\Space #\Tab #\Return #\Newline) line)
          unless (or (string= trimmed "")
                     (and (> (length trimmed) 0)
                          (char= (char trimmed 0) #\#)))
            collect trimmed)))

(defun parse-tags (tags-str)
  "Parse comma-separated tags string into a list."
  (when tags-str
    (mapcar (lambda (s) (string-trim '(#\Space #\Tab #\Return #\Newline) s))
            (uiop:split-string tags-str :separator ","))))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun parse-json-file (filepath)
  "Read and parse JSON from a file."
  (with-open-file (stream filepath :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun print-json-response (response)
  "Pretty print JSON response."
  (format t "~a~%" response))

(defun print-error (message)
  "Print error message to stderr."
  (format *error-output* "Error: ~a~%" message))

(defun print-success (message)
  "Print success message."
  (format t "✓ ~a~%" message))

;;; ============================================================================
;;; Global Options
;;; ============================================================================

(defun global-options ()
  "Define global options for all commands."
  (list
   (clingon:make-option
    :string
    :description "Base URL of the star-server API"
    :short-name #\u
    :long-name "url"
    :env-vars '("STAR_SERVER_URL")
    :initial-value "http://127.0.0.1:5000"
    :key :base-url)
   (clingon:make-option
    :boolean
    :description "Pretty print JSON output"
    :long-name "pretty"
    :initial-value nil
    :key :pretty)))

(defun make-client (cmd)
  "Create a star-client from command options."
  (make-instance 'star-client
                 :base-url (clingon:getopt cmd :base-url)))

;;; ============================================================================
;;; Document Commands
;;; ============================================================================

(defun document/create-options ()
  "Options for creating a document."
  (list
   (clingon:make-option
    :string
    :description "Document type (dtype)"
    :short-name #\t
    :long-name "type"
    :required t
    :key :dtype)
   (clingon:make-option
    :string
    :description "Path to JSON file containing document data"
    :short-name #\f
    :long-name "file"
    :key :file)
   (clingon:make-option
    :string
    :description "JSON string of document data (alternative to --file)"
    :short-name #\d
    :long-name "data"
    :key :data)))

(defun document/create-handler (cmd)
  "Handler for creating a document."
  (let* ((client (make-client cmd))
         (dtype (clingon:getopt cmd :dtype))
         (file (clingon:getopt cmd :file))
         (data (clingon:getopt cmd :data))
         (json-data (cond
                      (file (parse-json-file file))
                      (data data)
                      (t (progn
                           (print-error "Either --file or --data must be provided")
                           (clingon:exit 1))))))
    (handler-case
        (let ((response (submit-document client json-data dtype)))
          (print-json-response response)
          (print-success (format nil "Document created with type '~a'" dtype)))
      (error (e)
        (print-error (format nil "Failed to create document: ~a" e))
        (clingon:exit 1)))))

(defun document/create-command ()
  "Create document command."
  (clingon:make-command
   :name "create"
   :description "Create a new document"
   :options (document/create-options)
   :handler #'document/create-handler))

(defun document/get-options ()
  "Options for getting a document."
  (list
   (clingon:make-option
    :string
    :description "Document ID"
    :short-name #\i
    :long-name "id"
    :required t
    :key :doc-id)))

(defun document/get-handler (cmd)
  "Handler for getting a document."
  (let* ((client (make-client cmd))
         (doc-id (clingon:getopt cmd :doc-id)))
    (handler-case
        (let ((response (get-document client doc-id)))
          (print-json-response response))
      (error (e)
        (print-error (format nil "Failed to get document: ~a" e))
        (clingon:exit 1)))))

(defun document/get-command ()
  "Get document command."
  (clingon:make-command
   :name "get"
   :description "Get a document by ID"
   :options (document/get-options)
   :handler #'document/get-handler))

(defun document/update-options ()
  "Options for updating a document."
  (list
   (clingon:make-option
    :string
    :description "Document ID"
    :short-name #\i
    :long-name "id"
    :required t
    :key :doc-id)
   (clingon:make-option
    :string
    :description "Path to JSON file containing updated document data"
    :short-name #\f
    :long-name "file"
    :key :file)
   (clingon:make-option
    :string
    :description "JSON string of updated document data"
    :short-name #\d
    :long-name "data"
    :key :data)))

(defun document/update-handler (cmd)
  "Handler for updating a document."
  (let* ((client (make-client cmd))
         (doc-id (clingon:getopt cmd :doc-id))
         (file (clingon:getopt cmd :file))
         (data (clingon:getopt cmd :data))
         (json-data (cond
                      (file (parse-json-file file))
                      (data data)
                      (t (progn
                           (print-error "Either --file or --data must be provided")
                           (clingon:exit 1))))))
    (handler-case
        (let ((response (api-request client (format nil "/document/~a" doc-id)
                                     :method :put
                                     :content json-data)))
          (print-json-response response)
          (print-success (format nil "Document '~a' updated" doc-id)))
      (error (e)
        (print-error (format nil "Failed to update document: ~a" e))
        (clingon:exit 1)))))

(defun document/update-command ()
  "Update document command."
  (clingon:make-command
   :name "update"
   :description "Update an existing document"
   :options (document/update-options)
   :handler #'document/update-handler))

(defun document/delete-options ()
  "Options for deleting a document."
  (list
   (clingon:make-option
    :string
    :description "Document ID"
    :short-name #\i
    :long-name "id"
    :required t
    :key :doc-id)
   (clingon:make-option
    :boolean
    :description "Force deletion without confirmation"
    :short-name #\f
    :long-name "force"
    :initial-value nil
    :key :force)))

(defun document/delete-handler (cmd)
  "Handler for deleting a document."
  (let* ((client (make-client cmd))
         (doc-id (clingon:getopt cmd :doc-id))
         (force (clingon:getopt cmd :force)))
    (unless force
      (format t "Are you sure you want to delete document '~a'? (yes/no): " doc-id)
      (force-output)
      (let ((confirmation (string-downcase (read-line))))
        (unless (member confirmation '("yes" "y") :test #'string=)
          (format t "Deletion cancelled.~%")
          (clingon:exit 0))))
    (handler-case
        (let ((response (api-request client (format nil "/document/~a" doc-id)
                                     :method :delete)))
          (print-json-response response)
          (print-success (format nil "Document '~a' deleted" doc-id)))
      (error (e)
        (print-error (format nil "Failed to delete document: ~a" e))
        (clingon:exit 1)))))

(defun document/delete-command ()
  "Delete document command."
  (clingon:make-command
   :name "delete"
   :description "Delete a document"
   :options (document/delete-options)
   :handler #'document/delete-handler))

(defun document/search-options ()
  "Options for searching documents."
  (list
   (clingon:make-option
    :string
    :description "Search query string"
    :short-name #\q
    :long-name "query"
    :required t
    :key :query)
   (clingon:make-option
    :integer
    :description "Maximum number of results"
    :short-name #\l
    :long-name "limit"
    :initial-value 25
    :key :limit)
   (clingon:make-option
    :string
    :description "Bookmark for pagination"
    :short-name #\b
    :long-name "bookmark"
    :key :bookmark)
   (clingon:make-option
    :string
    :description "Sort field"
    :short-name #\s
    :long-name "sort"
    :key :sort)))

(defun document/search-handler (cmd)
  "Handler for searching documents."
  (let* ((client (make-client cmd))
         (query (clingon:getopt cmd :query))
         (limit (clingon:getopt cmd :limit))
         (bookmark (clingon:getopt cmd :bookmark))
         (sort (clingon:getopt cmd :sort)))
    (handler-case
        (let ((response (fts client :q query :limit limit :bookmark bookmark :sort sort)))
          (print-json-response response))
      (error (e)
        (print-error (format nil "Search failed: ~a" e))
        (clingon:exit 1)))))

(defun document/search-command ()
  "Search documents command."
  (clingon:make-command
   :name "search"
   :description "Search documents using full-text search"
   :options (document/search-options)
   :handler #'document/search-handler))

(defun document/handler (cmd)
  "Handler for document command group."
  (clingon:print-usage-and-exit cmd t))

(defun document/command ()
  "Document command group."
  (clingon:make-command
   :name "document"
   :description "Manage documents"
   :authors '("nsaspy <nsaspy@airmail.cc>")
   :license "GPL v3"
   :handler #'document/handler
   :sub-commands (list
                  (document/create-command)
                  (document/get-command)
                  (document/update-command)
                  (document/delete-command)
                  (document/search-command))))

;;; ============================================================================
;;; Target Commands
;;; ============================================================================

(defun target/create-options ()
  "Options for creating a target."
  (list
   (clingon:make-option
    :string
    :description "Actor name/ID"
    :short-name #\a
    :long-name "actor"
    :required t
    :key :actor)
   (clingon:make-option
    :string
    :description "Path to JSON file containing target data"
    :short-name #\f
    :long-name "file"
    :key :file)
   (clingon:make-option
    :string
    :description "JSON string of target data (alternative to --file)"
    :short-name #\d
    :long-name "data"
    :key :data)
   (clingon:make-option
    :boolean
    :description "Mark target as transient (non-persistent)"
    :long-name "transient"
    :initial-value nil
    :key :transient)))

(defun target/create-handler (cmd)
  "Handler for creating a target."
  (let* ((client (make-client cmd))
         (actor (clingon:getopt cmd :actor))
         (file (clingon:getopt cmd :file))
         (data (clingon:getopt cmd :data))
         (transient (clingon:getopt cmd :transient))
         (json-data (cond
                      (file (parse-json-file file))
                      (data data)
                      (t (progn
                           (print-error "Either --file or --data must be provided")
                           (clingon:exit 1))))))
    (handler-case
        (let ((response (new-target client json-data actor transient)))
          (print-json-response response)
          (print-success (format nil "Target created for actor '~a'" actor)))
      (error (e)
        (print-error (format nil "Failed to create target: ~a" e))
        (clingon:exit 1)))))

(defun target/create-command ()
  "Create target command."
  (clingon:make-command
   :name "create"
   :description "Create a new target for an actor"
   :options (target/create-options)
   :handler #'target/create-handler))

(defun target/list-options ()
  "Options for listing targets."
  (list
   (clingon:make-option
    :string
    :description "Actor name/ID to list targets for"
    :short-name #\a
    :long-name "actor"
    :required t
    :key :actor)))

(defun target/list-handler (cmd)
  "Handler for listing targets."
  (let* ((client (make-client cmd))
         (actor (clingon:getopt cmd :actor)))
    (handler-case
        (let ((response (get-targets client actor)))
          (print-json-response response))
      (error (e)
        (print-error (format nil "Failed to list targets: ~a" e))
        (clingon:exit 1)))))

(defun target/list-command ()
  "List targets command."
  (clingon:make-command
   :name "list"
   :description "List all targets for an actor"
   :options (target/list-options)
   :handler #'target/list-handler))

(defun target/get-options ()
  "Options for getting a target."
  (list
   (clingon:make-option
    :string
    :description "Target ID"
    :short-name #\i
    :long-name "id"
    :required t
    :key :target-id)))

(defun target/get-handler (cmd)
  "Handler for getting a target."
  (let* ((client (make-client cmd))
         (target-id (clingon:getopt cmd :target-id)))
    (handler-case
        (let ((response (get-document client target-id)))
          (print-json-response response))
      (error (e)
        (print-error (format nil "Failed to get target: ~a" e))
        (clingon:exit 1)))))

(defun target/get-command ()
  "Get target command."
  (clingon:make-command
   :name "get"
   :description "Get a target by ID"
   :options (target/get-options)
   :handler #'target/get-handler))

(defun target/update-options ()
  "Options for updating a target."
  (list
   (clingon:make-option
    :string
    :description "Target ID"
    :short-name #\i
    :long-name "id"
    :required t
    :key :target-id)
   (clingon:make-option
    :string
    :description "Path to JSON file containing updated target data"
    :short-name #\f
    :long-name "file"
    :key :file)
   (clingon:make-option
    :string
    :description "JSON string of updated target data"
    :short-name #\d
    :long-name "data"
    :key :data)))

(defun target/update-handler (cmd)
  "Handler for updating a target."
  (let* ((client (make-client cmd))
         (target-id (clingon:getopt cmd :target-id))
         (file (clingon:getopt cmd :file))
         (data (clingon:getopt cmd :data))
         (json-data (cond
                      (file (parse-json-file file))
                      (data data)
                      (t (progn
                           (print-error "Either --file or --data must be provided")
                           (clingon:exit 1))))))
    (handler-case
        (let ((response (api-request client (format nil "/document/~a" target-id)
                                     :method :put
                                     :content json-data)))
          (print-json-response response)
          (print-success (format nil "Target '~a' updated" target-id)))
      (error (e)
        (print-error (format nil "Failed to update target: ~a" e))
        (clingon:exit 1)))))

(defun target/update-command ()
  "Update target command."
  (clingon:make-command
   :name "update"
   :description "Update an existing target"
   :options (target/update-options)
   :handler #'target/update-handler))

(defun target/delete-options ()
  "Options for deleting a target."
  (list
   (clingon:make-option
    :string
    :description "Target ID"
    :short-name #\i
    :long-name "id"
    :required t
    :key :target-id)
   (clingon:make-option
    :boolean
    :description "Force deletion without confirmation"
    :short-name #\f
    :long-name "force"
    :initial-value nil
    :key :force)))

(defun target/delete-handler (cmd)
  "Handler for deleting a target."
  (let* ((client (make-client cmd))
         (target-id (clingon:getopt cmd :target-id))
         (force (clingon:getopt cmd :force)))
    (unless force
      (format t "Are you sure you want to delete target '~a'? (yes/no): " target-id)
      (force-output)
      (let ((confirmation (string-downcase (read-line))))
        (unless (member confirmation '("yes" "y") :test #'string=)
          (format t "Deletion cancelled.~%")
          (clingon:exit 0))))
    (handler-case
        (let ((response (api-request client (format nil "/document/~a" target-id)
                                     :method :delete)))
          (print-json-response response)
          (print-success (format nil "Target '~a' deleted" target-id)))
      (error (e)
        (print-error (format nil "Failed to delete target: ~a" e))
        (clingon:exit 1)))))

(defun target/delete-command ()
  "Delete target command."
  (clingon:make-command
   :name "delete"
   :description "Delete a target"
   :options (target/delete-options)
   :handler #'target/delete-handler))

(defun target/handler (cmd)
  "Handler for target command group."
  (clingon:print-usage-and-exit cmd t))

(defun target/command ()
  "Target command group."
  (clingon:make-command
   :name "target"
   :description "Manage targets"
   :authors '("nsaspy <nsaspy@airmail.cc>")
   :license "GPL v3"
   :handler #'target/handler
   :sub-commands (list
                  (target/create-command)
                  (target/list-command)
                  (target/get-command)
                  (target/update-command)
                  (target/delete-command))))

;;; ============================================================================
;;; Query Commands
;;; ============================================================================

(defun query/messages-by-user-options ()
  "Options for querying messages by user."
  (list
   (clingon:make-option
    :string
    :description "Username to query"
    :short-name #\u
    :long-name "user"
    :required t
    :key :user)
   (clingon:make-option
    :integer
    :description "Maximum number of results"
    :short-name #\l
    :long-name "limit"
    :initial-value 50
    :key :limit)
   (clingon:make-option
    :boolean
    :description "Return results in descending order"
    :long-name "descending"
    :initial-value nil
    :key :descending)))

(defun query/messages-by-user-handler (cmd)
  "Handler for querying messages by user."
  (let* ((client (make-client cmd))
         (user (clingon:getopt cmd :user))
         (limit (clingon:getopt cmd :limit))
         (descending (clingon:getopt cmd :descending)))
    (handler-case
        (let ((response (messages-by-user client :user user :limit limit :descending descending)))
          (print-json-response response))
      (error (e)
        (print-error (format nil "Query failed: ~a" e))
        (clingon:exit 1)))))

(defun query/messages-by-user-command ()
  "Query messages by user command."
  (clingon:make-command
   :name "messages-by-user"
   :description "Query messages by username"
   :options (query/messages-by-user-options)
   :handler #'query/messages-by-user-handler))

(defun query/messages-by-platform-options ()
  "Options for querying messages by platform."
  (list
   (clingon:make-option
    :string
    :description "Platform to query (e.g., discord, telegram)"
    :short-name #\p
    :long-name "platform"
    :required t
    :key :platform)
   (clingon:make-option
    :integer
    :description "Maximum number of results"
    :short-name #\l
    :long-name "limit"
    :initial-value 50
    :key :limit)
   (clingon:make-option
    :boolean
    :description "Return results in descending order"
    :long-name "descending"
    :initial-value nil
    :key :descending)))

(defun query/messages-by-platform-handler (cmd)
  "Handler for querying messages by platform."
  (let* ((client (make-client cmd))
         (platform (clingon:getopt cmd :platform))
         (limit (clingon:getopt cmd :limit))
         (descending (clingon:getopt cmd :descending)))
    (handler-case
        (let ((response (messages-by-platform client :platform platform :limit limit :descending descending)))
          (print-json-response response))
      (error (e)
        (print-error (format nil "Query failed: ~a" e))
        (clingon:exit 1)))))

(defun query/messages-by-platform-command ()
  "Query messages by platform command."
  (clingon:make-command
   :name "messages-by-platform"
   :description "Query messages by platform"
   :options (query/messages-by-platform-options)
   :handler #'query/messages-by-platform-handler))

(defun query/handler (cmd)
  "Handler for query command group."
  (clingon:print-usage-and-exit cmd t))

(defun query/command ()
  "Query command group."
  (clingon:make-command
   :name "query"
   :description "Query documents with various filters"
   :authors '("nsaspy <nsaspy@airmail.cc>")
   :license "GPL v3"
   :handler #'query/handler
   :sub-commands (list
                  (query/messages-by-user-command)
                  (query/messages-by-platform-command))))

;;; ============================================================================
;;; BBP Commands
;;; ============================================================================

(defun bbp/import-options ()
  "Options for importing BBP data."
  (list
   (clingon:make-option
    :string
    :description "Type of data to import (host, url, domain)"
    :short-name #\t
    :long-name "type"
    :required t
    :key :type)
   (clingon:make-option
    :string
    :description "Path to file containing data (one entry per line)"
    :short-name #\f
    :long-name "file"
    :required t
    :key :file)
   (clingon:make-option
    :string
    :description "Dataset name (e.g., bug bounty program name)"
    :short-name #\d
    :long-name "dataset"
    :required t
    :key :dataset)
   (clingon:make-option
    :boolean
    :description "Suppress verbose output"
    :short-name #\q
    :long-name "quiet"
    :initial-value nil
    :key :quiet)))

(defun make-host-document (hostname dataset)
  "Create a host document using the spec package."
  (encode (set-meta  (new-host dataset :hostname hostname) dataset)))

(defun make-url-document (url dataset)
  "Create a URL document using the spec package."
  (encode (set-meta (new-url dataset :url url) dataset)))

(defun make-domain-document (domain dataset)
  "Create a domain document using the spec package."
  (encode (set-meta  (new-domain dataset :record domain :record-type "A") dataset)))

(defun bbp/import-handler (cmd)
  "Handler for importing BBP data."
  (let* ((client (make-client cmd))
         (type (string-downcase (clingon:getopt cmd :type)))
         (file (clingon:getopt cmd :file))
         (dataset (clingon:getopt cmd :dataset))
         (verbose (not (clingon:getopt cmd :quiet)))
         (entries (parse-bbp-file file))
         (success-count 0)
         (error-count 0))

    (unless (member type '("host" "url" "domain") :test #'string=)
      (print-error (format nil "Invalid type '~a'. Must be host, url, or domain" type))
      (clingon:exit 1))

    (dolist (entry entries)
      (handler-case
          (let ((doc (cond
                       ((string= type "host")
                        (make-host-document entry dataset))
                       ((string= type "url")
                        (make-url-document entry dataset))
                       ((string= type "domain")
                        (make-domain-document entry dataset)))))
            (submit-document client doc type)
            (incf success-count)
            (when verbose
              (format t "✓ Imported ~a: ~a~%" type entry)))
        (error (e)
          (incf error-count)
          (when verbose
            (format *error-output* "✗ Failed to import ~a ~a: ~a~%" type entry e)))))

    (format t "~%Import complete: ~a succeeded, ~a failed~%" success-count error-count)
    (if (> error-count 0)
        (clingon:exit 1)
        (clingon:exit 0))))

(defun bbp/import-command ()
  "Import BBP data command."
  (clingon:make-command
   :name "import"
   :description "Import BBP data (hosts, URLs, domains) from file"
   :options (bbp/import-options)
   :handler #'bbp/import-handler))

(defun bbp/handler (cmd)
  "Handler for BBP command group."
  (clingon:print-usage-and-exit cmd t))

(defun bbp/command ()
  "BBP command group."
  (clingon:make-command
   :name "bbp"
   :description "Bug Bounty Program data management"
   :authors '("nsaspy <nsaspy@airmail.cc>")
   :license "GPL v3"
   :handler #'bbp/handler
   :sub-commands (list
                  (bbp/import-command))))

;;; ============================================================================
;;; Main Command
;;; ============================================================================

(defun main/handler (cmd)
  "Handler for main command."
  (clingon:print-usage-and-exit cmd t))

(defun main/command ()
  "Main command definition."
  (clingon:make-command
   :name "star-cli"
   :version "0.1.0"
   :description "Command-line client for StarIntel Gserver API"
   :authors '("nsaspy <nsaspy@airmail.cc>")
   :license "GPL v3"
   :options (global-options)
   :handler #'main/handler
   :sub-commands (list
                  (document/command)
                  (target/command)
                  (query/command)
                  (bbp/command))))

(defun main ()
  "Main entry point for the CLI application."
  (let ((app (main/command)))
    (clingon:run app)))
