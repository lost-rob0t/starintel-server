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
;;; Bulk Commands
;;; ============================================================================

(defun bulk/import-options ()
  "Options for bulk importing documents."
  (list
   (clingon:make-option
    :string
    :description "Path to file containing documents (NDJSON by default, use --format for array)"
    :short-name #\f
    :long-name "file"
    :required t
    :key :file)
   (clingon:make-option
    :integer
    :description "Maximum documents per batch (default: 500)"
    :short-name #\b
    :long-name "batch-size"
    :initial-value 500
    :key :batch-size)
   (clingon:make-option
    :string
    :description "Input format: 'ndjson' (newline-delimited JSON) or 'array' (JSON array)"
    :long-name "format"
    :initial-value "ndjson"
    :key :format)
   (clingon:make-option
    :boolean
    :description "Suppress verbose output"
    :short-name #\q
    :long-name "quiet"
    :initial-value nil
    :key :quiet)))

(defun read-ndjson-file (file)
  "Read NDJSON file and return a list of parsed JSON objects."
  (with-open-file (stream file :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          for trimmed = (string-trim '(#\Space #\Tab #\Return #\Newline) line)
          unless (string= trimmed "")
            collect (jsown:parse trimmed))))

(defun read-json-array-file (file)
  "Read a JSON array file and return a list of parsed JSON objects."
  (with-open-file (stream file :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      (jsown:parse content))))

(defun bulk/import-handler (cmd)
  "Handler for bulk importing documents."
  (let* ((client (make-client cmd))
         (file (clingon:getopt cmd :file))
         (batch-size (clingon:getopt cmd :batch-size))
         (format (string-downcase (clingon:getopt cmd :format)))
         (verbose (not (clingon:getopt cmd :quiet))))

    (unless (probe-file file)
      (print-error (format nil "File not found: ~a" file))
      (clingon:exit 1))

    (unless (member format '("ndjson" "array") :test #'string=)
      (print-error (format nil "Invalid format '~a'. Must be 'ndjson' or 'array'" format))
      (clingon:exit 1))

    (handler-case
        (let* ((documents (if (string= format "ndjson")
                              (read-ndjson-file file)
                              (read-json-array-file file))))

          (unless (listp documents)
            (print-error "File must contain a JSON array of documents")
            (clingon:exit 1))

          (let ((total-count (length documents))
                (total-success 0)
                (total-failed 0))

            (when verbose
              (format t "Found ~a documents to import (format: ~a)~%" total-count format))

            (loop for batch-start from 0 below total-count by batch-size
                  for batch-num from 1
                  do (let* ((batch-end (min (+ batch-start batch-size) total-count))
                            (batch (subseq documents batch-start batch-end)))
                       (when verbose
                         (format t "~%Submitting batch ~a (~a documents)...~%"
                                 batch-num (length batch)))
                       (handler-case
                           (let* ((response (bulk-submit client batch))
                                  (result (jsown:parse response))
                                  (succeeded (jsown:val result "succeeded"))
                                  (failed (jsown:val result "failed")))
                             (incf total-success succeeded)
                             (incf total-failed failed)
                             (when verbose
                               (format t "Batch ~a: ~a succeeded, ~a failed~%"
                                       batch-num succeeded failed))
                             (when (and (jsown:keyp result "errors") verbose)
                               (loop for error in (jsown:val result "errors")
                                     do (format *error-output* "  Error at index ~a: ~a~%"
                                                (jsown:val error "index")
                                                (jsown:val error "error")))))
                         (error (e)
                           (incf total-failed (length batch))
                           (when verbose
                             (format *error-output* "Batch ~a failed: ~a~%" batch-num e))))))

            (format t "~%Bulk import complete: ~a succeeded, ~a failed~%"
                    total-success total-failed)
            (if (> total-failed 0)
                (clingon:exit 1)
                (clingon:exit 0))))
      (error (e)
        (print-error (format nil "Failed to process file: ~a" e))
        (clingon:exit 1)))))

(defun bulk/import-command ()
  "Bulk import command."
  (clingon:make-command
   :name "import"
   :description "Import multiple documents from NDJSON or JSON array file"
   :options (bulk/import-options)
   :handler #'bulk/import-handler))

(defun bulk/handler (cmd)
  "Handler for bulk command group."
  (clingon:print-usage-and-exit cmd t))

(defun bulk/command ()
  "Bulk command group."
  (clingon:make-command
   :name "bulk"
   :description "Bulk document operations"
   :authors '("nsaspy <nsaspy@airmail.cc>")
   :license "GPL v3"
   :handler #'bulk/handler
   :sub-commands (list
                  (bulk/import-command))))

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
    :description "The Source of the data. eg nmap, subfinder."
    :required nil
    :short-name #\s
    :long-name "source"
    :key :source)
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
    :key :dataset
    :env-vars '("HACKMODE_OPERATION"))
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
  (encode (set-meta  (new-domain dataset :record domain) dataset)))

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
            (submit-document client (jsown:to-json doc) type)
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
;;; Document Creation
;;; ============================================================================



(defun kebab-case (sym-or-str)
  "Convert symbol/string into lowercase kebab-case string."
  (let* ((s (etypecase sym-or-str
              (symbol (string-downcase (symbol-name sym-or-str)))
              (string (string-downcase sym-or-str)))))
    (substitute #\- #\_ s)))

(defun parse-comma-list (s)
  "Parse comma-separated string into a list of trimmed, non-empty strings."
  (when s
    (remove-if (lambda (x) (or (null x) (string= x "")))
               (mapcar (lambda (x) (string-trim '(#\Space #\Tab #\Return #\Newline) x))
                       (uiop:split-string s :separator ",")))))

(defun ensure-list (x) (if (listp x) x (list x)))

(defun plist-put (plist key value)
  "Functional plist set."
  (let ((pos (position key plist :test #'eq)))
    (if pos
        (let ((out (copy-list plist)))
          (setf (nth (1+ pos) out) value)
          out)
        (list* key value plist))))

(defun maybe-parse-integer (x)
  (cond
    ((integerp x) x)
    ((stringp x)
     (handler-case (parse-integer x)
       (error () x)))
    (t x)))

(defun gen/print-one-json (x)
  (cond
    ((null x) nil)
    ((stringp x) (format t "~a~%" x))
    (t (format t "~a~%" (jsown:to-json x)))))

(defun gen/jsown-structure-p (value)
  (and (consp value)
       (member (first value) '(:obj :arr :array) :test #'eq)))

(defun gen/print-documents (docs)
  (cond
    ((gen/jsown-structure-p docs)
     (gen/print-one-json docs))
    ((listp docs)
     (dolist (d docs)
       (gen/print-one-json d)))
    (t
     (gen/print-one-json docs))))

(defun gen/ensure-finalized-class (class-or-symbol)
  "Return a finalized class object (SBCL requires finalization for CLASS-SLOTS)."
  (let ((class (etypecase class-or-symbol
                 (symbol (find-class class-or-symbol))
                 (class class-or-symbol))))
    (unless (sb-mop:class-finalized-p class)
      (sb-mop:finalize-inheritance class))
    class))

;;; ---------------------------------------------------------------------------
;;; Registry: dtype -> class
;;; ---------------------------------------------------------------------------

(defparameter *gen-dtype-registry* (make-hash-table :test 'equal))
(defparameter *gen-dtype-order* '()) ; deterministic ordering

(defmacro define-gen-dtype (dtype class-symbol)
  "Register a dtype string to a CLOS class symbol."
  `(progn
     (setf (gethash (string-downcase ,dtype) *gen-dtype-registry*) ',class-symbol)
     (pushnew (string-downcase ,dtype) *gen-dtype-order* :test #'string=)
     ',class-symbol))

(defun gen/class-for-dtype (dtype)
  (or (gethash (string-downcase dtype) *gen-dtype-registry*)
      (error "Unknown dtype '~a'. Register with (define-gen-dtype ...)." dtype)))

;;; ---------------------------------------------------------------------------
;;; Slots to hide from CLI (base/meta fields)
;;; ---------------------------------------------------------------------------

(defparameter *gen-excluded-slot-names*
  ;; compare by symbol-name (case-insensitive) to avoid package headaches
  '("_ID" "_REV" "DTYPE" "DATASET" "DATE-ADDED" "DATE-UPDATED" "VERSION" "SOURCES"))

(defun gen/slot-allowed-p (slot-name-symbol)
  (let ((n (string-upcase (symbol-name slot-name-symbol))))
    (not (member n *gen-excluded-slot-names* :test #'string=))))

(defun gen/slot-initarg (slotd)
  (first (sb-mop:slot-definition-initargs slotd)))

(defun gen/slot-default (slotd)
  (ignore-errors (sb-mop:slot-definition-initform slotd)))

(defun gen/slot-type (slotd)
  (ignore-errors (sb-mop:slot-definition-type slotd)))

(defun gen/list-of-integers-type-p (stype)
  (and (consp stype)
       (eq (first stype) 'list)
       (member 'integer (rest stype) :test #'eq)))

(defun gen/infer-option-type (slotd)
  "Infer a clingon option type from slot type/initform (best-effort)."
  (let ((stype (gen/slot-type slotd))
        (initf (gen/slot-default slotd)))
    (cond
      ((or (eq stype 'boolean) (typep initf 'boolean)) :boolean)
      ((or (eq stype 'integer) (typep initf 'integer)) :integer)
      (t :string))))

;;; ---------------------------------------------------------------------------
;;; Common gen options
;;; ---------------------------------------------------------------------------

(defun gen/common-options ()
  (list
   (clingon:make-option
    :string
    :description "Dataset name"
    :short-name #\d
    :long-name "dataset"
    :env-vars '("HACKMODE_OPERATION" "STAR_DATASET")
    :initial-value "default"
    :key :dataset)))


;;; ---------------------------------------------------------------------------
;;; Build clingon options dynamically from class slots
;;; ---------------------------------------------------------------------------

(defun gen/make-slot-option (slotd)
  (let* ((slot-name (sb-mop:slot-definition-name slotd))
         (initarg   (gen/slot-initarg slotd)))
    (when (and initarg (gen/slot-allowed-p slot-name))
      (let* ((otype (gen/infer-option-type slotd))
             (lname (kebab-case slot-name)))
        ;; DO NOT set initial values from initforms; let the class defaults apply.
        (clingon:make-option
         otype
         :description (format nil "Set ~a" slot-name)
         :long-name lname
         :key initarg
         :initial-value (if (eq otype :boolean) nil nil))))))

(defun gen/options-for-class (class-symbol)
  (let* ((class (gen/ensure-finalized-class class-symbol))
         (slots (sb-mop:class-slots class)))
    (remove nil (mapcar #'gen/make-slot-option slots))))

;;; ---------------------------------------------------------------------------
;;; Multi-doc emission support (comma list -> N docs)
;;;   Map dtype -> slot-name (string) whose value can be "a,b,c" => emit 3 docs.
;;; ---------------------------------------------------------------------------

(defparameter *gen-multi-slot-by-dtype*
  ;; dtype -> slot-name-string (case-insensitive match vs slot-definition-name)
  '(("target" . "TARGET")))

(defun gen/find-slotd-by-name (class slot-name-upcase)
  (find slot-name-upcase
        (sb-mop:class-slots class)
        :key (lambda (sd) (string-upcase (symbol-name (sb-mop:slot-definition-name sd))))
        :test #'string=))

(defun gen/multi-slot-name (dtype)
  (cdr (assoc (string-downcase dtype) *gen-multi-slot-by-dtype* :test #'string=)))

;;; ---------------------------------------------------------------------------
;;; Build initargs from CLI + make/encode instances
;;; ---------------------------------------------------------------------------

(defun gen/build-initargs (cmd class-symbol)
  "Collect initargs for CLASS-SYMBOL from CLI values. Only include non-NIL values."
  (let* ((class (gen/ensure-finalized-class class-symbol))
         (slots (sb-mop:class-slots class))
         (initargs '()))
    (dolist (slotd slots initargs)
      (let* ((slot-name (sb-mop:slot-definition-name slotd))
             (initarg   (gen/slot-initarg slotd)))
        (when (and initarg (gen/slot-allowed-p slot-name))
          (let ((val (clingon:getopt cmd initarg)))
            (when (not (null val))
              (let* ((stype (gen/slot-type slotd))
                     (parsed
                       (cond
                         ;; list slot: allow comma parsing
                         ((or (eq stype 'list) (and (consp stype) (eq (first stype) 'list)))
                          (let ((lst (if (stringp val) (parse-comma-list val) (ensure-list val))))
                            (if (gen/list-of-integers-type-p stype)
                                (mapcar #'maybe-parse-integer lst)
                                lst)))
                         ;; integer slot: already integer from clingon, but be safe
                         ((eq stype 'integer) (maybe-parse-integer val))
                         (t val))))
                (setf initargs (plist-put initargs initarg parsed))))))))))

(defun gen/encode-instance (instance dataset)
  (encode (set-meta instance dataset)))

(defun gen/generate (cmd dtype)
  "Generate JSON doc strings for a dtype."
  (let* ((dataset      (clingon:getopt cmd :dataset))
         (class-symbol (gen/class-for-dtype dtype))
         ;; finalize once, early
         (class        (gen/ensure-finalized-class class-symbol))
         (initargs     (gen/build-initargs cmd class-symbol))
         (multi-slot   (gen/multi-slot-name dtype)))
    (declare (ignore class))
    (if multi-slot
        (let* ((class   (gen/ensure-finalized-class class-symbol))
               (slotd   (or (gen/find-slotd-by-name class (string-upcase multi-slot))
                            (error "Configured multi-slot ~a not found on class ~a" multi-slot class-symbol)))
               (initarg  (or (gen/slot-initarg slotd)
                             (error "Slot ~a has no initarg on class ~a" multi-slot class-symbol)))
               (raw      (clingon:getopt cmd initarg))
               (vals     (if (stringp raw) (parse-comma-list raw) (ensure-list raw))))
          (unless (and vals (>= (length vals) 1))
            (error "Missing multi value for --~a" (kebab-case multi-slot)))
          (mapcar (lambda (v)
                    (gen/encode-instance
                     (apply #'make-instance class-symbol
                            (plist-put initargs initarg v))
                     dataset))
                  vals))
        (gen/encode-instance
         (apply #'make-instance class-symbol initargs)
         dataset))))

(defun gen/make-subcommand-for-dtype (dtype)
  (let* ((class-symbol (gen/class-for-dtype dtype))
         (_class (gen/ensure-finalized-class class-symbol))
         (opts (append (gen/common-options)
                       (gen/options-for-class class-symbol))))
    (declare (ignore _class))
    (clingon:make-command
     :name dtype
     :description (format nil "Generate ~a document(s) locally (MOP-derived options)" dtype)
     :options opts
     :handler (lambda (cmd)
                (handler-case
                    (gen/print-documents (gen/generate cmd dtype))
                  (error (e)
                    (print-error (format nil "Gen failed: ~a" e))
                    (clingon:exit 1)))))))

(defun gen/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defun gen/command ()
  (clingon:make-command
   :name "gen"
   :description "Generate StarIntel documents locally (MOP-derived options)"
   :handler #'gen/handler
   :sub-commands
   (let ((dtypes (sort (copy-list *gen-dtype-order*) #'string<)))
     (mapcar #'gen/make-subcommand-for-dtype dtypes))))

(define-gen-dtype "target" starintel:target)
(define-gen-dtype "person" starintel:person)
(define-gen-dtype "url"    starintel:url)
(define-gen-dtype "host"   starintel:host)
(define-gen-dtype "domain" starintel:domain)
(define-gen-dtype "org"    starintel:org)
(define-gen-dtype "relation"    starintel:relation)

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
                  (bulk/command)
                  (bbp/command)
                  (gen/command))))

(defun main ()
  "Main entry point for the CLI application."
  (let ((app (main/command)))
    (clingon:run app)))
