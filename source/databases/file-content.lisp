(in-package :star.databases.couchdb)

(defparameter *file-max-bytes* (* 8 1024 1024)
  "Maximum decoded content accepted by the bounded CouchDB file adapter.")

(define-condition file-content-error (error)
  ((code :initarg :code :reader file-content-error-code)
   (status :initarg :status :initform 422 :reader file-content-error-status))
  (:documentation "A typed rejection of file bytes or their transport identity.")
  (:report (lambda (condition stream)
             (write-string (file-content-error-code condition) stream))))

(defun reject-file-content (code &optional (status 422))
  "Reject file input with a stable machine code and HTTP status."
  (error 'file-content-error :code code :status status))

(defun file-content-digest (bytes)
  "Return the lowercase hexadecimal SHA-256 digest of BYTES."
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 bytes)))

(defun verify-file-content (document bytes)
  "Verify bounded bytes against a canonical file's declared content identity."
  (unless (member (star.documents:document-dtype document)
                  '("file" "image" "picture" "video" "video-frame" "audio") :test #'string=)
    (reject-file-content "unsupported_file_dtype"))
  (when (> (length bytes) *file-max-bytes*)
    (reject-file-content "file_too_large" 413))
  (unless (equal "sha256" (outbox-object-value document "bytesHashAlgorithm"))
    (reject-file-content "unsupported_bytes_hash_algorithm"))
  (unless (equal (file-content-digest bytes) (outbox-object-value document "bytesHash"))
    (reject-file-content "bytes_hash_mismatch"))
  (let ((declared-size (outbox-object-value document "sizeBytes")))
    (when (and declared-size (/= declared-size (length bytes)))
      (reject-file-content "file_size_mismatch")))
  bytes)

(defun prepare-file-ingest (envelope &key trusted-tenant-p)
  "Decode a transport envelope; add attachment only AFTER core validation.

Trusted broker callers may carry the existing server tenant_id metadata.
HTTP callers must pass strict canonical documents without this internal field."
  (unless (and (json-object-p envelope)
               (= 2 (length (star.documents:object-keys envelope)))
               (outbox-object-has-key-p envelope "document")
               (outbox-object-has-key-p envelope "contentBase64"))
    (reject-file-content "invalid_file_envelope" 400))
  (let* ((document (star.documents:clone-document-object (jsown:val envelope "document")))
         (tenant (and trusted-tenant-p (outbox-object-value document "tenant_id")))
         (encoded (jsown:val envelope "contentBase64")))
    (when tenant (jsown:remkey document "tenant_id"))
    (star.documents:validate-document document)
    (unless (and (stringp encoded)
                 (<= (length encoded) (* 4 (ceiling *file-max-bytes* 3))))
      (reject-file-content "file_too_large" 413))
    (let ((bytes (handler-case (cl-base64:base64-string-to-usb8-array encoded)
                   (error () (reject-file-content "invalid_content_base64" 400)))))
      ;; Reject permissive decoder interpretations and trailing garbage.
      (unless (string= encoded (cl-base64:usb8-array-to-base64-string bytes))
        (reject-file-content "invalid_content_base64" 400))
      (verify-file-content document bytes)
      (setf (jsown:val document "sizeBytes") (length bytes)
            (jsown:val document "storageId")
            (format nil "couchdb:~a:content" (star.documents:document-id document)))
      (unless (outbox-object-has-key-p document "quarantined")
        (setf (jsown:val document "quarantined") :true))
      (star.documents:validate-document document)
      (setf document (star.documents:ensure-document document))
      (when tenant (setf (jsown:val document "tenant_id") tenant))
      (setf (jsown:val document "_attachments")
            (jsown:new-js
              ("content" (jsown:new-js ("content_type" "application/octet-stream")
                                        ("data" encoded)))))
      document)))

(defun persist-file-ingest (client database document publish-fn)
  "Commit bytes with metadata/outbox; preserve reviewed metadata on replay.

A metadata-only existing file requires its current canonical rev to attach
bytes. Every CAS retry checks that same revision before changing storage."
  (let* ((id (star.documents:document-id document))
         (existing (couchdb-load-outbox-document client database id)))
    (unless existing
      (return-from persist-file-ingest
        (couchdb-process-outbox-mutation client database publish-fn document :new)))
    (unless (equal (outbox-object-value existing "tenant_id" "default")
                   (outbox-object-value document "tenant_id" "default"))
      (error 'mutation-conflict :document-id id :mutation-id "file-upload"
             :reason "existing file tenant differs"))
    (dolist (key '("dtype" "dataset" "bytesHash" "bytesHashAlgorithm"))
      (unless (equal (outbox-object-value existing key) (outbox-object-value document key))
        (error 'mutation-conflict :document-id id :mutation-id "file-upload"
               :reason (format nil "existing file ~a differs" key))))
    (when (outbox-object-has-key-p existing "_attachments")
      ;; Replay reconciles pending publication, never downgrades later reviews.
      (couchdb-file-content client database existing)
      (recover-outbox-documents
       (lambda (document-id) (couchdb-load-outbox-document client database document-id))
       (lambda (state) (couchdb-save-outbox-document client database state))
       publish-fn (list existing))
      (return-from persist-file-ingest (couchdb-load-outbox-document client database id)))
    (let ((expected-rev (outbox-object-value document "rev"))
          (candidate (clone-outbox-json existing))
          (committed-p nil))
      (unless (and expected-rev (equal expected-rev (outbox-object-value existing "_rev")))
        (reject-file-content "file_revision_required_or_stale" 409))
      (dolist (key '("storageId" "sizeBytes" "_attachments"))
        (setf (jsown:val candidate key) (jsown:val document key)))
      (star.documents:validate-stored-document candidate)
      (process-outbox-mutation
       (lambda (document-id)
         (let ((current (couchdb-load-outbox-document client database document-id)))
           (unless (or committed-p
                       (equal expected-rev (outbox-object-value current "_rev")))
             (reject-file-content "file_revision_required_or_stale" 409))
           current))
       (lambda (state)
         (prog1 (couchdb-save-outbox-document client database state)
           (setf committed-p t)))
       publish-fn candidate :updated))))

(defun read-bounded-file-stream (stream)
  "Read at most the configured ceiling plus one sentinel; always close STREAM."
  (with-open-stream (input stream)
    (let* ((bytes (make-array (1+ *file-max-bytes*) :element-type '(unsigned-byte 8)))
           (count (read-sequence bytes input)))
      (when (> count *file-max-bytes*) (reject-file-content "file_too_large" 413))
      (subseq bytes 0 count))))

(defun couchdb-file-content (client database document)
  "Retrieve the server-owned attachment through the existing CouchDB client."
  (unless (equal (outbox-object-value document "storageId")
                 (format nil "couchdb:~a:content" (star.documents:document-id document)))
    (reject-file-content "file_content_unavailable" 404))
  (let* ((path (format nil "/~a/~a/content"
                       (quri:url-encode database)
                       (quri:url-encode (star.documents:document-id document))))
         (uri (quri:make-uri :path path
                             :query (quri:url-encode-params
                                     (list (cons "rev" (jsown:val document "_rev"))))))
         (stream (cl-couch:couchdb-request client uri :stream t :force-binary t)))
    (verify-file-content document (read-bounded-file-stream stream))))
