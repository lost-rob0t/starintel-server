(in-package :star.storage)

(defparameter +file-ingest-extension-key+ "starintel.file_ingest.v1"
  "Transient trusted-actor extension carrying one bounded base64 file payload.")

(defparameter +file-storage-extension-key+ "_server_file"
  "Server-private extension locating the durable raw file object.")

(define-condition file-artifact-validation-error (error)
  ((reason :initarg :reason :reader file-artifact-validation-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid file artifact: ~a"
             (file-artifact-validation-reason condition)))))

(defun file-artifact-error (control &rest arguments)
  (error 'file-artifact-validation-error
         :reason (apply #'format nil control arguments)))

(defun file-document-p (document)
  (string= (or (star.documents:document-dtype document) "") "file"))

(defun file-extensions-object (document)
  (let ((extensions (and (jsown:keyp document "extensions")
                         (jsown:val document "extensions"))))
    (and (json-object-p extensions) extensions)))

(defun file-extension-object (document key)
  (let ((extensions (file-extensions-object document)))
    (and extensions
         (jsown:keyp extensions key)
         (let ((value (jsown:val extensions key)))
           (and (json-object-p value) value)))))

(defun remove-file-extension! (document key)
  (let ((extensions (file-extensions-object document)))
    (when (and extensions (jsown:keyp extensions key))
      (jsown:remkey extensions key)))
  document)

(defun ensure-file-data-object (document)
  (let ((data (and (jsown:keyp document "data")
                   (jsown:val document "data"))))
    (unless (json-object-p data)
      (file-artifact-error "file data must be an object"))
    data))

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun base64-value (character)
  (position character +base64-alphabet+ :test #'char=))

(defun decode-base64-strict (value &key max-bytes)
  "Decode canonical padded BASE64 VALUE to an octet vector with a hard size cap."
  (unless (stringp value)
    (file-artifact-error "content must be a base64 string"))
  (unless (zerop (mod (length value) 4))
    (file-artifact-error "base64 content length must be divisible by four"))
  (let* ((quartets (/ (length value) 4))
         (estimated (* quartets 3)))
    (when (and max-bytes (> estimated (+ max-bytes 2)))
      (file-artifact-error "encoded content exceeds configured file limit"))
    (let ((output
            (make-array 0
                        :element-type '(unsigned-byte 8)
                        :adjustable t
                        :fill-pointer 0)))
      (loop for offset from 0 below (length value) by 4
            for final-p = (= (+ offset 4) (length value))
            for c0 = (char value offset)
            for c1 = (char value (+ offset 1))
            for c2 = (char value (+ offset 2))
            for c3 = (char value (+ offset 3))
            for v0 = (base64-value c0)
            for v1 = (base64-value c1)
            for pad2 = (char= c2 #\=)
            for pad3 = (char= c3 #\=)
            for v2 = (and (not pad2) (base64-value c2))
            for v3 = (and (not pad3) (base64-value c3))
            do
               (unless (and v0 v1
                            (or pad2 v2)
                            (or pad3 v3))
                 (file-artifact-error "base64 content contains an invalid character"))
               (when (or (and pad2 (not pad3))
                         (and (or pad2 pad3) (not final-p)))
                 (file-artifact-error "base64 padding is invalid"))
               (vector-push-extend
                (logior (ash v0 2) (ash v1 -4))
                output)
               (unless pad2
                 (vector-push-extend
                  (logior (ash (logand v1 #x0f) 4)
                          (ash v2 -2))
                  output))
               (unless pad3
                 (vector-push-extend
                  (logior (ash (logand v2 #x03) 6) v3)
                  output)))
      (when (and max-bytes (> (length output) max-bytes))
        (file-artifact-error "decoded content exceeds configured file limit"))
      (coerce output '(simple-array (unsigned-byte 8) (*))))))

(defun encode-base64-octets (octets)
  "Encode OCTETS as padded RFC 4648 base64."
  (with-output-to-string (stream)
    (loop for offset from 0 below (length octets) by 3
          for remaining = (- (length octets) offset)
          for b0 = (aref octets offset)
          for b1 = (and (> remaining 1) (aref octets (+ offset 1)))
          for b2 = (and (> remaining 2) (aref octets (+ offset 2)))
          do
             (write-char
              (char +base64-alphabet+ (ldb (byte 6 2) b0))
              stream)
             (write-char
              (char +base64-alphabet+
                    (logior (ash (logand b0 #x03) 4)
                            (if b1 (ash b1 -4) 0)))
              stream)
             (if b1
                 (write-char
                  (char +base64-alphabet+
                        (logior (ash (logand b1 #x0f) 2)
                                (if b2 (ash b2 -6) 0)))
                  stream)
                 (write-char #\= stream))
             (if b2
                 (write-char
                  (char +base64-alphabet+ (logand b2 #x3f))
                  stream)
                 (write-char #\= stream)))))

(defun octets-sha256-hex (octets)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256 octets)))

(defun file-artifact-tier ()
  (normalize-storage-tier star::*file-artifact-storage-tier*))

(defun file-artifact-backend-name ()
  (let ((backend (tier-backend-name (file-artifact-tier))))
    (when (string= backend "couchdb")
      (error 'storage-backend-error
             :backend backend
             :operation :resolve
             :key ""
             :reason "raw file artifacts require an external storage backend"))
    backend))

(defun file-artifact-object-key (document digest name)
  (format nil "tenants/~a/datasets/~a/files/sha256/~a/~a"
          (safe-key-component (document-tenant document))
          (safe-key-component
           (or (star.documents:document-dataset document) "default"))
          (safe-key-component digest)
          (safe-key-component name)))

(defun file-storage-metadata (backend tier key digest size media-type)
  (jsown:new-js
    ("version" 1)
    ("backend" backend)
    ("tier" tier)
    ("object_key" key)
    ("content_sha256" digest)
    ("size_bytes" size)
    ("media_type" media-type)
    ("stored_at" (star.documents:utc-now))))

(defun set-file-storage-metadata! (document metadata)
  (setf (jsown:val (ensure-extensions! document)
                   +file-storage-extension-key+)
        metadata)
  document)

(defun file-storage-metadata-object (document)
  (file-extension-object document +file-storage-extension-key+))

(defun prepare-file-artifact-document (document)
  "Materialize a trusted inline file payload and return the stripped document.

Actors may supply only +FILE-INGEST-EXTENSION-KEY+. Any server-file placement
metadata supplied by an actor is discarded. The raw object is content-addressed
and written before the canonical document reaches the outbox/CouchDB commit."
  (let ((copy (clone-json document)))
    (remove-file-extension! copy +file-storage-extension-key+)
    (if (not (file-document-p copy))
        (progn
          (remove-file-extension! copy +file-ingest-extension-key+)
          copy)
        (let ((ingest (file-extension-object copy +file-ingest-extension-key+)))
          (if (null ingest)
              copy
              (let* ((encoding (jsown:val-safe ingest "encoding"))
                     (content (jsown:val-safe ingest "content"))
                     (declared-sha (jsown:val-safe ingest "sha256"))
                     (declared-size (jsown:val-safe ingest "size_bytes"))
                     (data (ensure-file-data-object copy))
                     (octets
                       (progn
                         (unless (and (stringp encoding)
                                      (string-equal encoding "base64"))
                           (file-artifact-error "encoding must be base64"))
                         (decode-base64-strict
                          content
                          :max-bytes star::*file-artifact-max-bytes*)))
                     (size (length octets))
                     (digest (octets-sha256-hex octets))
                     (document-sha (jsown:val-safe data "content_hash"))
                     (document-size (jsown:val-safe data "size_bytes"))
                     (name (or (jsown:val-safe data "name")
                               (star.documents:document-id copy)
                               "artifact"))
                     (media-type
                       (or (jsown:val-safe data "media_type")
                           "application/octet-stream"))
                     (tier (file-artifact-tier))
                     (backend-name (file-artifact-backend-name))
                     (key (file-artifact-object-key copy digest name)))
                (unless (and (integerp declared-size) (= declared-size size))
                  (file-artifact-error
                   "declared size ~s does not match decoded size ~d"
                   declared-size size))
                (unless (and (stringp declared-sha)
                             (string-equal declared-sha digest))
                  (file-artifact-error "declared sha256 does not match content"))
                (unless (and (stringp document-sha)
                             (string-equal document-sha digest))
                  (file-artifact-error "file data.content_hash does not match content"))
                (unless (and (integerp document-size)
                             (= document-size size))
                  (file-artifact-error "file data.size_bytes does not match content"))
                (storage-put
                 (resolve-storage-backend backend-name)
                 key
                 octets
                 :content-type media-type
                 :metadata
                 (list :tenant (document-tenant copy)
                       :dataset (star.documents:document-dataset copy)
                       :document-id (star.documents:document-id copy)
                       :content-sha256 digest))
                (setf (jsown:val data "uri")
                      (format nil "starintel-file:~a"
                              (star.documents:document-id copy)))
                (remove-file-extension! copy +file-ingest-extension-key+)
                (set-file-storage-metadata!
                 copy
                 (file-storage-metadata
                  backend-name tier key digest size media-type))))))))

(defun load-file-artifact (document)
  "Return raw OCTETS and server metadata for a materialized file document."
  (unless (file-document-p document)
    (file-artifact-error "document is not dtype=file"))
  (let ((metadata (file-storage-metadata-object document)))
    (unless metadata
      (file-artifact-error "file document has no server-owned artifact"))
    (let* ((backend-name (jsown:val-safe metadata "backend"))
           (key (jsown:val-safe metadata "object_key"))
           (value
             (storage-get
              (resolve-storage-backend backend-name)
              key)))
      (values
       (etypecase value
         (string (babel:string-to-octets value :encoding :latin-1))
         ((vector (unsigned-byte 8)) value))
       metadata))))

(defun file-artifact-json (document)
  "Build a bounded JSON transfer envelope for a stored file artifact."
  (multiple-value-bind (octets metadata)
      (load-file-artifact document)
    (jsown:new-js
      ("file_id" (star.documents:document-id document))
      ("media_type" (or (jsown:val-safe metadata "media_type")
                        "application/octet-stream"))
      ("content_hash" (jsown:val-safe metadata "content_sha256"))
      ("size_bytes" (length octets))
      ("encoding" "base64")
      ("content" (encode-base64-octets octets)))))
