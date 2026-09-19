;; SPDX-License-Identifier: GPL-3.0-or-later
(uiop:define-package :star.exports
  (:use :cl)
  (:export
   #:create-dataset-export
   #:export-receipt-json
   #:export-artifact-owned-p)
  (:documentation
   "Server-owned dataset export artifacts and stable export receipts."))

(in-package :star.exports)

(defun export-unix-now ()
  (- (get-universal-time) 2208988800))

(defun consistency-name (value)
  (string-downcase
   (etypecase value
     (string value)
     (symbol (symbol-name value)))))

(defun export-artifact-path (export-id &key (export-root star:*export-root*))
  (merge-pathnames
   (format nil "~a.jsonl" export-id)
   (uiop:ensure-directory-pathname (pathname export-root))))

(defun path-components-prefix-p (prefix candidate)
  (let ((prefix-directory (pathname-directory prefix))
        (candidate-directory (pathname-directory candidate)))
    (and (<= (length prefix-directory) (length candidate-directory))
         (equal prefix-directory
                (subseq candidate-directory 0 (length prefix-directory))))))

(defun export-artifact-owned-p (path &key (export-root star:*export-root*))
  "Return true only when existing PATH resolves beneath the server EXPORT-ROOT.

Both paths are canonicalized through =TRUENAME= before comparison, preventing
symlink and =..= traversal from turning an IPFS publish into arbitrary file
access."
  (let* ((root (uiop:ensure-directory-pathname (pathname export-root)))
         (root-true (and (probe-file root) (truename root)))
         (path-true (and (probe-file path) (truename path))))
    (and root-true
         path-true
         (path-components-prefix-p root-true path-true))))

(defun export-result-manifest (export-id dataset result created-at)
  (jsown:new-js
    ("export_id" export-id)
    ("dataset" dataset)
    ("path" (getf result :path))
    ("format" (getf result :format))
    ("exported" (getf result :exported))
    ("bytes" (getf result :bytes))
    ("sha256" (getf result :sha256))
    ("consistency" (consistency-name (getf result :consistency)))
    ("created_at" created-at)
    ("status" "ready")
    ("license" (getf result :license))
    ("license_scope" (getf result :license-scope))
    ("source_repository" (getf result :source-repository))))

(defun create-dataset-export
    (client database dataset
     &key
       (export-root star:*export-root*)
       (page-size star:*export-page-size*)
       (created-at (export-unix-now))
       (query-fn #'star.databases.couchdb:query-view))
  "Create one server-owned JSONL DATASET export and return its internal manifest.

The caller supplies CouchDB CLIENT and DATABASE but never a destination path.
A server-generated ULID selects the artifact filename under EXPORT-ROOT. The
underlying exporter remains an atomic monotonic key scan. On failure this
function signals an error and removes no previously completed export."
  (unless (and (stringp dataset) (<= 1 (length dataset) 256))
    (error "Dataset must be a non-empty string of at most 256 characters"))
  (unless (and (integerp page-size) (plusp page-size)
               (<= page-size star:*export-max-page-size*))
    (error "Export page size is outside the configured bound"))
  (let* ((root (uiop:ensure-directory-pathname (pathname export-root)))
         (export-id (star.ids:ulid))
         (path (export-artifact-path export-id :export-root root)))
    (ensure-directories-exist path)
    (let ((result
            (star.databases.couchdb:export-by-dataset*
             client database dataset path
             :page-size page-size
             :query-fn query-fn)))
      (unless (getf result :ok)
        (error "Dataset export failed: ~a"
               (or (getf result :error) "unknown export failure")))
      (export-result-manifest export-id dataset result created-at))))

(defun export-receipt-json (manifest &key cid correlation-id)
  "Return the public export receipt for MANIFEST without leaking its file path."
  (let ((receipt
          (jsown:new-js
            ("export_id" (jsown:val manifest "export_id"))
            ("dataset" (jsown:val manifest "dataset"))
            ("format" (jsown:val manifest "format"))
            ("exported" (jsown:val manifest "exported"))
            ("bytes" (jsown:val manifest "bytes"))
            ("sha256" (jsown:val manifest "sha256"))
            ("consistency" (jsown:val manifest "consistency"))
            ("created_at" (jsown:val manifest "created_at"))
            ("status" (if cid "published" (jsown:val manifest "status")))
            ("license" (jsown:val manifest "license"))
            ("license_scope" (jsown:val manifest "license_scope"))
            ("source_repository" (jsown:val manifest "source_repository")))))
    (when cid
      (setf (jsown:val receipt "cid") cid))
    (when correlation-id
      (setf (jsown:val receipt "correlation_id") correlation-id))
    receipt))
