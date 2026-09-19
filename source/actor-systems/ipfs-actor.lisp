;; SPDX-License-Identifier: GPL-3.0-or-later
(uiop:define-package :star.actors.ipfs
  (:use :cl :sento.actor :sento.actor-context)
  (:export
   #:*ipfs-actor*
   #:publish-export-manifest
   #:publish-export
   #:start-ipfs-actor)
  (:documentation
   "Local Sento actor for publishing completed StarIntel-owned export artifacts to IPFS."))

(in-package :star.actors.ipfs)

(defvar *ipfs-actor* nil
  "Local Sento actor registered as =ipfs= in the StarIntel actor index.")

(defstruct (ipfs-publish-command
            (:constructor make-ipfs-publish-command (manifest)))
  manifest)

(defun file-byte-length (path)
  (with-open-file (stream path
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun file-sha256 (path)
  (string-downcase
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-file :sha256 path))))

(defun non-empty-string-p (value)
  (and (stringp value) (plusp (length value))))

(defun parse-ipfs-size (value fallback)
  (cond
    ((integerp value) value)
    ((stringp value)
     (handler-case (parse-integer value :junk-allowed nil)
       (error () fallback)))
    (t fallback)))

(defun last-non-empty-line (text)
  (car
   (last
    (remove-if
     (lambda (line)
       (zerop
        (length
         (string-trim '(#\Space #\Tab #\Newline #\Return) line))))
     (uiop:split-string text :separator '(#\Newline #\Return))))))

(defun parse-ipfs-add-response (body)
  (unless (stringp body)
    (error "IPFS add returned a non-text response"))
  (let ((line (last-non-empty-line body)))
    (unless line
      (error "IPFS add returned an empty response"))
    (jsown:parse line)))

(defun ipfs-add-file (path)
  (let* ((base (string-right-trim "/" star:*ipfs-api-url*))
         (url
           (format nil
                   "~a/api/v0/add?pin=true&cid-version=1&wrap-with-directory=false"
                   base)))
    (multiple-value-bind (body status)
        (dexador:post
         url
         :content (list (cons "file" (pathname path)))
         :connect-timeout star:*ipfs-request-timeout-seconds*
         :read-timeout star:*ipfs-request-timeout-seconds*
         :force-string t)
      (unless (and (integerp status) (<= 200 status 299))
        (error "IPFS add failed with HTTP status ~a" status))
      (parse-ipfs-add-response body))))

(defun verify-export-manifest-artifact (manifest export-root)
  (unless (and (consp manifest) (eq :obj (first manifest)))
    (error "IPFS publish requires a StarIntel export manifest"))
  (let* ((path-value (jsown:val-safe manifest "path"))
         (expected-bytes (jsown:val-safe manifest "bytes"))
         (expected-sha256 (jsown:val-safe manifest "sha256"))
         (path (and (stringp path-value) (pathname path-value))))
    (unless (and path
                 (star.exports:export-artifact-owned-p
                  path :export-root export-root))
      (error "Export artifact is outside the server-owned export root"))
    (unless (and (integerp expected-bytes)
                 (<= 0 expected-bytes star:*ipfs-max-publish-bytes*))
      (error "Export artifact size is outside the configured IPFS bound"))
    (let ((actual-bytes (file-byte-length path)))
      (unless (= expected-bytes actual-bytes)
        (error "Export artifact byte count changed before IPFS publish"))
      (unless (and (non-empty-string-p expected-sha256)
                   (string-equal expected-sha256 (file-sha256 path)))
        (error "Export artifact SHA-256 changed before IPFS publish")))
    path))

(defun publish-export-manifest
    (manifest
     &key
       (export-root star:*export-root*)
       (add-fn #'ipfs-add-file))
  "Publish one completed export MANIFEST through IPFS and return a CID receipt.

The artifact path must resolve beneath EXPORT-ROOT, its byte count and SHA-256
must still match the export receipt, and its size must not exceed
=*ipfs-max-publish-bytes*=. ADD-FN is injectable for hermetic tests and receives
only the validated server-owned pathname. The configured IPFS endpoint is
operator-owned; callers cannot supply a remote URL."
  (let* ((path (verify-export-manifest-artifact manifest export-root))
         (response (funcall add-fn path))
         (cid (jsown:val-safe response "Hash")))
    (unless (non-empty-string-p cid)
      (error "IPFS add response is missing Hash/CID"))
    (jsown:new-js
      ("status" "published")
      ("action" "publish-export")
      ("export_id" (jsown:val-safe manifest "export_id"))
      ("dataset" (jsown:val-safe manifest "dataset"))
      ("cid" cid)
      ("bytes"
       (parse-ipfs-size
        (jsown:val-safe response "Size")
        (jsown:val manifest "bytes")))
      ("sha256" (jsown:val manifest "sha256"))
      ("license" star.http.contract:+software-license+)
      ("license_scope" "server-software")
      ("source_repository" star.http.contract:+source-repository+))))

(defun maybe-log-ipfs-event (event-type manifest details)
  (when star.actors:*actor-event-receiver*
    (ignore-errors
      (star.actors:log-actor-event
       "ipfs"
       :component "ipfs"
       :event-type event-type
       :details details
       :source-id (or (jsown:val-safe manifest "export_id") "")))))

(defun complete-ipfs-request (result)
  (when *sender*
    (reply result *sender*))
  result)

(defun ipfs-receive (message)
  (handler-case
      (typecase message
        (ipfs-publish-command
         (let* ((manifest (ipfs-publish-command-manifest message))
                (result (publish-export-manifest manifest)))
           (maybe-log-ipfs-event
            "export.published"
            manifest
            (format nil "cid=~a" (jsown:val result "cid")))
           (complete-ipfs-request result)))
        (t
         (log:warn
          "IPFS actor rejected non-internal message of type ~a"
          (type-of message))
         (complete-ipfs-request
          (jsown:new-js
            ("status" "rejected")
            ("code" "invalid_ipfs_command")))))
    (error (condition)
      (log:error "IPFS actor publish failed: ~a" condition)
      (let ((manifest
              (and (typep message 'ipfs-publish-command)
                   (ipfs-publish-command-manifest message))))
        (when manifest
          (maybe-log-ipfs-event
           "export.publish-failed"
           manifest
           (princ-to-string condition))))
      (complete-ipfs-request
       (jsown:new-js
         ("status" "error")
         ("code" "ipfs_publish_failed")
         ("message" (princ-to-string condition)))))))

(defun start-ipfs-actor ()
  "Start and register the local =ipfs= actor in the existing actor index."
  (setf *ipfs-actor*
        (actor-of
         star.actors:*sys*
         :name "ipfs"
         :receive #'ipfs-receive))
  (star.actors:register-actor "ipfs" *ipfs-actor*)
  *ipfs-actor*)

(defun publish-export (manifest)
  "Publish MANIFEST through the running IPFS actor and return its successful receipt.

This API wraps the actor in an internal command type, so target JSON delivered
through the generic target router cannot smuggle arbitrary filesystem paths
into the IPFS publisher."
  (unless *ipfs-actor*
    (error "IPFS actor is not running"))
  (let ((result
          (sento.actor:ask-s
           *ipfs-actor*
           (make-ipfs-publish-command manifest)
           :time-out star:*ipfs-request-timeout-seconds*)))
    (unless (and (consp result)
                 (eq :obj (first result))
                 (string= "published"
                          (or (jsown:val-safe result "status") "")))
      (error "IPFS actor rejected export: ~a"
             (if (and (consp result) (eq :obj (first result)))
                 (or (jsown:val-safe result "message")
                     (jsown:val-safe result "code")
                     "unknown error")
                 result)))
    result))

(nhooks:add-hook star:*actors-start-hook* #'start-ipfs-actor)
