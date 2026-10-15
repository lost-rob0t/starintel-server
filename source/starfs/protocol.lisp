(in-package :star.starfs)

;;;; StarFS protocol: the storage boundary of the server.
;;;;
;;;; Only this package is visible above the storage boundary. It defines the
;;;; backend-neutral block-store port plus content addressing over SHA-256.
;;;; Backends implement the port; nothing above StarFS may import a backend
;;;; package. No backend becomes a semantic authority merely because it
;;;; stores bytes: the namespace layer above (a follow-up concern) stays the
;;;; only mutable mapping and backends only ever hold immutable content.

;; A parameter rather than DEFCONSTANT: SBCL rejects re-running DEFCONSTANT
;; on string literals across fasl reloads, and this value must survive the
;; image being reloaded by live-patch tooling.
(defparameter +content-id-prefix+ "sha256:"
  "Canonical content-id scheme prefix followed by 64 lowercase hex digits.")

(define-condition starfs-error (error)
  ()
  (:documentation "Base condition for all StarFS storage errors."))

(define-condition block-not-found (starfs-error)
  ((content-id :initarg :content-id
               :reader block-not-found-content-id
               :initform nil))
  (:documentation "Signaled when a block address has no stored content."))

(define-condition block-integrity-error (starfs-error)
  ((content-id :initarg :content-id
               :reader block-integrity-error-content-id
               :initform nil))
  (:documentation "Signaled when stored bytes do not hash to their address."))

(define-condition content-authentication-error (starfs-error)
  ()
  (:documentation "Signaled when AEAD authentication fails on decryption: wrong key, forged record, or corrupted ciphertext."))

(setf (documentation 'block-not-found-content-id 'function)
      "The =content-id= slot of =block-not-found=.")
(setf (documentation 'block-integrity-error-content-id 'function)
      "The =content-id= slot of =block-integrity-error=.")

(defun content-id-p (object)
  "Return true when OBJECT is a canonical StarFS content id: the sha256 prefix followed by exactly 64 lowercase hex digits."
  (and (stringp object)
       (> (length object) 64)
       (string= object +content-id-prefix+ :end1 (length +content-id-prefix+))
       (let ((hex-length (- (length object)
                            (length +content-id-prefix+))))
         (and (= 64 hex-length)
              (every (lambda (character)
                       (or (digit-char-p character 16)
                           ;; digit-char-p accepts uppercase too; content
                           ;; ids are lowercase only.
                           nil))
                     (subseq object (length +content-id-prefix+)))
              (string= (subseq object (length +content-id-prefix+))
                       (string-downcase
                        (subseq object (length +content-id-prefix+))))))))

(defun content-id-from-bytes (bytes)
  "Compute the canonical content id of BYTES: the sha256 hex digest of the exact octets. Content ids never derive from anything but the stored octets."
  (check-type bytes (vector (unsigned-byte 8)))
  (concatenate 'string
               +content-id-prefix+
               (ironclad:byte-array-to-hex-string
                (ironclad:digest-sequence :sha256 bytes))))

(defun content-id-hex (content-id)
  "Return the 64-hex-digit body of a valid CONTENT-ID."
  (unless (content-id-p content-id)
    (error 'block-not-found :content-id content-id))
  (subseq content-id (length +content-id-prefix+)))

(defclass block-store ()
  ()
  (:documentation "Protocol base class for StarFS block-store backends."))

(defgeneric put-block (store bytes)
  (:documentation
   "Store BYTES immutably in STORE under their content id and return that content id. Putting the same bytes again is idempotent: it returns the same content id without duplicating storage. Backends must make partial writes invisible: a block is gettable only after it is complete."))

(defgeneric get-block (store content-id)
  (:documentation
   "Return the bytes stored under CONTENT-ID in STORE. Verify that the stored bytes hash back to CONTENT-ID and signal block-integrity-error otherwise; signal block-not-found when the address has no content."))

(defgeneric block-exists-p (store content-id)
  (:documentation
   "Return true when STORE holds complete content addressed by CONTENT-ID. Partially written blocks do not exist."))

(defgeneric delete-block (store content-id)
  (:documentation
   "Delete the block addressed by CONTENT-ID from STORE. Return true when content was deleted and nil when the address was already absent, so deletion is idempotent. Deleting unknown addresses never signals."))
