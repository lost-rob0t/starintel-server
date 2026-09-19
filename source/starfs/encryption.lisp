(in-package :star.starfs)

;;;; Encrypted content store over any block-store backend.
;;;;
;;;; End-to-end content encryption with explicit caller-supplied keys. A
;;;; logical content record is stored as one immutable block:
;;;;   nonce (16 octets) || hmac-sha256 (32 octets) || aes-256-ctr ciphertext
;;;; The block address is the SHA-256 of that record, so:
;;;; - at rest and in transit above the backend only ciphertext exists;
;;;; - content ids never derive from plaintext and are randomized per write
;;;;   (a fresh counter nonce every put), so identical plaintext never shares
;;;;   an address and content ids cannot serve as a cross-tenant dedup
;;;;   oracle;
;;;; - encryption-then-MAC over (nonce || ciphertext) with a MAC subkey
;;;;   derived from the caller key means a wrong key, a forged record at a
;;;;   valid address, or corrupted ciphertext fails closed before any
;;;;   decryption is trusted.
;;;; Key separation is by SHA-256 domain separation of the caller key:
;;;;   encryption key = sha256(key || 0x01), mac key = sha256(key || 0x02).
;;;; Key distribution, rotation, and tenant->key assignment are NOT modeled
;;;; here: the caller owns the key for its tenant boundary.

(defconstant +nonce-octets+ 16
  "AES-CTR initial counter block length in octets for StarFS content records.")

(defconstant +mac-octets+ 32
  "HMAC-SHA-256 authentication tag length in octets for StarFS content records.")

(defconstant +key-octets+ 32
  "Caller key length in octets required for StarFS content records.")

(defconstant +subkey-domain-encryption+ 1
  "Domain-separation octet appended to the caller key for the AES key.")

(defconstant +subkey-domain-mac+ 2
  "Domain-separation octet appended to the caller key for the HMAC key.")

(defclass content-store ()
  ((backend :initarg :backend
            :reader %content-store-backend
            :documentation "The block-store backend holding ciphertext records."))
  (:documentation
   "End-to-end encrypted logical content view over a block-store backend."))

(defun make-content-store (backend)
  "Wrap BLOCK-STORE backend BACKEND so put-content/get-content store and fetch encrypted content records."
  (check-type backend block-store)
  (make-instance 'content-store :backend backend))

(defun validate-content-key (key)
  "Signal starfs-error unless KEY is a 32-octet key octet vector."
  (unless (and (vectorp key)
               (= (length key) +key-octets+)
               (every (lambda (octet)
                        (typep octet '(unsigned-byte 8)))
                      key))
    (error 'starfs-error
           :format-control
           "StarFS content key must be a ~d-octet (unsigned-byte 8) vector"
           :format-arguments (list +key-octets+)))
  key)

(defun derive-subkey (key domain)
  "SHA-256 domain-separated subkey of KEY for DOMAIN (encrypt/mac)."
  (let ((material (make-array (1+ +key-octets+)
                              :element-type '(unsigned-byte 8))))
    (replace material key)
    (setf (aref material +key-octets+) domain)
    (ironclad:digest-sequence :sha256 material)))

(defun content-record (nonce mac ciphertext)
  "Assemble one StarFS content record: nonce || mac || ciphertext."
  (let ((record
          (make-array (+ +nonce-octets+ +mac-octets+ (length ciphertext))
                      :element-type '(unsigned-byte 8))))
    (replace record nonce)
    (replace record mac :start1 +nonce-octets+)
    (replace record ciphertext
             :start1 (+ +nonce-octets+ +mac-octets+))
    record))

(defun split-content-record (record)
  "Split RECORD into (values nonce mac ciphertext), validating shape. A zero-length ciphertext is legitimate (empty content)."
  (unless (and (vectorp record)
               (>= (length record) (+ +nonce-octets+ +mac-octets+)))
    (error 'block-integrity-error))
  (values (subseq record 0 +nonce-octets+)
          (subseq record +nonce-octets+ (+ +nonce-octets+ +mac-octets+))
          (subseq record (+ +nonce-octets+ +mac-octets+))))

(defun record-mac (mac-key nonce ciphertext)
  "HMAC-SHA-256 over (NONCE || CIPHERTEXT) with MAC-KEY (encrypt-then-MAC)."
  (let ((mac (ironclad:make-mac :hmac mac-key :sha256)))
    (ironclad:update-mac mac nonce)
    (when (plusp (length ciphertext))
      (ironclad:update-mac mac ciphertext))
    (ironclad:produce-mac mac)))

(defun constant-time-octet-vector-equal (a b)
  "Compare octet vectors A and B without short-circuiting on the first differing octet."
  (and (= (length a) (length b))
       (let ((difference 0))
         (dotimes (index (length a))
           (setf difference
                 (logior difference
                         (logxor (aref a index) (aref b index)))))
         (zerop difference))))

(defun ctr-encrypt (key nonce plaintext)
  "AES-256-CTR encrypt PLAINTEXT under KEY and NONCE; returns fresh ciphertext."
  (let ((ciphertext (make-array (length plaintext)
                                :element-type '(unsigned-byte 8))))
    (when (plusp (length plaintext))
      (let ((cipher
              (ironclad:make-cipher
               :aes :mode :ctr
               :key (derive-subkey key +subkey-domain-encryption+)
               :initialization-vector nonce)))
        (ironclad:encrypt cipher plaintext ciphertext)))
    ciphertext))

(defun put-content (store plaintext &key key)
  "Encrypt PLAINTEXT with KEY (fresh nonce every call) and store the record immutably via STORE's backend. Returns the content id of the stored ciphertext record."
  (validate-content-key key)
  (let* ((nonce (ironclad:random-data +nonce-octets+))
         (ciphertext (ctr-encrypt key nonce plaintext))
         (mac (record-mac (derive-subkey key +subkey-domain-mac+)
                          nonce ciphertext)))
    (put-block (%content-store-backend store)
               (content-record nonce mac ciphertext))))

(defun get-content (store content-id &key key)
  "Fetch the record addressed by CONTENT-ID through STORE's backend, verify its authentication tag with KEY, and return the plaintext. Failures fail closed with content-authentication-error."
  (validate-content-key key)
  (let ((record (get-block (%content-store-backend store) content-id)))
    (multiple-value-bind (nonce mac ciphertext)
        (split-content-record record)
      (unless (constant-time-octet-vector-equal
               mac
               (record-mac (derive-subkey key +subkey-domain-mac+)
                           nonce ciphertext))
        (error 'content-authentication-error))
      (ctr-encrypt key nonce ciphertext))))
