(in-package :star-server-tests)

(def-suite starfs-tests
  :description "Backend-neutral StarFS block-store contract and encrypted content store")

(in-suite starfs-tests)

;;;; Backend-neutral contract.
;;;;
;;;; The identical assertion suite runs against every StarFS block-store
;;;; backend so semantic drift cannot pass CI for one adapter while another
;;;; enforces the contract. It mirrors the lease-store contract suite. The
;;;; in-memory fake backend below is test scaffolding: it proves the contract
;;;; is backend-neutral without a second production persistence path.

(defstruct fake-block-store
  "In-memory contract-testing backend (test scaffolding only)."
  (blocks (make-hash-table :test 'equal))
  (put-count 0))

(defmethod star.starfs:put-block ((store fake-block-store) bytes)
  (let ((content-id (star.starfs:content-id-from-bytes bytes)))
    (unless (gethash content-id (fake-block-store-blocks store))
      (incf (fake-block-store-put-count store)))
    (setf (gethash content-id (fake-block-store-blocks store))
          (copy-seq bytes))
    content-id))

(defmethod star.starfs:get-block ((store fake-block-store) content-id)
  (multiple-value-bind (bytes found-p)
      (gethash content-id (fake-block-store-blocks store))
    (unless found-p
      (error 'star.starfs:block-not-found :content-id content-id))
    (unless (string= content-id (star.starfs:content-id-from-bytes bytes))
      (error 'star.starfs:block-integrity-error :content-id content-id))
    (copy-seq bytes)))

(defmethod star.starfs:block-exists-p ((store fake-block-store) content-id)
  (multiple-value-bind (bytes found-p)
      (gethash content-id (fake-block-store-blocks store))
    (and found-p
         (string= content-id (star.starfs:content-id-from-bytes bytes)))))

(defmethod star.starfs:delete-block ((store fake-block-store) content-id)
  (if (remhash content-id (fake-block-store-blocks store))
      t
      nil))

(defstruct starfs-contract-fixture
  "Everything the backend-neutral contract needs from one backend."
  (store nil)
  (stored-count nil :type (or null function))
  (corrupt-stored nil :type (or null function))
  (inject-partial-write nil :type (or null function)))

(defparameter *starfs-test-roots* nil
  "Local store roots created by tests; removed by cleanup test.")

(defun starfs-test-root ()
  (let ((root
          (merge-pathnames
           (format nil "starfs-test-~A/" (string-downcase (star.ids:ulid)))
           (uiop:ensure-directory-pathname (uiop:temporary-directory)))))
    (push root *starfs-test-roots*)
    root))

(defun make-local-contract-fixture ()
  (let* ((store (star.starfs:make-local-block-store :root (starfs-test-root)))
         (root (star.starfs:local-block-store-root store)))
    (labels ((hex-of (content-id)
               (subseq content-id
                       (length star.starfs:+content-id-prefix+)))
             (block-file-path (content-id &optional (type "blk"))
               (let ((hex (hex-of content-id)))
                 (merge-pathnames
                  (make-pathname
                   :directory (append (pathname-directory root)
                                      (list (subseq hex 0 2)))
                   :name hex
                   :type type)
                  root)))
             (fan-out-file-count ()
               (loop for dir in (uiop:subdirectories root)
                     sum (length (uiop:directory-files dir)))))
      (make-starfs-contract-fixture
       :store store
       :stored-count #'fan-out-file-count
       :corrupt-stored
       (lambda (content-id)
         (let ((path (block-file-path content-id)))
           (with-open-file (stream path :element-type '(unsigned-byte 8))
             (let ((bytes (make-array (file-length stream)
                                      :element-type '(unsigned-byte 8))))
               (read-sequence bytes stream)
               (setf (aref bytes 0) (logxor (aref bytes 0) #xff))
               (with-open-file (out path
                                    :direction :output
                                    :element-type '(unsigned-byte 8)
                                    :if-exists :supersede)
                 (write-sequence bytes out))))))
       :inject-partial-write
       (lambda (content-id bytes)
         (let* ((hex (hex-of content-id))
                (partial (block-file-path content-id "blk-tmp")))
           (ensure-directories-exist
            (uiop:pathname-directory-pathname partial))
           (with-open-file (out partial
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :element-type '(unsigned-byte 8))
             (write-sequence
              (subseq bytes 0 (max 1 (floor (length bytes) 2))) out))
           partial))))))

(defun make-fake-contract-fixture ()
  (let ((store (make-fake-block-store)))
    (make-starfs-contract-fixture
     :store store
     :stored-count (lambda () (fake-block-store-put-count store))
     :corrupt-stored
     (lambda (content-id)
       (let ((bytes (gethash content-id (fake-block-store-blocks store))))
         (setf (aref bytes 0) (logxor (aref bytes 0) #xff))
         (setf (gethash content-id (fake-block-store-blocks store)) bytes))))))

(defun starfs-sample-bytes (size seed)
  (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
    (loop for index below size
          do (setf (aref bytes index)
                   (mod (+ (* index 31) seed 7) 256)))
    bytes))

(defun assert-backend-neutral-starfs-contract (fixture)
  "Run the full backend-neutral StarFS contract against FIXTURE's store."
  (let ((store (starfs-contract-fixture-store fixture))
        (stored-count (starfs-contract-fixture-stored-count fixture))
        (corrupt-stored (starfs-contract-fixture-corrupt-stored fixture))
        (inject-partial-write
          (starfs-contract-fixture-inject-partial-write fixture)))
    ;; Roundtrip.
    (let* ((bytes (starfs-sample-bytes 64 1))
           (content-id (star.starfs:put-block store bytes)))
      (is (star.starfs:content-id-p content-id))
      (is (equalp bytes (star.starfs:get-block store content-id)))
      (is-true (star.starfs:block-exists-p store content-id))
      ;; Idempotent put: same bytes -> same content id, no duplicate storage.
      (is (string= content-id (star.starfs:put-block store bytes)))
      (when stored-count
        (is (= 1 (funcall stored-count))))
      ;; Delete semantics.
      (is-true (star.starfs:delete-block store content-id))
      (is-false (star.starfs:block-exists-p store content-id))
      (signals star.starfs:block-not-found
        (star.starfs:get-block store content-id))
      (is-false (star.starfs:delete-block store content-id)))
    ;; Empty and binary edge payloads roundtrip.
    (dolist (size (list 0 1 4096))
      (let* ((bytes (starfs-sample-bytes size size))
             (content-id (star.starfs:put-block store bytes)))
        (is (equalp bytes (star.starfs:get-block store content-id)))))
    ;; Missing block signals block-not-found and never fabricates content.
    (signals star.starfs:block-not-found
      (star.starfs:get-block
       store (star.starfs:content-id-from-bytes
              (starfs-sample-bytes 16 99))))
    ;; Integrity on retrieval: out-of-band corruption must be detected. A
    ;; corrupt path is not a complete block and a retry must repair it from
    ;; the caller's bytes instead of returning a false idempotent success.
    (let* ((bytes (starfs-sample-bytes 128 3))
           (content-id (star.starfs:put-block store bytes)))
      (funcall corrupt-stored content-id)
      (is-false (star.starfs:block-exists-p store content-id))
      (signals star.starfs:block-integrity-error
        (star.starfs:get-block store content-id))
      (is (string= content-id (star.starfs:put-block store bytes)))
      (is-true (star.starfs:block-exists-p store content-id))
      (is (equalp bytes (star.starfs:get-block store content-id))))
    ;; Atomic visibility: a partially written temp file is invisible.
    (when inject-partial-write
      (let* ((bytes (starfs-sample-bytes 48 5))
             (content-id (star.starfs:content-id-from-bytes bytes)))
        (funcall inject-partial-write content-id bytes)
        (is-false (star.starfs:block-exists-p store content-id))
        (signals star.starfs:block-not-found
          (star.starfs:get-block store content-id))
        (is (string= content-id (star.starfs:put-block store bytes)))
        (is (equalp bytes (star.starfs:get-block store content-id)))))))

(test starfs-local-backend-satisfies-contract
  (assert-backend-neutral-starfs-contract (make-local-contract-fixture)))

(test starfs-memory-fake-backend-satisfies-contract
  (assert-backend-neutral-starfs-contract (make-fake-contract-fixture)))

;;;; Encrypted content store: end-to-end content encryption with explicit
;;;; keys; content IDs address ciphertext so they leak no plaintext-derived
;;;; metadata and are randomized per write (no cross-tenant dedup oracle).

(defparameter *starfs-test-key*
  (make-array 32 :element-type '(unsigned-byte 8)
                :initial-contents (loop for i below 32 collect i)))

(defmacro with-content-store ((store-var &optional backend-var) &body body)
  (let ((backend (or backend-var (gensym "BACKEND"))))
    `(let* ((,backend
              (star.starfs:make-local-block-store :root (starfs-test-root)))
            (,store-var (star.starfs:make-content-store ,backend)))
       ,@body)))

(test content-store-roundtrips-plaintext
  (with-content-store (store backend)
    (dolist (size (list 0 1 64 4096))
      (let* ((plaintext (starfs-sample-bytes size (+ size 11)))
             (content-id (star.starfs:put-content
                          store plaintext :key *starfs-test-key*)))
         (is (equalp plaintext
                    (star.starfs:get-content
                     store content-id :key *starfs-test-key*)))
         (is-true (star.starfs:block-exists-p backend content-id))))))

(test content-store-stores-only-ciphertext
  (with-content-store (store backend)
    (let* ((plaintext (starfs-sample-bytes 96 21))
           (content-id (star.starfs:put-content
                        store plaintext :key *starfs-test-key*))
           (stored (star.starfs:get-block backend content-id)))
      (is (not (equalp plaintext stored)))
      ;; Content ID addresses ciphertext, never plaintext-derived material.
      (is (not (string= content-id
                        (star.starfs:content-id-from-bytes plaintext))))
      (is (string= content-id
                   (star.starfs:content-id-from-bytes stored))))))

(test content-store-fresh-nonce-per-write
  (with-content-store (store backend)
    (let ((plaintext (starfs-sample-bytes 96 33)))
      (let ((first-id (star.starfs:put-content
                       store plaintext :key *starfs-test-key*))
            (second-id (star.starfs:put-content
                        store plaintext :key *starfs-test-key*)))
        ;; Randomized encryption: identical plaintext never yields a shared
        ;; address, so content IDs cannot become a cross-tenant dedup oracle.
        (is (not (string= first-id second-id)))
        (is (not (equalp (star.starfs:get-block backend first-id)
                        (star.starfs:get-block backend second-id))))
        (is (equalp plaintext
                   (star.starfs:get-content
                    store first-id :key *starfs-test-key*)))
        (is (equalp plaintext
                   (star.starfs:get-content
                    store second-id :key *starfs-test-key*)))))))

(test content-store-wrong-key-fails-closed
  (with-content-store (store)
    (let* ((plaintext (starfs-sample-bytes 96 44))
           (content-id (star.starfs:put-content
                        store plaintext :key *starfs-test-key*))
           (wrong-key
             (make-array 32 :element-type '(unsigned-byte 8)
                           :initial-contents
                           (loop for i below 32 collect (mod (+ i 1) 256)))))
      (signals star.starfs:content-authentication-error
        (star.starfs:get-content store content-id :key wrong-key)))))

(test content-store-forged-record-fails-authentication
  (with-content-store (store backend)
    (let* ((plaintext (starfs-sample-bytes 96 55))
           (content-id (star.starfs:put-content
                        store plaintext :key *starfs-test-key*))
           (record (star.starfs:get-block backend content-id))
           (forged (copy-seq record)))
      ;; A record with a valid address but tampered ciphertext must fail
      ;; authentication rather than decrypt to garbage. The ciphertext starts
      ;; after nonce (16) and mac (32).
      (setf (aref forged (+ 48 (floor (length record) 2)))
            (logxor (aref forged (+ 48 (floor (length record) 2))) #x01))
      (let ((forged-id (star.starfs:put-block backend forged)))
        (signals star.starfs:content-authentication-error
          (star.starfs:get-content
           store forged-id :key *starfs-test-key*))))))

(test content-store-rejects-invalid-keys
  (with-content-store (store)
    (signals star.starfs:starfs-error
      (star.starfs:put-content
       store (starfs-sample-bytes 16 66)
       :key (make-array 16 :element-type '(unsigned-byte 8))))))

(test starfs-cleanup-test-roots
  (let ((roots *starfs-test-roots*))
    (setf *starfs-test-roots* nil)
    (dolist (root roots)
      (when (uiop:directory-exists-p root)
        (uiop:delete-directory-tree root :validate t)))))