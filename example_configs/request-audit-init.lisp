;;; Load from the trusted STAR_SERVER_INIT_FILE, before starting workers/HTTP.
;;; Provision one dedicated request-audit database per tenant first. No automatic activation.
(in-package #:cl-user)
(asdf:load-system :starintel-request-audit)

(defun request-audit-secret (path)
  (handler-case
      (string-right-trim '(#\Newline #\Return) (uiop:read-file-string path))
    (error () (error "Cannot read request-audit secret file"))))

(defun request-audit-hmac-key ()
  (handler-case
      (ironclad:hex-string-to-byte-array
       (request-audit-secret "/run/secrets/request-audit-hmac.hex"))
    (error () (error "Invalid request-audit HMAC key"))))

;; Strict Boolean. NIL means audit only; changing this to T is the explicit opt-in.
(defparameter *request-audit-refusals-enabled* nil)
(defvar *request-audit-uninstall* nil)

;; Optional operator rule. Do not infer permission/consent from model or request text.
(defun operator-request-policy (local model)
  (star.request-audit:default-policy local model))

(when *request-audit-uninstall*
  (funcall *request-audit-uninstall*)
  (setf *request-audit-uninstall* nil))

(setf *request-audit-uninstall*
      (star.request-audit:install
       :writers
       (list (cons "default"
                   (star.audit:make-couchdb-writer
                    :database-url "http://127.0.0.1:5984/starintel_request_audit_default"
                    :tenant "default"
                    :username (request-audit-secret "/run/secrets/request-audit-writer-user")
                    :password (request-audit-secret "/run/secrets/request-audit-writer-password"))))
       :hmac-key (request-audit-hmac-key)
       :key-id "k1"
       :version "operator-v1"
       :policy #'operator-request-policy
       :refusals-enabled *request-audit-refusals-enabled*
       :on-assessment-error :deny
       ;; :ALLOW preserves availability on sink failure; :DENY only blocks in enforce mode.
       :on-audit-error :allow
       ;; Leave NIL for Lisp-only auditing. No remote/default model is selected.
       :classifier nil))

;;; To enable the local-model harness, replace :CLASSIFIER NIL above with:
;;; (star.request-audit:make-local-classifier
;;;  :endpoint "http://127.0.0.1:8080/v1/chat/completions"
;;;  :model (uiop:getenv "STAR_REQUEST_AUDIT_MODEL")
;;;  :max-concurrent 2)
;;; Set STAR_REQUEST_AUDIT_MODEL to your pinned model with known quantization.
;;; Turn off prompt logging at that local model service. Never put secrets here.
