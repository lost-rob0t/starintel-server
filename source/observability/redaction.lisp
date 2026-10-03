(in-package :star.observability)

;; Defense in depth layer 1: application emission policy. Structural field
;; removal, not payload scanning. The collector (layer 2) and the OpenObserve
;; ingestion path (layer 3) enforce the same namespace independently.

(defparameter *forbidden-attribute-keys*
  '("authorization" "cookie" "set-cookie" "password" "secret" "token"
    "api_key" "apikey" "api-key" "access_token" "refresh_token"
    "client_secret" "request.body" "response.body" "document.body"
    "message.body" "authorization_header")
  "Attribute keys that must never be recorded. Matched case-insensitively on
the exact key or its last dot-segment.")

(defparameter *forbidden-key-regex*
  (cl-ppcre:create-scanner
   "(?i)(^|[^a-z0-9_-])(authorization|cookie|set-cookie|password|passwd|secret|token|api[_-]?key|access[_-]?token|refresh[_-]?token|client[_-]?secret)$")
  "Suffix matcher for key names that embed credential concepts, e.g.
http.request.header.authorization.")

(defun forbidden-key-p (key)
  "True when KEY (a string or symbol) names a forbidden attribute."
  (when key
    (let ((name (string-downcase (if (symbolp key)
                                     (symbol-name key)
                                     (princ-to-string key)))))
      (or (member name *forbidden-attribute-keys* :test #'string=)
          (cl-ppcre:scan *forbidden-key-regex* name)))))

(defun redact-attributes (attributes)
  "Remove forbidden keys from an alist of (KEY . VALUE) attributes.
Cardinality guard: never allow nil/bignums; stringify values."
  (loop for (key . value) in attributes
        unless (forbidden-key-p key)
          collect (cons key (if (or (stringp value) (numberp value)
                                    (keywordp value))
                                value
                                (princ-to-string value)))))
