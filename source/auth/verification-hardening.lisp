(in-package :star.auth)

(defparameter +dummy-verifier-salt+
  "00000000000000000000000000000000")

(defparameter +dummy-verifier+
  "0000000000000000000000000000000000000000000000000000000000000000")

(defun credential-verifier-material (record)
  "Return verifier bytes and salt without exposing record existence through work.
Malformed stored material fails closed through the same dummy verifier path."
  (handler-case
      (if record
          (values (decode-hex (api-key-record-verifier record))
                  (api-key-record-salt record))
          (values (decode-hex +dummy-verifier+)
                  +dummy-verifier-salt+))
    (error ()
      (values (decode-hex +dummy-verifier+)
              +dummy-verifier-salt+))))

(defun authenticate-api-key (api-key correlation-id deadline
                              &key (store *credential-store*))
  "Authenticate through one verifier-comparison path for known and unknown ids."
  (unless store
    (signal-authentication-failure))
  (multiple-value-bind (credential-id secret-octets)
      (parse-api-key api-key)
    (let* ((record (credential-store-get store credential-id))
           (now (auth-now)))
      (multiple-value-bind (expected salt)
          (credential-verifier-material record)
        (let* ((actual
                 (handler-case
                     (derive-verifier secret-octets salt star:*auth-pepper*)
                   (error ()
                     (derive-verifier
                      secret-octets
                      +dummy-verifier-salt+
                      (or star:*auth-pepper* "")))))
               (verified
                 (funcall *verifier-compare-function* expected actual)))
          (unless (and verified (active-record-p record now))
            (signal-authentication-failure))))
      (%make-request-security-context
       :principal (record-principal record)
       :correlation-id correlation-id
       :deadline deadline
       :authenticated-at now))))
