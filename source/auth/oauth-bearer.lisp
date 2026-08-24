(in-package :star.auth)

(defun oauth-access-token-string-p (token)
  (and (stringp token)
       (>= (length token) (length +oauth-access-token-prefix+))
       (string= +oauth-access-token-prefix+
                token
                :end2 (length +oauth-access-token-prefix+))))

(defun authenticate-bearer-authorization-header
    (authorization-header correlation-id deadline
     &key (store *credential-store*))
  "Authenticate either a StarIntel OAuth access token or legacy API key.
Both credential families produce the same request security-context boundary."
  (let ((token (bearer-token authorization-header)))
    (if (oauth-access-token-string-p token)
        (authenticate-oauth-access-token
         token correlation-id deadline :store store)
        (authenticate-api-key
         token correlation-id deadline :store store))))

(export '(authenticate-bearer-authorization-header)
        :star.auth)
