(in-package :star.frontends.http-api)

(defun oauth-param (params name &optional required-p)
  (let ((value (query-value params name)))
    (when (and required-p
               (not (non-empty-string-p value)))
      (star.auth::signal-oauth-error
       "invalid_request"
       (format nil "OAuth parameter ~a is required" name)))
    value))

(defun oauth-scope-list (value)
  (unless (non-empty-string-p value)
    (star.auth::signal-oauth-error
     "invalid_scope"
     "At least one OAuth scope is required"))
  (let ((scopes
          (remove-if
           (lambda (scope) (zerop (length scope)))
           (cl-ppcre:split "[[:space:]]+" value))))
    (unless scopes
      (star.auth::signal-oauth-error
       "invalid_scope"
       "At least one OAuth scope is required"))
    (star.auth::normalize-oauth-scopes scopes)))

(defun oauth-valid-s256-challenge-p (challenge method)
  (and (stringp method)
       (string= method "S256")
       (stringp challenge)
       (= 43 (length challenge))
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "-_")))
              challenge)))

(defun oauth-provider-authorization-request
    (params &key (store star.auth:*credential-store*))
  "Validate an OAuth authorization request before any user authentication.

The returned plist contains only normalized protocol state and can be passed to
OAUTH-PROVIDER-AUTHORIZE after the user authenticates."
  (let* ((response-type (oauth-param params "response_type" t))
         (client-id (oauth-param params "client_id" t))
         (redirect-uri (oauth-param params "redirect_uri" t))
         (scope-value (oauth-param params "scope" t))
         (state (oauth-param params "state" nil))
         (challenge (oauth-param params "code_challenge" t))
         (method (oauth-param params "code_challenge_method" t))
         (client (star.auth::active-oauth-client client-id store))
         (scopes (oauth-scope-list scope-value)))
    (unless (string= response-type "code")
      (star.auth::signal-oauth-error
       "unsupported_response_type"
       "Only OAuth authorization-code response type is supported"))
    (unless (member redirect-uri
                    (star.auth:oauth-client-record-redirect-uris client)
                    :test #'string=)
      (star.auth::signal-oauth-error
       "invalid_redirect_uri"
       "OAuth redirect URI is invalid"))
    (unless (star.auth::scopes-subset-p
             scopes
             (star.auth:oauth-client-record-allowed-scopes client))
      (star.auth::signal-oauth-error
       "invalid_scope"
       "Requested OAuth scope is not permitted"))
    (unless (oauth-valid-s256-challenge-p challenge method)
      (star.auth::signal-oauth-error
       "invalid_request"
       "PKCE S256 is required"))
    (list :client-id client-id
          :redirect-uri redirect-uri
          :scopes scopes
          :state state
          :code-challenge challenge
          :code-challenge-method "S256")))

(defun oauth-percent-encode (value)
  (let ((octets (babel:string-to-octets (or value "") :encoding :utf-8))
        (digits "0123456789ABCDEF"))
    (with-output-to-string (stream)
      (loop for byte across octets
            for character = (and (< byte 128) (code-char byte))
            do (if (and character
                        (or (alphanumericp character)
                            (find character "-._~")))
                   (write-char character stream)
                   (progn
                     (write-char #\% stream)
                     (write-char (char digits (ldb (byte 4 4) byte)) stream)
                     (write-char (char digits (ldb (byte 4 0) byte)) stream)))))))

(defun oauth-redirect-location (redirect-uri raw-code state)
  (format nil "~a~:[?~;&~]code=~a~@[&state=~a~]"
          redirect-uri
          (not (null (position #\? redirect-uri)))
          (oauth-percent-encode raw-code)
          (and state (oauth-percent-encode state))))

(defun oauth-provider-authorize
    (request username password &key (store star.auth:*credential-store*))
  "Authenticate an existing StarIntel user and issue a one-time OAuth code.

This path intentionally calls AUTHENTICATE-USER-PASSWORD directly and never
creates an API key or a second user identity."
  (let ((user
          (handler-case
              (star.auth:authenticate-user-password
               username password :store store)
            (star.auth:authentication-error ()
              (star.auth::signal-oauth-error
               "access_denied"
               "OAuth authorization was denied")))))
    (multiple-value-bind (record raw-code)
        (star.auth:issue-oauth-authorization-code
         (getf request :client-id)
         (getf request :redirect-uri)
         (star.auth:user-record-username user)
         (getf request :scopes)
         (getf request :code-challenge)
         (getf request :code-challenge-method)
         :store store)
      (declare (ignore record))
      (oauth-redirect-location
       (getf request :redirect-uri)
       raw-code
       (getf request :state)))))

(defun oauth-provider-token-exchange
    (params &key (store star.auth:*credential-store*))
  "Exchange one authorization code for one short-lived opaque bearer token."
  (let ((grant-type (oauth-param params "grant_type" t)))
    (unless (string= grant-type "authorization_code")
      (star.auth::signal-oauth-error
       "unsupported_grant_type"
       "Only authorization_code grant type is supported"))
    (let ((raw-code (oauth-param params "code" t))
          (client-id (oauth-param params "client_id" t))
          (client-secret (oauth-param params "client_secret" t))
          (redirect-uri (oauth-param params "redirect_uri" t))
          (code-verifier (oauth-param params "code_verifier" t)))
      (multiple-value-bind (record raw-token)
          (star.auth:exchange-oauth-authorization-code
           raw-code
           client-id
           client-secret
           redirect-uri
           code-verifier
           :store store)
        (values
         (jsown:to-json
          (jsown:new-js
            ("access_token" raw-token)
            ("token_type" "Bearer")
            ("expires_in" star:*oauth-access-token-seconds*)
            ("scope"
             (format nil "~{~a~^ ~}"
                     (star.auth:oauth-access-token-record-scopes record)))))
         (list :cache-control "no-store"
               :pragma "no-cache"))))))

(defun oauth-error-status (code)
  (cond
    ((string= code "invalid_client") 401)
    ((string= code "access_denied") 403)
    (t 400)))

(defun oauth-error-body (condition)
  (jsown:to-json
   (jsown:new-js
     ("error" (star.auth:oauth-error-code condition))
     ("error_description" (star.auth:oauth-error-message condition)))))

(defun set-oauth-response-headers (&rest headers)
  (setf (lack.response:response-headers *response*)
        (append (lack.response:response-headers *response*) headers)))

(defun oauth-html-escape (value)
  (with-output-to-string (stream)
    (loop for character across (or value "")
          do (case character
               (#\& (write-string "&amp;" stream))
               (#\< (write-string "&lt;" stream))
               (#\> (write-string "&gt;" stream))
               (#\" (write-string "&quot;" stream))
               (#\' (write-string "&#39;" stream))
               (otherwise (write-char character stream))))))

(defun oauth-hidden-input (name value)
  (format nil "<input type=\"hidden\" name=\"~a\" value=\"~a\">"
          (oauth-html-escape name)
          (oauth-html-escape value)))

(defun oauth-authorization-form (request)
  (let ((scope-text (format nil "~{~a~^ ~}" (getf request :scopes))))
    (format nil
            "<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"><title>Authorize StarIntel</title></head><body><main><h1>Authorize StarIntel</h1><p>Requested scopes: <code>~a</code></p><form method=\"post\" action=\"/oauth/authorize\">~a~a~a~a~a~a<label>Username <input name=\"username\" autocomplete=\"username\" required></label><label>Password <input type=\"password\" name=\"password\" autocomplete=\"current-password\" required></label><button type=\"submit\">Authorize</button></form></main></body></html>"
            (oauth-html-escape scope-text)
            (oauth-hidden-input "response_type" "code")
            (oauth-hidden-input "client_id" (getf request :client-id))
            (oauth-hidden-input "redirect_uri" (getf request :redirect-uri))
            (oauth-hidden-input "scope" scope-text)
            (oauth-hidden-input "state" (or (getf request :state) ""))
            (concatenate
             'string
             (oauth-hidden-input "code_challenge" (getf request :code-challenge))
             (oauth-hidden-input "code_challenge_method" "S256")))))

(defun handle-oauth-authorize-get (params)
  (handler-case
      (let ((request (oauth-provider-authorization-request params)))
        (set-oauth-response-headers
         :content-type "text/html; charset=utf-8"
         :cache-control "no-store"
         :pragma "no-cache"
         :content-security-policy "default-src 'none'; form-action 'self'; base-uri 'none'; frame-ancestors 'none'")
        (oauth-authorization-form request))
    (star.auth:oauth-error (condition)
      (setf (lack.response:response-status *response*)
            (oauth-error-status (star.auth:oauth-error-code condition)))
      (set-oauth-response-headers
       :content-type "application/json"
       :cache-control "no-store"
       :pragma "no-cache")
      (oauth-error-body condition))))

(defun handle-oauth-authorize-post (params)
  (handler-case
      (let* ((request (oauth-provider-authorization-request params))
             (username (oauth-param params "username" t))
             (password (oauth-param params "password" t))
             (location
               (oauth-provider-authorize request username password)))
        (setf (lack.response:response-status *response*) 302)
        (set-oauth-response-headers
         :location location
         :cache-control "no-store"
         :pragma "no-cache")
        "")
    (star.auth:oauth-error (condition)
      (setf (lack.response:response-status *response*)
            (oauth-error-status (star.auth:oauth-error-code condition)))
      (set-oauth-response-headers
       :content-type "application/json"
       :cache-control "no-store"
       :pragma "no-cache")
      (oauth-error-body condition))))

(defun handle-oauth-token-post (params)
  (handler-case
      (multiple-value-bind (body headers)
          (oauth-provider-token-exchange params)
        (set-oauth-response-headers
         :content-type "application/json"
         :cache-control (getf headers :cache-control)
         :pragma (getf headers :pragma))
        body)
    (star.auth:oauth-error (condition)
      (setf (lack.response:response-status *response*)
            (oauth-error-status (star.auth:oauth-error-code condition)))
      (set-oauth-response-headers
       :content-type "application/json"
       :cache-control "no-store"
       :pragma "no-cache")
      (oauth-error-body condition))))

(setf (ningle:route *app* "/oauth/authorize" :method :get)
      #'handle-oauth-authorize-get)

(setf (ningle:route *app* "/oauth/authorize" :method :post)
      #'handle-oauth-authorize-post)

(setf (ningle:route *app* "/oauth/token" :method :post)
      #'handle-oauth-token-post)
