(in-package :star.actors)

;; The proxy protocol stays inside the actor system. Credential-bearing replies
;; must never travel through the target/document/Rabbit publication paths.

(defstruct (proxy-request
            (:constructor make-proxy-request
                (&key mode project-id seed (product "rp") (protocol "http")
                      country (lifetime-minutes 30))))
  "Typed internal selection or capability request for the proxy gateway."
  mode project-id seed product protocol country lifetime-minutes)

(defstruct (proxy-config
            (:constructor make-proxy-config
                (&key host port username password protocol product session-id
                      lifetime-minutes stability)))
  "Credential-bearing connection fields; handle only inside trusted callers."
  host port username password protocol product session-id lifetime-minutes stability)

(defstruct (proxy-result
            (:constructor make-proxy-result
                (&key status config capabilities reason)))
  "Gateway reply with a status and optional configuration or capabilities."
  status config capabilities reason)

(defmethod print-object ((object proxy-request) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "mode=~a project=REDACTED seed=REDACTED"
            (proxy-request-mode object))))

(defmethod print-object ((object proxy-config) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "protocol=~a credentials=REDACTED"
            (proxy-config-protocol object))))

(defmethod print-object ((object proxy-result) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "status=~a" (proxy-result-status object))))

(defun proxy-capabilities ()
  '(:rotating t :sticky :best-effort :vpn nil :protocols ("http" "socks5")))

(defun valid-proxy-id-p (value)
  (and (stringp value)
       (<= 1 (length value) 128)
       (cl-ppcre:scan "^[A-Za-z0-9][A-Za-z0-9._:-]*$" value)))

(defun valid-proxy-request-p (request)
  (and (typep request 'proxy-request)
       (member (proxy-request-mode request) '(:rotating :sticky))
       (valid-proxy-id-p (proxy-request-project-id request))
       (member (proxy-request-product request) '("rp" "rpc" "sdc" "mp")
               :test #'string=)
       (member (proxy-request-protocol request) '("http" "socks5")
               :test #'string=)
       (let ((country (proxy-request-country request)))
         (or (null country)
             (and (stringp country)
                  (cl-ppcre:scan "^[A-Z]{2}$" country))))
       (or (eq :rotating (proxy-request-mode request))
           (and (valid-proxy-id-p (proxy-request-seed request))
                (integerp (proxy-request-lifetime-minutes request))
                (<= 1 (proxy-request-lifetime-minutes request) 1440)))))

(defun proxy-session-id (request secret)
  "Derive one opaque Evomi session token scoped to project and selection."
  (unless (and (stringp secret) (plusp (length secret)))
    (error "Proxy session secret is unavailable."))
  (let* ((parts (list (proxy-request-project-id request)
                      (proxy-request-product request)
                      (or (proxy-request-country request) "")
                      (proxy-request-seed request)))
         (message (format nil "~{~d:~a~^|~}"
                          (loop for part in parts append (list (length part) part))))
         (hmac (ironclad:make-hmac
                (babel:string-to-octets secret :encoding :utf-8) :sha256)))
    (ironclad:update-hmac hmac (babel:string-to-octets message :encoding :utf-8))
    (subseq (ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac)) 0 10)))

(defun evomi-generate-url (request)
  "Build the Evomi generation URL without putting the API key in the query."
  (let ((parameters
          `(("product" . ,(proxy-request-product request))
            ("protocol" . ,(proxy-request-protocol request))
            ("amount" . "1")
            ("format" . "2")
            ("prepend_protocol" . "false"))))
    (when (proxy-request-country request)
      (setf parameters
            (append parameters
                    (list (cons "countries" (proxy-request-country request))))))
    (concatenate 'string "https://api.evomi.com/public/generate?"
                 (quri:url-encode-params parameters))))

(defun evomi-http-transport (url api-key)
  (dex:get url
           :headers (list (cons "x-apikey" api-key))
           :connect-timeout 2
           :read-timeout 5
           :use-connection-pool nil))

(defun parse-evomi-proxy (body request session-id)
  "Parse only Evomi's requested host-first format; reject unexpected hosts."
  (unless (and (stringp body) (<= (length body) 2048))
    (error "Invalid Evomi proxy response."))
  (let* ((line (string-trim '(#\Space #\Tab #\Newline #\Return) body))
         (parts (uiop:split-string line :separator ":")))
    (unless (= 4 (length parts))
      (error "Invalid Evomi proxy response."))
    (destructuring-bind (host port-text username password) parts
      (let ((port (ignore-errors (parse-integer port-text :junk-allowed nil))))
        (unless (and (cl-ppcre:scan
                      "^[A-Za-z0-9-]+(\\.[A-Za-z0-9-]+)*\\.(evomi\\.com|evomi-proxy\\.com)$"
                      host)
                     port (<= 1 port 65535)
                     (cl-ppcre:scan "^[A-Za-z0-9._-]+$" username)
                     (plusp (length password))
                     (notany (lambda (character)
                               (or (char= character #\:)
                                   (<= (char-code character) 32)
                                   (= (char-code character) 127)))
                             password))
          (error "Invalid Evomi proxy response."))
        (make-proxy-config
         :host host :port port :username username
         :password (if session-id
                       (format nil "~a_session-~a_lifetime-~d"
                               password session-id
                               (proxy-request-lifetime-minutes request))
                       password)
         :protocol (proxy-request-protocol request)
         :product (proxy-request-product request)
         :session-id session-id
         :lifetime-minutes (and session-id
                                (proxy-request-lifetime-minutes request))
         :stability (if session-id :best-effort :rotating))))))

(defun complete-proxy-request (result)
  "Return RESULT to ASK-S and reply to an asynchronous sender when present."
  (when *sender*
    (reply result *sender*))
  result)

(defun make-evomi-provider-handler (&key api-key session-secret
                                         (transport #'evomi-http-transport))
  "Return a provider actor handler with an injectable HTTP effect."
  (lambda (request)
    (complete-proxy-request
     (cond
       ((not (valid-proxy-request-p request))
        (make-proxy-result :status :invalid-request :reason :invalid-input))
       ((or (not (stringp api-key)) (zerop (length api-key))
            (and (eq :sticky (proxy-request-mode request))
                 (or (not (stringp session-secret))
                     (zerop (length session-secret)))))
        (make-proxy-result :status :unavailable :reason :not-configured))
       (t
        (handler-case
            (multiple-value-bind (body status)
                (funcall transport (evomi-generate-url request) api-key)
              (unless (= 200 status)
                (error "Evomi generation failed."))
              (make-proxy-result
               :status :ok
               :config (parse-evomi-proxy
                        body request
                        (when (eq :sticky (proxy-request-mode request))
                          (proxy-session-id request session-secret)))))
          (condition ()
            ;; Never reflect provider response bodies or credential-bearing URLs.
            (make-proxy-result :status :provider-error
                               :reason :generation-failed))))))))

(defun make-proxy-gateway-handler (provider &key (ask-fn #'sento.actor:ask-s))
  "Validate and route a typed request to the Evomi provider actor."
  (lambda (request)
    (complete-proxy-request
     (cond
       ((and (typep request 'proxy-request)
             (eq :capabilities (proxy-request-mode request)))
        (make-proxy-result :status :ok :capabilities (proxy-capabilities)))
       ((and (typep request 'proxy-request)
             (eq :vpn (proxy-request-mode request)))
        (make-proxy-result :status :unsupported :reason :vpn-unavailable))
       ((not (valid-proxy-request-p request))
        (make-proxy-result :status :invalid-request :reason :invalid-input))
       (t
        (handler-case
            (funcall ask-fn provider request :time-out 8)
          (condition ()
            (make-proxy-result :status :provider-error
                               :reason :provider-unavailable))))))))

(defvar *evomi-provider* nil "Process-owned Evomi provider actor reference.")
(defvar *proxy-gateway* nil "Process-owned proxy gateway actor reference.")

(defun start-proxy-actors (&optional (system *sys*)
                            &key
                              (api-key (uiop:getenv "EVOMI_API_KEY"))
                              (session-secret (uiop:getenv "EVOMI_SESSION_SECRET"))
                              (transport #'evomi-http-transport))
  "Register the provider and gateway with the process-owned Sento system."
  (setf *evomi-provider*
        (actor-of system :name "evomi-proxy-provider" :dispatcher :proxy
                         :receive (make-evomi-provider-handler
                                   :api-key api-key
                                   :session-secret session-secret
                                   :transport transport))
        *proxy-gateway*
        (actor-of system :name "proxy-gateway" :dispatcher :pinned
                         :receive (make-proxy-gateway-handler *evomi-provider*)))
  *proxy-gateway*)

(defun request-proxy (request &key (time-out 9))
  "Ask the core gateway for a proxy configuration or capability result."
  (unless *proxy-gateway*
    (error "Proxy gateway is not running."))
  (sento.actor:ask-s *proxy-gateway* request :time-out time-out))

(nhooks:add-hook star:*actors-start-hook* #'start-proxy-actors)

(setf (documentation 'make-proxy-request 'function)
      "Construct a typed proxy request for rotating, sticky, or capability mode.")
(setf (documentation 'make-proxy-result 'function)
      "Construct a typed gateway result; never stringify its credential fields.")
(dolist (entry
         '((proxy-request-mode "Requested operation keyword.")
           (proxy-request-project-id "Caller-supplied project scope identifier.")
           (proxy-request-seed "Opaque target seed for sticky sessions.")
           (proxy-request-product "Evomi proxy product code.")
           (proxy-request-protocol "Requested HTTP or SOCKS5 protocol.")
           (proxy-request-country "Optional uppercase ISO country code.")
           (proxy-request-lifetime-minutes "Bounded sticky lifetime in minutes.")
           (proxy-config-host "Provider proxy endpoint hostname.")
           (proxy-config-port "Provider proxy endpoint port.")
           (proxy-config-username "Secret provider proxy username.")
           (proxy-config-password "Secret provider proxy password with session suffix.")
           (proxy-config-protocol "Connection protocol for the endpoint.")
           (proxy-config-product "Evomi product used for the configuration.")
           (proxy-config-session-id "Opaque sticky session token, when present.")
           (proxy-config-lifetime-minutes "Configured sticky lifetime, when present.")
           (proxy-config-stability "Rotating or best-effort sticky behavior.")
           (proxy-result-status "Typed outcome keyword.")
           (proxy-result-config "Credential-bearing proxy configuration on success.")
           (proxy-result-capabilities "Supported gateway modes and protocols.")
           (proxy-result-reason "Safe reason keyword without provider secrets.")))
  (setf (documentation (first entry) 'function) (second entry)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(proxy-request make-proxy-request
            proxy-request-mode proxy-request-project-id proxy-request-seed
            proxy-request-product proxy-request-protocol proxy-request-country
            proxy-request-lifetime-minutes
            proxy-config proxy-config-host proxy-config-port
            proxy-config-username proxy-config-password proxy-config-protocol
            proxy-config-product proxy-config-session-id
            proxy-config-lifetime-minutes proxy-config-stability
            proxy-result make-proxy-result proxy-result-status proxy-result-config
            proxy-result-capabilities proxy-result-reason
            make-proxy-gateway-handler make-evomi-provider-handler
            evomi-generate-url parse-evomi-proxy proxy-session-id
            start-proxy-actors request-proxy
            *proxy-gateway* *evomi-provider*)
          :star.actors))
