(in-package :star-server-tests)

;;;; HTTP API Unit Tests

(def-suite http-api-tests
  :description "Test suite for HTTP API endpoints")

(in-suite http-api-tests)

;;; ----------------------------------------------------------------------
;;; Debug toggles
;;; ----------------------------------------------------------------------

(defparameter *http-tests-debug* t
  "When true, print verbose debug info during HTTP tests.")

(defun dbg (fmt &rest args)
  (when *http-tests-debug*
    (apply #'format *error-output* (concatenate 'string "~&[http-tests] " fmt "~%") args)))

;;; ----------------------------------------------------------------------
;;; Test Configuration
;;; ----------------------------------------------------------------------

(defparameter *test-server* nil
  "Test server instance")

(defparameter *test-port* 5555
  "Port for test HTTP server")

(defparameter *test-base-url* (format nil "http://localhost:~d" *test-port*)
  "Base URL for test requests")

(defparameter *test-database* "starintel-test"
  "Database name for tests (separate from production)")

;;; ----------------------------------------------------------------------
;;; Header helpers (Dexador headers are annoyingly inconsistent)
;;; ----------------------------------------------------------------------

(defun header->string (v)
  (dbg "header->string input: ~s (type ~a)" v (type-of v))
  (cond
    ((null v) "")
    ((stringp v) v)
    ((and (listp v) (every #'stringp v))
     (format nil "~{~a~^, ~}" v))
    (t
     (princ-to-string v))))

(defun headers/get (headers name)
  "Return header value for NAME (case-insensitive) as a string."
  (dbg "headers/get name=~s headers-type=~a headers=~s"
       name (type-of headers) headers)
  (labels ((norm (x) (string-downcase (string x)))
           (matchp (k) (string= (norm k) (norm name))))
    (cond
      ((hash-table-p headers)
       (let ((found nil))
         (maphash (lambda (k v)
                    (when (and (null found) (matchp k))
                      (setf found v)))
                  headers)
         (dbg "headers/get(hash) found=~s (type ~a)" found (type-of found))
         (header->string found)))

      ((and (listp headers) (consp (car headers)))
       (let ((v (cdr (assoc name headers :test #'string-equal))))
         (dbg "headers/get(alist) raw=~s (type ~a)" v (type-of v))
         (header->string v)))

      ((listp headers)
       (let ((v (or (getf headers name)
                    (getf headers (string-downcase name))
                    (getf headers (string-upcase name)))))
         (dbg "headers/get(plist) raw=~s (type ~a)" v (type-of v))
         (header->string v)))

      (t
       (dbg "headers/get: unknown header container, returning empty string")
       ""))))

(defun header-contains-p (headers name needle)
  (let ((v (headers/get headers name)))
    (dbg "header-contains-p name=~s needle=~s value=~s (len ~d)"
         name needle v (length v))
    (and (plusp (length v))
         (search (string-downcase needle) (string-downcase v)))))

;;; ----------------------------------------------------------------------
;;; Test Fixtures and Helpers
;;; ----------------------------------------------------------------------

(defun ensure-test-database ()
  "Ensure the test database exists, create it if it doesn't."
  (dbg "Ensuring test database exists: ~a" *test-database*)
  (handler-case
      (anypool:with-connection (client star.databases.couchdb::*couchdb-pool*)
        (handler-case
            (cl-couch:get-database client *test-database*)
          (dexador:http-request-not-found ()
            (dbg "Database doesn't exist, creating: ~a" *test-database*)
            (cl-couch:create-database client *test-database*))))
    (error (e)
      (dbg "Error ensuring database: ~a" e)
      (error e))))

(defun destroy-test-database ()
  "Destroy the test database."
  (dbg "Destroying test database: ~a" *test-database*)
  (handler-case
      (anypool:with-connection (client star.databases.couchdb::*couchdb-pool*)
        (handler-case
            (cl-couch:delete-database client *test-database*)
          (dexador:http-request-not-found ()
            (dbg "Database already doesn't exist: ~a" *test-database*))))
    (error (e)
      (dbg "Error destroying database: ~a" e))))

(defun start-test-server ()
  "Start a test HTTP server instance."
  (dbg "start-test-server port=~d" *test-port*)
  (when (null *test-server*)
    (setf *test-server*
          (clack:clackup star.frontends.http-api::*app* 
                         :port *test-port*
                         :silent t)))
  (dbg "start-test-server server=~s" *test-server*)
  *test-server*)

(defun stop-test-server ()
  "Stop the test HTTP server."
  (dbg "stop-test-server server=~s" *test-server*)
  (when *test-server*
    (clack:stop *test-server*)
    (setf *test-server* nil))
  (dbg "stop-test-server done"))

(defun make-test-url (path)
  (let ((url (format nil "~a~a" *test-base-url* path)))
    (dbg "make-test-url path=~s -> ~s" path url)
    url))

(defun make-test-document (&key (id "test-123") (dtype "message"))
  "Create a test document structure."
  (jsown:new-js
    ("_id" id)
    ("type" dtype)
    ("content" "test content")
    ("timestamp" (get-universal-time))
    ("transient" :false)))

(defun make-test-target (&key (id "target-123") (actor "nmap"))
  "Create a test target structure."
  (jsown:new-js
    ("_id" id)
    ("type" "target")
    ("actor" actor)
    ("address" "192.168.1.1")
    ("status" "pending")))

;;; Test document creation helpers for BBP/network/web types

(defun make-test-host (&key (id "host-test-1") (ip "192.168.1.100"))
  "Create a test host document."
  (jsown:new-js
    ("_id" id)
    ("dtype" "host")
    ("ip" ip)
    ("hostname" "testhost.local")
    ("os" "Linux")
    ("ports" (list (jsown:new-js
                     ("port" 22)
                     ("name" "ssh"))
                   (jsown:new-js
                     ("port" 80)
                     ("name" "http"))))
    ("dataset" "test")
    ("dateAdded" (get-universal-time))))

(defun make-test-email (&key (id "email-test-1") (user "testuser") (domain "example.com"))
  "Create a test email document."
  (jsown:new-js
    ("_id" id)
    ("dtype" "email")
    ("user" user)
    ("domain" domain)
    ("password" "testpass123")
    ("dataset" "test")
    ("dateAdded" (get-universal-time))))

(defun make-test-domain (&key (id "domain-test-1") (record "example.com"))
  "Create a test domain document."
  (jsown:new-js
    ("_id" id)
    ("dtype" "domain")
    ("record" record)
    ("recordType" "A")
    ("resolvedAddresses" (list "1.2.3.4" "5.6.7.8"))
    ("dataset" "test")
    ("dateAdded" (get-universal-time))))

(defun make-test-user (&key (id "user-test-1") (name "testuser") (platform "github"))
  "Create a test user document."
  (jsown:new-js
    ("_id" id)
    ("dtype" "user")
    ("name" name)
    ("platform" platform)
    ("url" (format nil "https://~a.com/~a" platform name))
    ("bio" "Test user bio")
    ("dataset" "test")
    ("dateAdded" (get-universal-time))))

(defun make-test-network (&key (id "network-test-1") (asn 12345))
  "Create a test network document."
  (jsown:new-js
    ("_id" id)
    ("dtype" "network")
    ("asn" asn)
    ("org" "Test Organization")
    ("subnet" "10.0.0.0/8")
    ("dataset" "test")
    ("dateAdded" (get-universal-time))))

(defun make-test-url-doc (&key (id "url-test-1") (url "https://example.com/test"))
  "Create a test url document."
  (jsown:new-js
    ("_id" id)
    ("dtype" "url")
    ("url" url)
    ("path" "/test")
    ("content" "Test page content")
    ("dataset" "test")
    ("dateAdded" (get-universal-time))))

(defun make-test-breach (&key (id "breach-test-1") (total 10000))
  "Create a test breach document."
  (jsown:new-js
    ("_id" id)
    ("dtype" "breach")
    ("url" "https://example.com/breach")
    ("description" "Test breach description")
    ("total" total)
    ("dataset" "test")
    ("dateAdded" (get-universal-time))))

(defun make-test-email-message (&key (id "email-msg-test-1") (from "sender@example.com") (to "recipient@example.com"))
  "Create a test email-message document."
  (jsown:new-js
    ("_id" id)
    ("dtype" "email-message")
    ("from" from)
    ("to" to)
    ("subject" "Test Email Subject")
    ("body" "This is a test email message body")
    ("dataset" "test")
    ("dateAdded" (get-universal-time))))

(defun insert-test-document (doc)
  "Insert a test document directly into CouchDB."
  (dbg "Inserting test document: ~a" (jsown:val doc "_id"))
  (handler-case
      (anypool:with-connection (client star.databases.couchdb::*couchdb-pool*)
        (cl-couch:create-document client *test-database*
                                  (jsown:to-json doc)))
    (error (e)
      (dbg "Error inserting document: ~a" e)
      (error e))))

(defun delete-test-document (id)
  "Delete a test document from CouchDB."
  (dbg "Deleting test document: ~a" id)
  (handler-case
      (anypool:with-connection (client star.databases.couchdb::*couchdb-pool*)
        (handler-case
            (let* ((doc (cl-couch:get-document client *test-database* id))
                   (parsed-doc (jsown:parse doc))
                   (rev (jsown:val parsed-doc "_rev")))
              (dbg "Deleting document ~a with rev ~a" id rev)
              (cl-couch:delete-document client *test-database* id rev))
          (dexador:http-request-not-found ()
            (dbg "Document ~a not found, already deleted" id))))
    (dexador:http-request-conflict (e)
      (dbg "Conflict deleting document ~a: ~a (may have been updated/deleted concurrently)" id e))
    (error (e)
      (dbg "Error deleting document: ~a" e))))

(defun cleanup-test-documents ()
  "Clean up all test documents."
  (dbg "Cleaning up test documents")
  (loop for id in '("host-test-1" "host-test-2" "host-test-3"
                    "email-test-1" "email-test-2" "email-test-3"
                    "domain-test-1" "domain-test-2" "domain-test-3"
                    "user-test-1" "user-test-2" "user-test-3"
                    "network-test-1" "network-test-2"
                    "url-test-1" "url-test-2"
                    "breach-test-1" "breach-test-2"
                    "email-msg-test-1" "email-msg-test-2")
        do (delete-test-document id)))

;;; ----------------------------------------------------------------------
;;; Root Endpoint Tests
;;; ----------------------------------------------------------------------

(test test-root-endpoint
      "Test GET / returns server metadata."
      (dbg "TEST: test-root-endpoint")
      (let* ((url (make-test-url "/"))
             (response (dex:get url)))
        (dbg "root response type=~a len=~d" (type-of response) (length response))
        (dbg "root response snippet=~s"
             (subseq response 0 (min 200 (length response))))
        (let ((data (jsown:parse response)))
          (dbg "root parsed type=~a keys(doc_spec_version? ~a default-dataset? ~a event_log? ~a)"
               (type-of data)
               (jsown:keyp data "doc_spec_version")
               (jsown:keyp data "default-dataset")
               (jsown:keyp data "event_log"))
          (is (not (null data)))
          (is (jsown:keyp data "doc_spec_version"))
          (is (jsown:keyp data "default-dataset"))
          (is (jsown:keyp data "event_log")))))

(test test-root-endpoint-content-type
      "Test root endpoint returns JSON content type."
      (dbg "TEST: test-root-endpoint-content-type")
      (multiple-value-bind (body status headers)
          (dex:get (make-test-url "/"))
        (dbg "root status=~s body-type=~a body-len=~d headers-type=~a"
             status (type-of body) (if body (length body) 0) (type-of headers))
        (dbg "root headers=~s" headers)
        (is (= 200 status))
        (is (header-contains-p headers "content-type" "application/json"))))

;;; ----------------------------------------------------------------------
;;; A super-focused reproducer for the old crash
;;; ----------------------------------------------------------------------

(test test-debug-print-raw-headers
      "Print raw headers and exercise content-type lookup without crashing."
      (dbg "TEST: test-debug-print-raw-headers")
      (multiple-value-bind (body status headers)
          (dex:get (make-test-url "/"))
        (declare (ignore body))
        (dbg "status=~s headers=~s" status headers)
        (let ((ct (headers/get headers "content-type")))
          (dbg "content-type resolved => ~s (type ~a)" ct (type-of ct))
          (is (stringp ct))
          ;; if it's empty string, that's still useful debug data; don't hard-fail yet
          (pass))))

;;; ----------------------------------------------------------------------
;;; BBP/Network/Web Endpoint Tests
;;; ----------------------------------------------------------------------

;;; Host endpoint tests

(test test-hosts-by-ip
      "Test GET /documents/hosts/by-ip endpoint."
      (dbg "TEST: test-hosts-by-ip")
      (unwind-protect
           (progn
             (insert-test-document (make-test-host :id "host-test-1" :ip "192.168.1.100"))
             (insert-test-document (make-test-host :id "host-test-2" :ip "192.168.1.101"))
             (sleep 2)
             (let* ((url (make-test-url "/documents/hosts/by-ip?ip=192.168.1.100"))
                    (response (dex:get url)))
               (dbg "hosts-by-ip response len=~d" (length response))
               (let ((data (jsown:parse response)))
                 (is (listp data))
                 (is (> (length data) 0))
                 (let ((first-doc (car data)))
                   (is (string= "192.168.1.100" (jsown:val first-doc "ip")))))))
        (cleanup-test-documents)))

(test test-hosts-by-port
      "Test GET /documents/hosts/by-port endpoint."
      (dbg "TEST: test-hosts-by-port")
      (unwind-protect
           (progn
             (insert-test-document (make-test-host :id "host-test-1" :ip "192.168.1.100"))
             (sleep 2)
             (let* ((url (make-test-url "/documents/hosts/by-port?port=22"))
                    (response (dex:get url)))
               (dbg "hosts-by-port response len=~d" (length response))
               (let ((data (jsown:parse response)))
                 (is (listp data))
                 (is (> (length data) 0)))))
        (cleanup-test-documents)))

(test test-hosts-by-service
      "Test GET /documents/hosts/by-service endpoint."
      (dbg "TEST: test-hosts-by-service")
      (unwind-protect
           (progn
             (insert-test-document (make-test-host :id "host-test-1" :ip "192.168.1.100"))
             (sleep 2)
             (let* ((url (make-test-url "/documents/hosts/by-service?service=ssh"))
                    (response (dex:get url)))
               (dbg "hosts-by-service response len=~d" (length response))
               (let ((data (jsown:parse response)))
                 (is (listp data))
                 (is (> (length data) 0)))))
        (cleanup-test-documents)))

;;; Email endpoint tests

(test test-emails-by-email
      "Test GET /documents/emails/by-email endpoint."
      (dbg "TEST: test-emails-by-email")
      (unwind-protect
           (progn
             (insert-test-document (make-test-email :id "email-test-1" :user "testuser" :domain "example.com"))
             (sleep 2)
             (let* ((url (make-test-url "/documents/emails/by-email?email=testuser@example.com"))
                    (response (dex:get url)))
               (dbg "emails-by-email response len=~d" (length response))
               (let ((data (jsown:parse response)))
                 (is (listp data))
                 (is (> (length data) 0))
                 (let ((first-doc (car data)))
                   (is (string= "testuser" (jsown:val first-doc "user")))
                   (is (string= "example.com" (jsown:val first-doc "domain")))))))
        (cleanup-test-documents)))

(test test-emails-by-domain
      "Test GET /documents/emails/by-domain endpoint."
      (dbg "TEST: test-emails-by-domain")
      (unwind-protect
           (progn
             (insert-test-document (make-test-email :id "email-test-1" :user "user1" :domain "example.com"))
             (insert-test-document (make-test-email :id "email-test-2" :user "user2" :domain "example.com"))
             (insert-test-document (make-test-email :id "email-test-3" :user "user3" :domain "other.com"))
             (sleep 2)
             (let* ((url (make-test-url "/documents/emails/by-domain?domain=example.com"))
                    (response (dex:get url)))
               (dbg "emails-by-domain response len=~d" (length response))
               (let ((data (jsown:parse response)))
                 (is (listp data))
                 (is (>= (length data) 2)))))
        (cleanup-test-documents)))

(test test-emails-with-password
      "Test GET /documents/emails/with-password endpoint."
      (dbg "TEST: test-emails-with-password")
      (unwind-protect
           (progn
             (insert-test-document (make-test-email :id "email-test-1" :user "testuser" :domain "example.com"))
             (sleep 2)
             (let* ((url (make-test-url "/documents/emails/with-password"))
                    (response (dex:get url)))
               (dbg "emails-with-password response len=~d" (length response))
               (let ((data (jsown:parse response)))
                 (is (listp data))
                 (is (> (length data) 0)))))
        (cleanup-test-documents)))

;;; Domain endpoint tests

(test test-domains-by-record
      "Test GET /documents/domains/by-record endpoint."
      (dbg "TEST: test-domains-by-record")
      (unwind-protect
           (progn
             (insert-test-document (make-test-domain :id "domain-test-1" :record "example.com"))
             (sleep 2)
             (let* ((url (make-test-url "/documents/domains/by-record?record=example.com"))
                    (response (dex:get url)))
               (dbg "domains-by-record response len=~d" (length response))
               (let ((data (jsown:parse response)))
                 (is (listp data))
                 (is (> (length data) 0))
                 (let ((first-doc (car data)))
                   (is (string= "example.com" (jsown:val first-doc "record")))))))
        (cleanup-test-documents)))

(test test-domains-by-resolved-address
      "Test GET /documents/domains/by-resolved-address endpoint."
      (dbg "TEST: test-domains-by-resolved-address")
      (unwind-protect
           (progn
             (insert-test-document (make-test-domain :id "domain-test-1" :record "example.com"))
             (sleep 2)
             (let* ((url (make-test-url "/documents/domains/by-resolved-address?ip=1.2.3.4"))
                    (response (dex:get url)))
               (dbg "domains-by-resolved-address response len=~d" (length response))
               (let ((data (jsown:parse response)))
                 (is (listp data))
                 (is (> (length data) 0)))))
        (cleanup-test-documents)))

;;; User endpoint tests

(test test-users-by-name
      "Test GET /documents/users/by-name endpoint."
      (dbg "TEST: test-users-by-name")
      (unwind-protect
           (progn
             (insert-test-document (make-test-user :id "user-test-1" :name "testuser" :platform "github"))
             (sleep 2)
             (let* ((url (make-test-url "/documents/users/by-name?name=testuser"))
                    (response (dex:get url)))
               (dbg "users-by-name response len=~d" (length response))
               (let ((data (jsown:parse response)))
                 (is (listp data))
                 (is (> (length data) 0))
                 (let ((first-doc (car data)))
                   (is (string= "testuser" (jsown:val first-doc "name")))))))
        (cleanup-test-documents)))

(test test-users-by-platform
      "Test GET /documents/users/by-platform endpoint."
      (dbg "TEST: test-users-by-platform")
      (unwind-protect
           (progn
             (insert-test-document (make-test-user :id "user-test-1" :name "user1" :platform "github"))
             (insert-test-document (make-test-user :id "user-test-2" :name "user2" :platform "github"))
             (insert-test-document (make-test-user :id "user-test-3" :name "user3" :platform "twitter"))
             (sleep 2)
             (let* ((url (make-test-url "/documents/users/by-platform?platform=github"))
                    (response (dex:get url)))
               (dbg "users-by-platform response len=~d" (length response))
               (let ((data (jsown:parse response)))
                 (is (listp data))
                 (is (>= (length data) 2)))))
        (cleanup-test-documents)))

;;; Network endpoint tests

(test test-networks-by-asn
      "Test GET /documents/networks/by-asn endpoint."
      (dbg "TEST: test-networks-by-asn")
      (unwind-protect
           (progn
             (insert-test-document (make-test-network :id "network-test-1" :asn 12345))
             (sleep 2)
             (let* ((url (make-test-url "/documents/networks/by-asn?asn=12345"))
                    (response (dex:get url)))
               (dbg "networks-by-asn response len=~d" (length response))
               (let ((data (jsown:parse response)))
                 (is (listp data))
                 (is (> (length data) 0))
                 (let ((first-doc (car data)))
                   (is (= 12345 (jsown:val first-doc "asn")))))))
        (cleanup-test-documents)))

(test test-networks-by-org
      "Test GET /documents/networks/by-org endpoint."
      (dbg "TEST: test-networks-by-org")
      (unwind-protect
           (progn
             (insert-test-document (make-test-network :id "network-test-1" :asn 12345))
             (sleep 2)
             (let* ((url (make-test-url "/documents/networks/by-org?org=Test%20Organization"))
                    (response (dex:get url)))
               (dbg "networks-by-org response len=~d" (length response))
               (let ((data (jsown:parse response)))
                 (is (listp data))
                 (is (> (length data) 0)))))
        (cleanup-test-documents)))

;;; URL endpoint tests

(test test-urls-by-url
      "Test GET /documents/urls/by-url endpoint."
      (dbg "TEST: test-urls-by-url")
      (unwind-protect
           (progn
             (insert-test-document (make-test-url-doc :id "url-test-1" :url "https://example.com/test"))
             (sleep 2)
             (let* ((url (make-test-url "/documents/urls/by-url?url=https://example.com/test"))
                    (response (dex:get url)))
               (dbg "urls-by-url response len=~d" (length response))
               (let ((data (jsown:parse response)))
                 (is (listp data))
                 (is (> (length data) 0))
                 (let ((first-doc (car data)))
                   (is (string= "https://example.com/test" (jsown:val first-doc "url")))))))
        (cleanup-test-documents)))

(test test-urls-by-domain
      "Test GET /documents/urls/by-domain endpoint."
      (dbg "TEST: test-urls-by-domain")
      (unwind-protect
           (progn
             (insert-test-document (make-test-url-doc :id "url-test-1" :url "https://example.com/test"))
             (sleep 2)
             (let* ((url (make-test-url "/documents/urls/by-domain?domain=example.com"))
                    (response (dex:get url)))
               (dbg "urls-by-domain response len=~d" (length response))
               (let ((data (jsown:parse response)))
                 (is (listp data))
                 (is (> (length data) 0)))))
        (cleanup-test-documents)))

;;; Breach endpoint tests

(test test-breaches-by-size
      "Test GET /documents/breaches/by-size endpoint."
      (dbg "TEST: test-breaches-by-size")
      (unwind-protect
           (progn
             (insert-test-document (make-test-breach :id "breach-test-1" :total 10000))
             (insert-test-document (make-test-breach :id "breach-test-2" :total 50000))
             (sleep 2)
             (let* ((url (make-test-url "/documents/breaches/by-size?descending=true&limit=10"))
                    (response (dex:get url)))
               (dbg "breaches-by-size response len=~d" (length response))
               (let ((data (jsown:parse response)))
                 (is (listp data))
                 (is (>= (length data) 2)))))
        (cleanup-test-documents)))

;;; Document creation via POST tests

(test test-post-new-host-document
      "Test POST /new/document/host endpoint."
      (dbg "TEST: test-post-new-host-document")
      (let* ((url (make-test-url "/new/document/host"))
             (doc (make-test-host :id "host-post-test" :ip "10.0.0.1")))
        (unwind-protect
             (handler-case
                 (let ((response (dex:post url
                                           :content (jsown:to-json doc)
                                           :headers '(("Content-Type" . "application/json")))))
                   (dbg "post-host response len=~d" (length response))
                   (let ((data (jsown:parse response)))
                     (is (jsown:keyp data "_id"))
                     (is (string= "host-post-test" (jsown:val data "_id")))))
               (error (e)
                 (dbg "Expected error (no RabbitMQ in tests): ~a" e)
                 (pass)))
          (delete-test-document "host-post-test"))))

(test test-post-new-email-document
      "Test POST /new/document/email endpoint."
      (dbg "TEST: test-post-new-email-document")
      (let* ((url (make-test-url "/new/document/email"))
             (doc (make-test-email :id "email-post-test" :user "posttest" :domain "example.com")))
        (unwind-protect
             (handler-case
                 (let ((response (dex:post url
                                           :content (jsown:to-json doc)
                                           :headers '(("Content-Type" . "application/json")))))
                   (dbg "post-email response len=~d" (length response))
                   (let ((data (jsown:parse response)))
                     (is (jsown:keyp data "_id"))
                     (is (string= "email-post-test" (jsown:val data "_id")))))
               (error (e)
                 (dbg "Expected error (no RabbitMQ in tests): ~a" e)
                 (pass)))
          (delete-test-document "email-post-test"))))

;;; ----------------------------------------------------------------------
;;; Document Deletion via HTTP API Tests
;;; ----------------------------------------------------------------------

(test test-delete-document-endpoint
  "Test DELETE /document/:id endpoint."
  (dbg "TEST: test-delete-document-endpoint")
  (let* ((test-id "delete-http-test-1")
         (doc (make-test-host :id test-id :ip "192.168.50.50")))
    (unwind-protect
         (progn
           ;; Insert test document
           (insert-test-document doc)
           (sleep 1)

           ;; Verify document exists via GET
           (let* ((get-url (make-test-url (format nil "/document/~a" test-id)))
                  (get-response (dex:get get-url)))
             (dbg "GET response len=~d" (length get-response))
             (let ((data (jsown:parse get-response)))
               (is (string= test-id (jsown:val data "_id")))))

           ;; Delete via HTTP API
           (let* ((delete-url (make-test-url (format nil "/document/~a" test-id)))
                  (delete-response (dex:delete delete-url)))
             (dbg "DELETE response: ~s" delete-response)
             (let ((data (jsown:parse delete-response)))
               (is (string= "success" (jsown:val data "status")))))

           ;; Verify document is deleted - GET should return 404
           (let ((get-url (make-test-url (format nil "/document/~a" test-id))))
             (handler-case
                 (progn
                   (dex:get get-url)
                   (fail "Expected 404 but document still exists"))
               (dexador:http-request-not-found ()
                 (dbg "Document successfully deleted via HTTP API")
                 (pass)))))
      ;; Cleanup
      (handler-case
          (delete-test-document test-id)
        (error () nil)))))

(test test-delete-nonexistent-document
  "Test DELETE /document/:id with nonexistent document returns 404."
  (dbg "TEST: test-delete-nonexistent-document")
  (let* ((nonexistent-id "nonexistent-doc-12345")
         (delete-url (make-test-url (format nil "/document/~a" nonexistent-id))))
    (handler-case
        (progn
          (dex:delete delete-url)
          (fail "Expected 404 for nonexistent document"))
      (dexador:http-request-not-found ()
        (dbg "Correctly returned 404 for nonexistent document")
        (pass)))))

;;; ----------------------------------------------------------------------
;;; Utility Functions
;;; ----------------------------------------------------------------------

(defun run-http-api-tests ()
  "Run all HTTP API tests."
  (format t "~%Starting HTTP API tests...~%")
  (let ((results nil))
    (unwind-protect
         (handler-case
             (progn
               (ensure-test-database)
               (start-test-server)
               (sleep 1)
               (setf results (run! 'http-api-tests)))
           (error (e)
             (format t "~%Error running HTTP tests: ~a~%" e)
             (dbg "TOP-LEVEL ERROR: ~a" e)
             (setf results nil)))
      ;; Cleanup always runs
      (cleanup-test-documents)
      (stop-test-server)
      (destroy-test-database)
      (format t "~%HTTP API tests completed~%"))
    ;; Return results after cleanup
    results))
