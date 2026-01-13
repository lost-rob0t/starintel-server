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
      (dbg "root parsed type=~a keys(spec? ~a default-dataset? ~a event-log? ~a)"
           (type-of data)
           (jsown:keyp data "spec")
           (jsown:keyp data "default-dataset")
           (jsown:keyp data "event-log"))
      (is (not (null data)))
      (is (jsown:keyp data "spec"))
      (is (jsown:keyp data "default-dataset"))
      (is (jsown:keyp data "event-log")))))

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
;;; Utility Functions
;;; ----------------------------------------------------------------------

(defun run-http-api-tests ()
  "Run all HTTP API tests."
  (format t "~%Starting HTTP API tests...~%")
  (unwind-protect
       (handler-case
           (progn
             (start-test-server)
             (sleep 1)
             (run! 'http-api-tests))
         (error (e)
           (format t "~%Error running HTTP tests: ~a~%" e)
           (dbg "TOP-LEVEL ERROR: ~a" e)
           nil))
    ;; Cleanup always runs
    (stop-test-server)
    (format t "~%HTTP API tests completed~%")))
