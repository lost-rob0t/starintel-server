;;; starintel-workbench-test.el --- Hermetic ERT tests for the OSINT workbench -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: nsaspy
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, processes

;; Commentary:

;; Hermetic ERT tests for the StarIntel workbench foundation:
;; server profiles, star:// URIs, the generic object buffer, and the
;; search results buffer.  All tests run against fake HTTP transports;
;; no live StarIntel server is required.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path dir))

(require 'client)
(require 'starintel-server)
(require 'starintel-uri)
(require 'starintel-object)
(require 'starintel-search)
(require 'starintel)

;;; ------------------------------------------------------------------
;;; Fake transport harness (same contract as client-test.el)
;;; ------------------------------------------------------------------

(defvar starintel-wb-test--requests nil)
(defvar starintel-wb-test--responses nil)
(defvar starintel-wb-test--deferred nil)

(defun starintel-wb-test-fake-transport (_method url headers _body timeout-ms callback)
  "Synchronous fake transport recording every call."
  (push (list :method 'req :url url :headers headers :timeout-ms timeout-ms)
        starintel-wb-test--requests)
  (funcall callback
           (if (null starintel-wb-test--responses)
               (error "fake transport: unexpected request to %s" url)
             (pop starintel-wb-test--responses))))

(defun starintel-wb-test-deferred-transport (_method url _headers _body _timeout-ms callback)
  "Fake async transport parking CALLBACK until released."
  (setq starintel-wb-test--deferred
        (append starintel-wb-test--deferred
                (list (list :url url :callback callback)))))

(defun starintel-wb-test--release (response)
  "Complete the oldest deferred request with RESPONSE."
  (let* ((entry (car starintel-wb-test--deferred)))
    (setq starintel-wb-test--deferred (cdr starintel-wb-test--deferred))
    (funcall (plist-get entry :callback) response)
    entry))

(defun starintel-wb-test-last-request ()
  (car starintel-wb-test--requests))

(defun starintel-wb-test--ok (body)
  "Build an HTTP 200 response plist with JSON BODY."
  (list :status 200 :headers '() :body (json-encode body)))

(defconst starintel-wb-test--capabilities
  `((status . "ok")
    (data . ((build . ((service . "starintel-gserver") (version . "0.9.4")))
             (schema_revisions . ((api . "v1") (document . "0.9.0")))
             (authentication . ((modes . ["api-key"])))
             (features . ((documents . t) (search . t) (stats . t)
                          (target_leases . :json-false) (streams . :json-false)))
             (endpoints . ,(vconcat
                            (list
                             `((id . "public_search") (method . "GET")
                               (path . "/api/v1/search") (legacy . :json-false)
                               (authority . "public"))
                             `((id . "document_read") (method . "GET")
                               (path . "/document/:id") (legacy . t)
                               (authority . "authenticated"))
                             `((id . "stats") (method . "GET")
                               (path . "/api/v1/stats") (legacy . :json-false)
                               (authority . "public")))))
             (compatibility . ((legacy_routes . t)))))))

(defconst starintel-wb-test--person-doc
  '((dtype . "person")
    (schema_version . "0.9.0")
    (_id . "01JPERSON0000000000000000")
    (_rev . "1-abc")
    (dataset . "investigation-a")
    (sources . ["manual"])
    (version . 1)
    (dateAdded . 1735689600)
    (data . ((fname . "Ada") (lname . "Lovelace") (bio . "first programmer")))
    (extensions . ((star_server . ((trace_id . "01JTRACE")))))))

(defconst starintel-wb-test--relation-doc
  '((dtype . "relation")
    (_id . "01JRELATION000000000000000")
    (dataset . "investigation-a")
    (data . ((source . "01JPERSON0000000000000000")
             (target . "01JORG000000000000000000000")
             (predicate . "employed-by")
             (note . "public filings")))))

(defconst starintel-wb-test--search-response
  '((status . "ok")
    (rows . [((id . "01JPERSON0000000000000000")
              (doc . ((dtype . "person")
                      (_id . "01JPERSON0000000000000000")
                      (dataset . "investigation-a")
                      (dateAdded . 1735689600)
                      (data . ((fname . "Ada") (lname . "Lovelace"))))))
             ((id . "01JDOMAIN000000000000000000")
              (doc . ((dtype . "domain")
                      (_id . "01JDOMAIN000000000000000000")
                      (dataset . "investigation-a")
                      (data . ((record . "example.com"))))))])
    (bookmark . "g1AAAABbe")))

(defmacro starintel-wb-test-with-client (&rest body)
  "Run BODY against a synchronous fake transport and clean state."
  `(let ((starintel-wb-test--requests nil)
         (starintel-wb-test--responses nil)
         (starintel-wb-test--deferred nil)
         (starintel-api-base-url "http://starintel.test:5000")
         (starintel-api-token "star_sk_v1_secret-token-1234")
         (starintel-api-transport-function #'starintel-wb-test-fake-transport)
         (starintel-api--capabilities-cache nil)
         (starintel-server-current-name nil)
         (starintel-servers nil))
     ,@body))

(defmacro starintel-wb-test-with-deferred (&rest body)
  "Run BODY against a deferred fake transport."
  `(let ((starintel-wb-test--requests nil)
         (starintel-wb-test--responses nil)
         (starintel-wb-test--deferred nil)
         (starintel-api-base-url "http://starintel.test:5000")
         (starintel-api-token "star_sk_v1_secret-token-1234")
         (starintel-api-transport-function #'starintel-wb-test-deferred-transport)
         (starintel-api--capabilities-cache nil)
         (starintel-server-current-name nil)
         (starintel-servers nil))
     ,@body))

;;; ------------------------------------------------------------------
;;; star:// URI model
;;; ------------------------------------------------------------------

(ert-deftest starintel-uri-parse-full ()
  (let ((parsed (starintel-uri-parse "star://local/document/01JABC")))
    (should (equal "local" (plist-get parsed :server)))
    (should (equal "document" (plist-get parsed :kind)))
    (should (equal "01JABC" (plist-get parsed :id)))))

(ert-deftest starintel-uri-parse-empty-server ()
  (let ((parsed (starintel-uri-parse "star:///person/01JABC")))
    (should (equal nil (plist-get parsed :server)))
    (should (equal "person" (plist-get parsed :kind)))
    (should (equal "01JABC" (plist-get parsed :id)))))

(ert-deftest starintel-uri-parse-slash-containing-id ()
  (let ((parsed (starintel-uri-parse "star://remote/search/alice example.com/x")))
    (should (equal "remote" (plist-get parsed :server)))
    (should (equal "search" (plist-get parsed :kind)))
    (should (equal "alice example.com/x" (plist-get parsed :id)))))

(ert-deftest starintel-uri-parse-percent-decodes-id ()
  (let ((parsed (starintel-uri-parse "star://local/search/alice%20smith")))
    (should (equal "alice smith" (plist-get parsed :id)))))

(ert-deftest starintel-uri-parse-rejects-garbage ()
  (should (null (starintel-uri-parse "http://example.com/thing")))
  (should (null (starintel-uri-parse "star://")))
  (should (null (starintel-uri-parse "star://just-a-server"))))

(ert-deftest starintel-uri-format-round-trip ()
  (let* ((uri (starintel-uri-format "local" "search" "ada lovelace 100%"))
         (parsed (starintel-uri-parse uri)))
    (should (equal "local" (plist-get parsed :server)))
    (should (equal "search" (plist-get parsed :kind)))
    (should (equal "ada lovelace 100%" (plist-get parsed :id)))))

(ert-deftest starintel-uri-format-keeps-plain-ids-readable ()
  (should (equal "star://local/document/01JABC"
                 (starintel-uri-format "local" "document" "01JABC"))))

(ert-deftest starintel-uri-format-default-server ()
  (let ((starintel-server-current-name 'remote))
    (should (equal "star://remote/document/01JABC"
                   (starintel-uri-format nil "document" "01JABC")))))

;;; ------------------------------------------------------------------
;;; Server profiles
;;; ------------------------------------------------------------------

(ert-deftest starintel-server-profile-names ()
  (let ((starintel-servers '((local :url "http://127.0.0.1:5000")
                             (remote :url "https://si.example.com"))))
    (should (equal '(local remote)
                   (starintel-server-profile-names)))))

(ert-deftest starintel-server-activate-sets-url-and-name ()
  (let ((starintel-servers '((remote :url "https://si.example.com"))))
    (starintel-server-activate 'remote)
    (should (eq 'remote starintel-server-current-name))
    (should (equal "https://si.example.com" starintel-api-base-url))))

(ert-deftest starintel-server-activate-clears-capability-cache ()
  (let ((starintel-servers '((remote :url "https://si.example.com")))
        (starintel-api--capabilities-cache '((stale . t))))
    (starintel-server-activate 'remote)
    (should (null starintel-api--capabilities-cache))))

(ert-deftest starintel-server-activate-with-token ()
  (let ((starintel-servers '((remote :url "https://si.example.com"
                                     :token "star_sk_v1_remote"))))
    (starintel-server-activate 'remote)
    (should (equal "star_sk_v1_remote" starintel-api-token))))

(ert-deftest starintel-server-activate-with-auth-source ()
  (let* ((starintel-servers '((remote :url "https://si.example.com"
                                      :auth-source (:host "starintel-remote"
                                                    :user "api"))))
         (lookups nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest keys)
                 (push keys lookups)
                 (list (list :secret (lambda () "star_sk_v1_from-auth-source"))))))
      (starintel-server-activate 'remote)
      (should (null starintel-api-token))
      (should (functionp starintel-api-token-function))
      (should (equal "star_sk_v1_from-auth-source"
                     (funcall starintel-api-token-function)))
      (should (equal "starintel-remote" (plist-get (car lookups) :host)))
      (should (equal "api" (plist-get (car lookups) :user))))))

(ert-deftest starintel-server-activate-with-auth-source-shorthand ()
  (let* ((starintel-servers '((remote :url "https://si.example.com"
                                      :auth-source "starintel-remote")))
         (lookups nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest keys)
                 (push keys lookups)
                 (list (list :secret (lambda () "star_sk_v1_shorthand"))))))
      (starintel-server-activate 'remote)
      (should (null starintel-api-token))
      (should (equal "star_sk_v1_shorthand"
                     (funcall starintel-api-token-function)))
      (should (equal "starintel-remote" (plist-get (car lookups) :host)))
      (should (equal "api" (plist-get (car lookups) :user))))))

(ert-deftest starintel-server-activate-unknown-profile-signals ()
  (should-error (starintel-server-activate 'does-not-exist)))

(ert-deftest starintel-server-uri-name ()
  (let ((starintel-server-current-name nil))
    (should (equal "default" (starintel-server-uri-name))))
  (let ((starintel-server-current-name 'remote))
    (should (equal "remote" (starintel-server-uri-name)))))

(ert-deftest starintel-server-switch-completes-and-activates ()
  (let ((starintel-servers '((local :url "http://127.0.0.1:5000")
                             (remote :url "https://si.example.com"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "remote"))
              ((symbol-function 'starintel-status) (lambda (&rest _) 'status-called)))
      (should (eq 'status-called (starintel-server-switch)))
      (should (eq 'remote starintel-server-current-name)))))

;;; ------------------------------------------------------------------
;;; Object identity and titles
;;; ------------------------------------------------------------------

(ert-deftest starintel-object-from-doc ()
  (let ((obj (starintel-object-from-doc starintel-wb-test--person-doc "local")))
    (should (equal "01JPERSON0000000000000000" (starintel-object-id obj)))
    (should (equal "person" (starintel-object-dtype obj)))
    (should (equal "investigation-a" (starintel-object-dataset obj)))
    (should (equal "local" (starintel-object-server obj)))
    (should (equal "star://local/person/01JPERSON0000000000000000"
                   (starintel-object-uri obj)))))

(ert-deftest starintel-object-from-doc-default-server ()
  (let ((starintel-server-current-name 'workbench))
    (should (equal "workbench"
                   (starintel-object-server
                    (starintel-object-from-doc starintel-wb-test--person-doc))))))

(ert-deftest starintel-object-title-person ()
  (should (equal "Ada Lovelace"
                 (starintel-object-title
                  (starintel-object-from-doc starintel-wb-test--person-doc)))))

(ert-deftest starintel-object-title-org ()
  (should (equal "Example Corp"
                 (starintel-object-title
                  (starintel-object-from-doc
                   '((dtype . "org") (_id . "A")
                     (data . ((name . "Example Corp")))))))))

(ert-deftest starintel-object-title-domain ()
  (should (equal "example.com"
                 (starintel-object-title
                  (starintel-object-from-doc
                   '((dtype . "domain") (_id . "A")
                     (data . ((record . "example.com")))))))))

(ert-deftest starintel-object-title-host ()
  (should (equal "host01.example.com (10.0.0.1)"
                 (starintel-object-title
                  (starintel-object-from-doc
                   '((dtype . "host") (_id . "A")
                     (data . ((hostname . "host01.example.com")
                              (ip . "10.0.0.1")))))))))

(ert-deftest starintel-object-title-url ()
  (should (equal "https://example.com/a"
                 (starintel-object-title
                  (starintel-object-from-doc
                   '((dtype . "url") (_id . "A")
                     (data . ((url . "https://example.com/a")))))))))

(ert-deftest starintel-object-title-user ()
  (should (equal "@ada (mastodon)"
                 (starintel-object-title
                  (starintel-object-from-doc
                   '((dtype . "user") (_id . "A")
                     (data . ((name . "ada") (platform . "mastodon")))))))))

(ert-deftest starintel-object-title-email ()
  (should (equal "ada@example.com"
                 (starintel-object-title
                  (starintel-object-from-doc
                   '((dtype . "email") (_id . "A")
                     (data . ((user . "ada") (domain . "example.com")))))))))

(ert-deftest starintel-object-title-relation ()
  (should (equal "01JPERSON0000000000000000 -employed-by-> 01JORG000000000000000000000"
                 (starintel-object-title
                  (starintel-object-from-doc starintel-wb-test--relation-doc)))))

(ert-deftest starintel-object-title-target ()
  (should (equal "example.com @ httpx"
                 (starintel-object-title
                  (starintel-object-from-doc
                   '((dtype . "target") (_id . "A")
                     (data . ((target . "example.com") (actor . "httpx")))))))))

(ert-deftest starintel-object-title-fallback ()
  (should (equal "blob 01JSHORT"
                 (starintel-object-title
                  (starintel-object-from-doc
                   '((dtype . "blob") (_id . "01JSHORTORLONGER")))))))

(ert-deftest starintel-object-title-legacy-flat-fields ()
  (should (equal "flat.example.com"
                 (starintel-object-title
                  (starintel-object-from-doc
                   '((dtype . "domain") (_id . "A") (record . "flat.example.com")))))))

;;; ------------------------------------------------------------------
;;; star:// dispatch
;;; ------------------------------------------------------------------

(ert-deftest starintel-uri-open-document-fetches-and-renders ()
  (starintel-wb-test-with-deferred
   (starintel-uri-open "star://default/document/01JPERSON0000000000000000")
   (let ((entry (starintel-wb-test--release
                 (starintel-wb-test--ok starintel-wb-test--capabilities))))
     (should (string-match-p "/api/v1/capabilities" (plist-get entry :url))))
   (let ((entry (starintel-wb-test--release
                 (starintel-wb-test--ok starintel-wb-test--person-doc))))
     (should (string-match-p "/document/01JPERSON0000000000000000"
                             (plist-get entry :url))))
   (let ((buffer (get-buffer "*StarIntel: Ada Lovelace*")))
     (should buffer)
     (with-current-buffer buffer
       (should (derived-mode-p 'starintel-object-mode))
       (should (string-match-p "01JPERSON0000000000000000" (buffer-string)))
       (should (string-match-p "star://default/person/01JPERSON0000000000000000"
                               (buffer-string)))))))

(ert-deftest starintel-uri-open-search-runs-search ()
  (starintel-wb-test-with-client
   (setq starintel-wb-test--responses
         (list (starintel-wb-test--ok starintel-wb-test--capabilities)
               (starintel-wb-test--ok starintel-wb-test--search-response)))
   (starintel-uri-open "star:///search/ada")
   (should (string-match-p "/api/v1/search"
                           (plist-get (starintel-wb-test-last-request) :url)))
   (let ((buffer (get-buffer "*StarIntel Search*")))
     (should buffer)
     (with-current-buffer buffer
       (should (derived-mode-p 'starintel-search-mode))
       (should (= 2 (length starintel-search--docs)))))))

(ert-deftest starintel-uri-open-unknown-kind-signals ()
  (starintel-wb-test-with-client
   (let ((msg (condition-case err
                  (progn (starintel-uri-open "star:///vobject") nil)
                (user-error (starintel-api-error-message err)))))
     (should (string-match-p "cannot open" msg))
     (should (string-match-p "vobject" msg))
     (should (string-match-p "with no id" msg)))))

(ert-deftest starintel-uri-open-other-server-switches-profile ()
  (starintel-wb-test-with-deferred
   (let ((starintel-servers '((remote :url "https://remote.test:5000"))))
     (starintel-uri-open "star://remote/document/01JPERSON0000000000000000")
     (should (eq 'remote starintel-server-current-name))
     (should (equal "https://remote.test:5000" starintel-api-base-url))
     (starintel-wb-test--release
      (starintel-wb-test--ok starintel-wb-test--capabilities))
     (starintel-wb-test--release
      (starintel-wb-test--ok starintel-wb-test--person-doc)))))

;;; ------------------------------------------------------------------
;;; Generic object buffer
;;; ------------------------------------------------------------------

(ert-deftest starintel-object-open-renders-typed-fields ()
  (starintel-wb-test-with-deferred
   (starintel-object-open "person" "01JPERSON0000000000000000")
   (starintel-wb-test--release
    (starintel-wb-test--ok starintel-wb-test--capabilities))
   (starintel-wb-test--release
    (starintel-wb-test--ok starintel-wb-test--person-doc))
   (with-current-buffer "*StarIntel: Ada Lovelace*"
     (let ((text (buffer-string)))
       (should (string-match-p "dtype +person" text))
       (should (string-match-p "dataset +investigation-a" text))
       (should (string-match-p "fname +Ada" text))
       (should (string-match-p "lname +Lovelace" text))
       (should (string-match-p "Provenance" text))
       (should (string-match-p "trace_id +01JTRACE" text))))))

(ert-deftest starintel-object-copy-id ()
  (starintel-wb-test-with-client
   (let ((obj (starintel-object-from-doc starintel-wb-test--person-doc)))
     (with-current-buffer (starintel-object--buffer obj)
       (starintel-object-copy-id)
       (should (equal "01JPERSON0000000000000000" (car kill-ring)))))))

(ert-deftest starintel-object-copy-uri ()
  (starintel-wb-test-with-client
   (let ((obj (starintel-object-from-doc starintel-wb-test--person-doc)))
     (with-current-buffer (starintel-object--buffer obj)
       (starintel-object-copy-uri)
       (should (equal "star://default/person/01JPERSON0000000000000000"
                      (car kill-ring)))))))

(ert-deftest starintel-object-refresh-refetches ()
  (starintel-wb-test-with-deferred
   (starintel-object-open "person" "01JPERSON0000000000000000")
   (starintel-wb-test--release
    (starintel-wb-test--ok starintel-wb-test--capabilities))
   (starintel-wb-test--release
    (starintel-wb-test--ok starintel-wb-test--person-doc))
   (let ((buffer (get-buffer "*StarIntel: Ada Lovelace*")))
     (should buffer)
     (with-current-buffer buffer
       (let ((requests-before (length starintel-wb-test--deferred)))
         (starintel-object-refresh)
         ;; Capabilities are already cached: refresh issues exactly one
         ;; document fetch.
         (should (= (1+ requests-before) (length starintel-wb-test--deferred)))
         (starintel-wb-test--release
          (starintel-wb-test--ok starintel-wb-test--person-doc)))))))

(ert-deftest starintel-object-unavailable-capability-is-clean-error ()
  (starintel-wb-test-with-client
   ;; Capabilities advertise no document_read and no legacy compatibility.
   (setq starintel-wb-test--responses
         (list (starintel-wb-test--ok
                '((status . "ok")
                  (data . ((features . ((documents . t)))
                           (endpoints . [])
                           (compatibility . ((legacy_routes . :json-false)))))))))
   (let ((err (condition-case e
                  (progn (starintel-object-open "person" "01JX") nil)
                (user-error e))))
     (should err)
     (should (string-match-p "not advertised"
                             (starintel-api-error-message err))))))

;;; ------------------------------------------------------------------
;;; Search results buffer
;;; ------------------------------------------------------------------

(ert-deftest starintel-search-open-renders-tabulated-results ()
  (starintel-wb-test-with-client
   (setq starintel-wb-test--responses
         (list (starintel-wb-test--ok starintel-wb-test--capabilities)
               (starintel-wb-test--ok starintel-wb-test--search-response)))
   (starintel-search-open "ada")
   (let ((buffer (get-buffer "*StarIntel Search*")))
     (should buffer)
     (with-current-buffer buffer
       (should (derived-mode-p 'starintel-search-mode))
       (should (equal "ada" starintel-search--query))
       (should (= 2 (length starintel-search--docs)))
       (should (equal "g1AAAABbe" starintel-search--bookmark))
       (goto-char (point-min))
       (search-forward "01JPERSON0000000000000000")
       (should (equal "01JPERSON0000000000000000"
                      (aref (tabulated-list-get-entry) 4)))))))

(ert-deftest starintel-search-sends-bookmark-on-next-page ()
  (starintel-wb-test-with-client
   (setq starintel-wb-test--responses
         (list (starintel-wb-test--ok starintel-wb-test--capabilities)
               (starintel-wb-test--ok starintel-wb-test--search-response)))
   (starintel-search-open "ada")
   (with-current-buffer "*StarIntel Search*"
     (setq starintel-wb-test--responses
           (list (starintel-wb-test--ok starintel-wb-test--search-response)))
     (starintel-search-next-page))
   (let ((url (plist-get (starintel-wb-test-last-request) :url)))
     (should (string-match-p "bookmark=g1AAAABbe" url)))))

(ert-deftest starintel-search-open-entry-opens-object ()
  (starintel-wb-test-with-deferred
   (setq starintel-wb-test--responses
         (list (starintel-wb-test--ok starintel-wb-test--capabilities)
               (starintel-wb-test--ok starintel-wb-test--search-response)))
   (starintel-search-open "ada")
   (let ((buffer (get-buffer "*StarIntel Search*")))
     (with-current-buffer buffer
       (goto-char (point-min))
       (search-forward "01JPERSON0000000000000000")
       (let ((object (starintel-search--entry-object)))
         (should (equal "person" (starintel-object-dtype object)))
         (should (equal "01JPERSON0000000000000000" (starintel-object-id object))))))))

(ert-deftest starintel-search-copy-uri ()
  (starintel-wb-test-with-client
   (setq starintel-wb-test--responses
         (list (starintel-wb-test--ok starintel-wb-test--capabilities)
               (starintel-wb-test--ok starintel-wb-test--search-response)))
   (starintel-search-open "ada")
   (with-current-buffer "*StarIntel Search*"
     (goto-char (point-min))
     (search-forward "01JPERSON0000000000000000")
     (starintel-search-copy-uri)
     (should (equal "star://default/person/01JPERSON0000000000000000"
                    (car kill-ring))))))

(ert-deftest starintel-search-marks ()
  (starintel-wb-test-with-client
   (setq starintel-wb-test--responses
         (list (starintel-wb-test--ok starintel-wb-test--capabilities)
               (starintel-wb-test--ok starintel-wb-test--search-response)))
   (starintel-search-open "ada")
   (with-current-buffer "*StarIntel Search*"
     (goto-char (point-min))
     (search-forward "01JPERSON0000000000000000")
     (starintel-search-toggle-mark)
     (should (assoc "01JPERSON0000000000000000" starintel-search--marked)))))

(ert-deftest starintel-search-unavailable-capability-is-clean-error ()
  (starintel-wb-test-with-client
   (setq starintel-wb-test--responses
         (list (starintel-wb-test--ok
                '((status . "ok")
                  (data . ((features . ((documents . t)))
                           (endpoints . [])
                           (compatibility . ((legacy_routes . :json-false)))))))))
   (let ((err (condition-case e
                  (progn (starintel-search-open "ada") nil)
                (user-error e))))
     (should err)
     (should (string-match-p "not advertised"
                             (starintel-api-error-message err))))))

;;; ------------------------------------------------------------------
;;; Workbench entry point
;;; ------------------------------------------------------------------

(ert-deftest starintel-entry-opens-status ()
  (starintel-wb-test-with-client
   (cl-letf (((symbol-function 'starintel-status)
              (lambda (&rest _) 'status-shown)))
     (should (eq 'status-shown (starintel))))))

(provide 'starintel-workbench-test)
;;; starintel-workbench-test.el ends here
