(uiop:define-package :star.actors.katana
  (:use :cl
   :sento.agent
        :sento.actor
   :sento.actor-system
        :sento.actor-context
   :star.actors)
  (:import-from :spec
                #:doc-id
                #:encode
                #:new-url
                #:new-domain
                #:new-relation)
  (:documentation "Katana runner + parser actors (non-blocking via task-async)."))

(in-package :star.actors.katana)

;;; ----------------------------------------------------------------------
;;; Globals / config
;;; ----------------------------------------------------------------------

(defvar *katana* nil
  "Actor that launches katana and streams output lines.")

(defvar *katana-parser* nil
  "Actor that parses katana output lines into StarIntel docs.")

(defparameter *katana-base-args* '("-silent" "-json")
  "Default katana flags. Adjust if your katana uses different JSONL output flags.")

(defparameter *katana-extra-args* '()
  "Extra katana flags appended after base args and before the -u target.")

;;; ----------------------------------------------------------------------
;;; Helpers
;;; ----------------------------------------------------------------------

(defun katana/doc->json (doc)
  "Spec rule: (encode doc) first, then jsown:to-json."
  (jsown:to-json (spec:encode doc)))

(defun katana/url->host (url)
  "Best-effort URL -> host extractor (no deps)."
  (when (and url (stringp url) (> (length url) 0))
    (let* ((u url)
           (scheme-pos (search "://" u))
           (start (if scheme-pos (+ scheme-pos 3) 0))
           (rest (subseq u start))
           (slash (position #\/ rest))
           (hostport (if slash (subseq rest 0 slash) rest))
           (at (position #\@ hostport))
           (hostport2 (if at (subseq hostport (1+ at)) hostport))
           (colon (position #\: hostport2)))
      (string-downcase
       (string-trim '(#\Space #\Tab #\Return #\Newline)
                    (if colon (subseq hostport2 0 colon) hostport2))))))

(defun katana/val-any (jdoc keys)
  (loop for k in keys
        for v = (ignore-errors (jsown:val-safe jdoc k))
        when (and v (stringp v) (> (length v) 0))
          do (return v)
        finally (return nil)))

(defun katana/maybe-url-line-p (s)
  (and (stringp s)
       (or (search "http://" s) (search "https://" s))))

(defun katana/publish (json &key routing-key type)
  (publish star.actors:*producer-agent*
           :body json
           :routing-key routing-key
           :properties (when type (list (cons :type type)))))

;;; ----------------------------------------------------------------------
;;; Parser actor
;;; Message format (from runner): (list :dataset <str> :target-id <ulid> :line <string>)
;;; ----------------------------------------------------------------------

(defun katana/emit-from-url (&key dataset target-id url-str)
  (let* ((host-str (katana/url->host url-str))
         (url-doc  (spec:new-url dataset :url url-str))
         (url-id   (spec:doc-id url-doc))
         (url-json (katana/doc->json url-doc)))

    (katana/publish url-json :routing-key "documents.new.url" :type "url")
    (log:info "[katana-parser] new-url id=~a url=~s" url-id url-str)
    (log-actor-event "katana-parser" :event-type "url-created" :details url-json :source-id target-id)

    ;; target -> url relation (always)
    (when target-id
      (let* ((rel1 (spec:new-relation dataset target-id url-id
                                      :note "katana-url"
                                      :predicate "contains"))
             (rel1-json (katana/doc->json rel1)))
        (katana/publish rel1-json :routing-key "documents.new.relation" :type "relation")
        (log-actor-event "katana-parser" :event-type "relation-created" :details rel1-json :source-id target-id)))

    ;; domain + url->domain relation (optional but nice)
    (when (and host-str (> (length host-str) 0))
      (let* ((dom-doc  (spec:new-domain dataset :record host-str))
             (dom-id   (spec:doc-id dom-doc))
             (dom-json (katana/doc->json dom-doc)))
        (katana/publish dom-json :routing-key "documents.new.domain" :type "domain")
        (log:info "[katana-parser] new-domain id=~a domain=~s" dom-id host-str)
        (log-actor-event "katana-parser" :event-type "domain-created" :details dom-json :source-id target-id)

        (let* ((rel2 (spec:new-relation dataset url-id dom-id
                                        :note "katana-domain"
                                        :predicate "contains"))
               (rel2-json (katana/doc->json rel2)))
          (katana/publish rel2-json :routing-key "documents.new.relation" :type "relation")
          (log-actor-event "katana-parser" :event-type "relation-created" :details rel2-json :source-id target-id))))))

(defun katana/parse-line (&key dataset target-id line)
  (cond
    ((or (null dataset) (not (stringp dataset)) (string= dataset ""))
     (log:warn "[katana-parser] missing dataset; dropping line")
     (log-actor-event "katana-parser" :event-type "missing-dataset" :details (or line "") :source-id target-id))

    ((or (null line) (not (stringp line)) (string= line ""))
     nil)

    (t
     ;; Katana output can be JSONL or plain URL lines depending on flags/version.
     (handler-case
         (let* ((jdoc (jsown:parse line))
                (url-str (or (katana/val-any jdoc '("url" "URL" "endpoint" "uri" "request"))
                             (katana/val-any jdoc '("output")))))
           (cond
             ((and url-str (> (length url-str) 0))
              (log-actor-event "katana-parser" :event-type "parse-start" :details line :source-id target-id)
              (katana/emit-from-url :dataset dataset :target-id target-id :url-str url-str)
              (log-actor-event "katana-parser" :event-type "parse-success" :details url-str :source-id target-id))
             (t
              (log:warn "[katana-parser] json line missing url-ish field; skipping")
              (log-actor-event "katana-parser" :event-type "missing-url" :details line :source-id target-id))))
       (error (e)
         (if (katana/maybe-url-line-p line)
             (progn
               (log:debug "[katana-parser] non-json url line; treating as url")
               (katana/emit-from-url :dataset dataset :target-id target-id :url-str line))
             (progn
               (log:error "[katana-parser] parse-error err=~a line=~a" e line)
               (log-actor-event "katana-parser"
                                :event-type "parse-error"
                                :details (format nil "err=~a line=~a" e line)
                                :source-id target-id))))))))

(defun start-katana-parser ()
  (log:info "[katana-parser] Initializing")
  (setf *katana-parser*
        (actor-of star.actors:*sys*
                  :name "katana-parser"
                  :receive (lambda (msg)
                             (let ((dataset   (getf msg :dataset))
                                   (target-id (getf msg :target-id))
                                   (line      (getf msg :line)))
                               (with-context (star.actors:*sys*)
                                 (task-async
                                  (lambda ()
                                    (katana/parse-line :dataset dataset
                                                       :target-id target-id
                                                       :line line))))))))
  (star.actors:register-actor "katana-parser" *katana-parser*)
  (log:info "[katana-parser] Registered"))

;;; ----------------------------------------------------------------------
;;; Runner actor
;;; Receives a TARGET doc (JSOWN) via (tell *katana* jdoc)
;;; ----------------------------------------------------------------------

(defun katana/run (&key dataset target-id target-str)
  (let* ((cmd (append (list "katana")
                      *katana-base-args*
                      *katana-extra-args*
                      (list "-u" target-str)))
         (t0 (get-internal-real-time))
         (n 0))
    (labels ((elapsed-seconds ()
               (/ (- (get-internal-real-time) t0)
                  internal-time-units-per-second)))
      (log:info "[katana] scan-start id=~a target=~s dataset=~a cmd=~s"
                target-id target-str dataset cmd)
      (log-actor-event "katana" :event-type "scan-start"
                                :details (format nil "~a" target-id)
                                :source-id target-id)

      (handler-case
          (let* ((proc (uiop:launch-program cmd
                                            :output :stream
                                            :error-output :output
                                            :ignore-error-status t))
                 (out  (uiop:process-info-output proc)))
            (unwind-protect
                 (loop for line = (read-line out nil nil)
                       while line
                       do (incf n)
                          (when *katana-parser*
                            (tell *katana-parser*
                                  (list :dataset dataset
                                        :target-id target-id
                                        :line line)))
                          (when (zerop (mod n 500))
                            (log:info "[katana] streamed lines=~d id=~a elapsed=~,,2fs"
                                      n target-id (elapsed-seconds))))
              (ignore-errors (close out)))

            (let ((code (uiop:wait-process proc)))
              (log:info "[katana] scan-finished id=~a exit=~d lines=~d elapsed=~,,2fs"
                        target-id code n (elapsed-seconds))
              (log-actor-event "katana" :event-type "scan-exit" :details (format nil "~d" code) :source-id target-id)
              (log-actor-event "katana" :event-type "scan-count" :details (format nil "~d" n) :source-id target-id)
              (log-actor-event "katana" :event-type "scan-finished" :details (format nil "~a" target-id) :source-id target-id)))

        (error (e)
          (log:error "[katana] scan-crashed id=~a err=~a" target-id e)
          (log-actor-event "katana" :event-type "scan-error" :details (format nil "~a" e) :source-id target-id))))))

(defun start-katana ()
  (log:info "[katana] Initializing runner + parser")

  (unless *katana-parser*
    (start-katana-parser))

  (setf *katana*
        (actor-of star.actors:*sys*
                  :name "katana"
                  :receive (lambda (jdoc)
                             (let* ((dataset   (jsown:val-safe jdoc "dataset"))
                                    (target-id (jsown:val-safe jdoc "_id"))
                                    (target-str (or (jsown:val-safe jdoc "target")
                                                    (jsown:val-safe jdoc "url"))))
                               (cond
                                 ((or (null dataset) (string= dataset ""))
                                  (log:warn "[katana] missing dataset in target message")
                                  (log-actor-event "katana" :event-type "missing-dataset"
                                                            :details (jsown:to-json jdoc)))

                                 ((or (null target-id) (string= target-id ""))
                                  (log:warn "[katana] missing _id in target message")
                                  (log-actor-event "katana" :event-type "missing-id"
                                                            :details (jsown:to-json jdoc)))

                                 ((or (null target-str) (not (stringp target-str)) (string= target-str ""))
                                  (log:warn "[katana] missing target string in message")
                                  (log-actor-event "katana" :event-type "missing-target"
                                                            :details (jsown:to-json jdoc)
                                                            :source-id target-id))

                                 (t
                                  (log:info "[katana] received target id=~a target=~s dataset=~a"
                                            target-id target-str dataset)
                                  (log-actor-event "katana" :event-type "target-received"
                                                            :details (format nil "~a" target-id)
                                                            :source-id target-id)

                                  (with-context (star.actors:*sys*)
                                    (task-async
                                     (lambda ()
                                       (katana/run :dataset dataset
                                                   :target-id target-id
                                                   :target-str target-str))))))))))
  (star.actors:register-actor "katana" *katana*)
  (log:info "[katana] Registered"))

;;; ----------------------------------------------------------------------
;;; Startup hook
;;; ----------------------------------------------------------------------

(nhooks:add-hook star:*actors-start-hook* #'start-katana)
