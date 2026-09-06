(uiop:define-package   :star.actors.matcher
  (:use       :sento.actor :sento.agent :sento.actor-system :sento.actor-context :cl :star.actors #:star.databases.couchdb)
  (:nicknames :matchers)
  (:import-from :serapeum :dict :dict* :@ :maphash-return)
  (:documentation "A actor that implements a simple way to run code against documents matching a condition.")
  (:export
   #:add-pattern
   #:pattern
   #:transientp
   #:match-fn
   #:decode-fn
   #:subcriptions
   #:define-pattern
   #:pattern-name
   #:notify-subs
   #:pattern-actor
   #:*pattern-agent*
   #:*pattern-actor*
   #:*url-extractor*
   #:*url-extractor-fields*
   #:*url-regex*
   #:start-url-extractor))






(in-package :star.actors.matcher)


(defclass pattern ()
  ((name :initarg :name :initform (error "Pattern requires a name") :accessor pattern-name)
   (transient :initarg :transient :initform nil :accessor transientp :documentation "Messages created by this pattern will be considered transient, and will not be saved to a database, use this if your pattern creates lots of targets that are not needed again later.")
   (match-fn :initarg :match-fn :accessor match-fn :initform #'identity)
   (subs :initarg :subscriptions :accessor subcriptions :type list :documentation "List of actor names to tell them about the message, it just runs (tell name <document>)"))

  (:documentation "Document pattern matcher, based on a match-fn you supply. When the handler returns non nill, the subs actors are then sent a message with the message being the document"))

;; Accessor documentation for pattern
(setf (documentation 'MATCH-FN 'function)
  "The matching function of =pattern=: document in, targets out.")
(setf (documentation 'TRANSIENTP 'function)
  "True when =pattern= creates transient targets that are not persisted.")
(setf (documentation 'PATTERN-NAME 'function)
  "The =name= slot of =pattern=.")
(setf (documentation 'SUBCRIPTIONS 'function)
  "The =subcriptions= slot of =pattern=: extracted fields this pattern watches.")


;; Accessor documentation for pattern
(setf (documentation 'MATCH-FN 'function)
"The =match-fn= slot of =pattern=.")
(setf (documentation 'PATTERN-NAME 'function)
"The =name= slot of =pattern=.")
(setf (documentation 'SUBCRIPTIONS 'function)
"The =subs= slot of =pattern=.")
(setf (documentation 'TRANSIENTP 'function)
"The =transient= slot of =pattern=.")


(setf (documentation 'PATTERN-NAME 'function)
"The =pattern-name= slot of =pattern=.")



(defgeneric notify-subs (self message)
  (:documentation "Notify subscribers about matched documents."))


(defmethod notify-subs ((pattern pattern) message)

  (when (funcall (match-fn pattern) message)
    (loop for actor in (subcriptions pattern)
          do (log:debug actor)
          do (act:tell actor message))))

(defmacro define-pattern ((name subs) &body body)
  "Define a named pattern: NAME with SUBS substitutions; BODY builds the document."
  `(make-instance 'pattern :name ,name  :subscriptions ,subs
                           :match-fn ,@body))
(defvar *pattern-lock* (bt:make-lock "patterns"))

(defun add-pattern (pattern)
  "Register PATTERN with the matcher actor so extracted fields are routed."
  (bt:with-lock-held (*pattern-lock*)
    (push pattern star:*document-patterns*)))


(defparameter *url-regex* (ppcre:create-scanner "https?:\\/\\/(www\\.)?\[-a-zA-Z0-9@:%.\_\\+~#=\]{1,256}\\.\[a-zA-Z0-9()\]{1,6}\\b(\[-a-zA-Z0-9()@:%\_\\+.~#?&//=\]\*)")
  "Regular expression used by the URL extractor to find URLs in documents.")

(defparameter *url-extractor-fields* '("content" "bio")
  "Document fields scanned by the URL extractor.")

(defparameter *url-extractor* nil "")


(defun start-url-extractor ()
  "Start the URL extractor actor over every registered pattern."
  (setf *url-extractor* (actor-of *sys*
                                  :name "url-extractor"
                                  :receive
                                  (lambda (msg)
                                    (let* ((dataset (jsown:val msg "dataset"))
                                           (docs
                                             ;; TODO use from-json instead of this rawdogging json?
                                             (alexandria:flatten (loop for field in *url-extractor-fields*
                                                                       for content = (or (jsown:val-safe msg field) "")
                                                                       collect (loop for url in (ppcre:all-matches-as-strings *url-regex* content)
                                                                                     for doc = (spec:new-url dataset :url url :content "")
                                                                                     for rel = (spec:new-relation dataset (jsown:val msg "_id") (spec:doc-id doc) "extracted")
                                                                                     collect (list rel doc))))))

                                      (when docs
                                        (loop for doc in docs
                                              for data = (as-json doc)
                                              do (log:error "documents.new.~a" (spec:doc-type doc))
                                              do (publish *producer-agent* :body data :properties (list (cons :type (spec:doc-type doc))) :routing-key (format nil "documents.new.~a" (spec:doc-type doc)))))))))

  (add-pattern (define-pattern ("url-extractor" (list *url-extractor*))
                 (lambda (msg)
                   (string= (jsown:val msg "dtype") "message")))))





(nhooks:add-hook star:*actors-start-hook* #'start-url-extractor)


