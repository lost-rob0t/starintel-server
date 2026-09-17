(in-package :star.expert.shell)

(defun whitespace-char-p (character)
  (find character " \t\n\r" :test #'char=))

(defun tokenize-command-line (line)
  "Split LINE like a small shell. Quotes group tokens; backslash escapes one char."
  (let ((tokens '())
        (buffer '())
        (quote-char nil)
        (escaped-p nil))
    (labels ((flush ()
               (when buffer
                 (push (coerce (nreverse buffer) 'string) tokens)
                 (setf buffer nil))))
      (loop for character across line do
        (cond
          (escaped-p
           (push character buffer)
           (setf escaped-p nil))
          ((char= character #\\)
           (setf escaped-p t))
          (quote-char
           (if (char= character quote-char)
               (setf quote-char nil)
               (push character buffer)))
          ((or (char= character #\")
               (char= character #\'))
           (setf quote-char character))
          ((whitespace-char-p character)
           (flush))
          (t
           (push character buffer))))
      (when escaped-p
        (push #\\ buffer))
      (when quote-char
        (error "Unterminated quoted string"))
      (flush)
      (nreverse tokens))))

(defun token-name (token)
  (string-downcase
   (etypecase token
     (string token)
     (symbol (symbol-name token))
     (integer (princ-to-string token)))))

(defun token= (token name)
  (string= (token-name token) (string-downcase name)))

(defun yes-marker-p (token)
  (or (and (stringp token) (string= token "--yes"))
      (eq token :yes)))

(defun transient-marker-p (token)
  (or (and (stringp token) (string= token "--transient"))
      (eq token :transient)))

(defun option-marker-p (token name)
  (or (and (stringp token)
           (string= token (format nil "--~a" name)))
      (and (keywordp token)
           (string-equal (symbol-name token) name))))

(defun option-value (items name &optional default)
  (loop for tail on items
        for item = (first tail)
        when (option-marker-p item name)
          do (return (or (second tail) default))
        finally (return default)))

(defun positional-items (items option-names &key flags)
  (let ((result '()))
    (loop while items do
      (let ((item (pop items)))
        (cond
          ((some (lambda (name) (option-marker-p item name)) option-names)
           (when items (pop items)))
          ((some (lambda (predicate) (funcall predicate item)) flags)
           nil)
          (t
           (push item result)))))
    (nreverse result)))

(defun stringify (value)
  (etypecase value
    (string value)
    (symbol (string-downcase (symbol-name value)))
    (integer (princ-to-string value))))

(defun join-items (items)
  (format nil "~{~a~^ ~}" (mapcar #'stringify items)))

(defun parse-integer-option (value option-name)
  (cond
    ((null value) nil)
    ((integerp value) value)
    ((stringp value)
     (or (parse-integer value :junk-allowed t)
         (error "~a must be an integer" option-name)))
    (t
     (error "~a must be an integer" option-name))))

(defun command-form (line)
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
    (when (zerop (length trimmed))
      (error "Empty command"))
    (if (char= (char trimmed 0) #\()
        (let ((*read-eval* nil))
          (multiple-value-bind (form position)
              (read-from-string trimmed nil nil)
            (unless form
              (error "Empty command form"))
            (unless (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                (subseq trimmed position))))
              (error "Unexpected input after command form"))
            (unless (listp form)
              (error "Command form must be a list"))
            form))
        (tokenize-command-line trimmed))))

(defun make-request (verb resource raw &key qualifier args confirmed-p)
  (make-instance 'shell-request
                 :verb verb
                 :resource resource
                 :qualifier qualifier
                 :args args
                 :confirmed-p confirmed-p
                 :raw raw))

(defun require-positional (items index description)
  (or (nth index items)
      (error "Missing ~a" description)))

(defun parse-command (line)
  "Parse LINE into a SHELL-REQUEST. No input is EVALed."
  (let* ((items (command-form line))
         (head (first items))
         (confirmed-p (some #'yes-marker-p items)))
    (unless head
      (error "Empty command"))
    (cond
      ((or (token= head "health") (token= head "status"))
       (make-request :health :server line))

      ((token= head "info")
       (make-request :info :server line))

      ((and (token= head "server")
            (second items)
            (token= (second items) "info"))
       (make-request :info :server line))

      ((or (token= head "doc") (token= head "document"))
       (let* ((action (require-positional items 1 "document action"))
              (tail (cddr items)))
         (cond
           ((token= action "get")
            (make-request :get :document line
                          :args (list :id (stringify
                                           (require-positional tail 0 "document id")))))
           ((token= action "search")
            (let* ((limit (or (parse-integer-option (option-value tail "limit") "--limit") 25))
                   (bookmark (option-value tail "bookmark"))
                   (sort (option-value tail "sort"))
                   (positionals (positional-items tail '("limit" "bookmark" "sort"))))
              (unless positionals
                (error "Missing search query"))
              (make-request :search :document line
                            :args (list :query (join-items positionals)
                                        :limit limit
                                        :bookmark (and bookmark (stringify bookmark))
                                        :sort (and sort (stringify sort))))))
           ((or (token= action "submit") (token= action "create"))
            (let* ((positionals (positional-items tail '() :flags (list #'yes-marker-p)))
                   (dtype (stringify (require-positional positionals 0 "document type")))
                   (json-parts (rest positionals)))
              (unless json-parts
                (error "Missing document JSON"))
              (make-request :submit :document line
                            :confirmed-p confirmed-p
                            :args (list :dtype dtype :json (join-items json-parts)))))
           ((token= action "delete")
            (let ((positionals (positional-items tail '() :flags (list #'yes-marker-p))))
              (make-request :delete :document line
                            :confirmed-p confirmed-p
                            :args (list :id (stringify
                                             (require-positional positionals 0 "document id"))))))
           (t
            (make-request :unknown :document line)))))

      ((token= head "search")
       (let* ((tail (rest items))
              (limit (or (parse-integer-option (option-value tail "limit") "--limit") 25))
              (positionals (positional-items tail '("limit"))))
         (unless positionals
           (error "Missing search query"))
         (make-request :search :document line
                       :args (list :query (join-items positionals)
                                   :limit limit))))

      ((token= head "get")
       (make-request :get :document line
                     :args (list :id (stringify
                                      (require-positional (rest items) 0 "document id")))))

      ((or (token= head "target") (token= head "targets"))
       (let* ((action (require-positional items 1 "target action"))
              (tail (cddr items)))
         (cond
           ((token= action "list")
            (make-request :list :target line
                          :args (list :actor (stringify
                                              (require-positional tail 0 "actor")))))
           ((token= action "get")
            (make-request :get :target line
                          :args (list :id (stringify
                                           (require-positional tail 0 "target id")))))
           ((or (token= action "create") (token= action "submit"))
            (let* ((positionals (positional-items tail '()
                                                  :flags (list #'yes-marker-p
                                                               #'transient-marker-p)))
                   (actor (stringify (require-positional positionals 0 "actor")))
                   (json-parts (rest positionals)))
              (unless json-parts
                (error "Missing target JSON"))
              (make-request :create :target line
                            :confirmed-p confirmed-p
                            :args (list :actor actor
                                        :json (join-items json-parts)
                                        :transient (some #'transient-marker-p tail)))))
           (t
            (make-request :unknown :target line)))))

      ((token= head "dataset")
       (let ((action (require-positional items 1 "dataset action"))
             (tail (cddr items)))
         (if (token= action "size")
             (make-request :size :dataset line
                           :args (list :dataset
                                       (stringify
                                        (require-positional tail 0 "dataset name"))))
             (make-request :unknown :dataset line))))

      ((token= head "groups")
       (let* ((tail (rest items))
              (limit (or (parse-integer-option (option-value tail "limit") "--limit") 50)))
         (make-request :list :groups line :args (list :limit limit))))

      ((token= head "messages")
       (let* ((qualifier-token (require-positional items 1 "messages qualifier"))
              (tail (cddr items))
              (limit (or (parse-integer-option (option-value tail "limit") "--limit") 50))
              (positionals (positional-items tail '("limit"))))
         (cond
           ((token= qualifier-token "user")
            (make-request :list :messages line
                          :qualifier :user
                          :args (list :user (stringify
                                             (require-positional positionals 0 "user"))
                                      :limit limit)))
           ((token= qualifier-token "platform")
            (make-request :list :messages line
                          :qualifier :platform
                          :args (list :platform (stringify
                                                 (require-positional positionals 0 "platform"))
                                      :limit limit)))
           ((token= qualifier-token "group")
            (make-request :list :messages line
                          :qualifier :group
                          :args (list :limit limit)))
           (t
            (make-request :unknown :messages line)))))

      ((token= head "social")
       (let* ((qualifier-token (require-positional items 1 "social qualifier"))
              (tail (cddr items))
              (limit (or (parse-integer-option (option-value tail "limit") "--limit") 50))
              (positionals (positional-items tail '("limit"))))
         (if (token= qualifier-token "user")
             (make-request :list :social line
                           :qualifier :user
                           :args (list :user (stringify
                                              (require-positional positionals 0 "user"))
                                       :limit limit))
             (make-request :unknown :social line))))

      ((or (token= head "raw") (token= head "api"))
       (let* ((method-token (require-positional items 1 "HTTP method"))
              (tail (cddr items))
              (positionals (positional-items tail '() :flags (list #'yes-marker-p)))
              (path (stringify (require-positional positionals 0 "API path")))
              (body-parts (rest positionals)))
         (cond
           ((token= method-token "get")
            (make-request :get :raw line :args (list :path path)))
           ((token= method-token "post")
            (make-request :post :raw line
                          :confirmed-p confirmed-p
                          :args (list :path path
                                      :body (and body-parts (join-items body-parts)))))
           ((token= method-token "put")
            (make-request :put :raw line
                          :confirmed-p confirmed-p
                          :args (list :path path
                                      :body (and body-parts (join-items body-parts)))))
           ((token= method-token "delete")
            (make-request :delete :raw line
                          :confirmed-p confirmed-p
                          :args (list :path path)))
           (t
            (make-request :unknown :raw line))))))

      (t
       (make-request :unknown :unknown line)))))

(defun require-plan-arg (plan key)
  (let ((marker (gensym "MISSING")))
    (let ((value (getf (shell-plan-args plan) key marker)))
      (if (eq value marker)
          (error "Planner omitted required argument ~s for ~s"
                 key (shell-plan-operation plan))
          value))))

(defun perform-operation (session plan)
  (let ((client (shell-session-client session)))
    (case (shell-plan-operation plan)
      (:health
       (star.api.client:health client))
      (:server-info
       (star.api.client:server-info client))
      (:document-get
       (star.api.client:get-document client (require-plan-arg plan :id)))
      (:document-search
       (star.api.client:fts
        client
        :q (require-plan-arg plan :query)
        :limit (or (getf (shell-plan-args plan) :limit) 25)
        :bookmark (getf (shell-plan-args plan) :bookmark)
        :sort (getf (shell-plan-args plan) :sort)))
      (:document-submit
       (star.api.client:submit-document
        client
        (require-plan-arg plan :json)
        (require-plan-arg plan :dtype)))
      (:document-delete
       (star.api.client:api-request
        client
        (format nil "/document/~a" (require-plan-arg plan :id))
        :method :delete))
      (:target-list
       (star.api.client:get-targets client (require-plan-arg plan :actor)))
      (:target-get
       (star.api.client:get-document client (require-plan-arg plan :id)))
      (:target-create
       (star.api.client:new-target
        client
        (require-plan-arg plan :json)
        (require-plan-arg plan :actor)
        (not (null (getf (shell-plan-args plan) :transient)))))
      (:dataset-size
       (star.api.client:dataset-size client (require-plan-arg plan :dataset)))
      (:groups
       (star.api.client:groups
        client :limit (or (getf (shell-plan-args plan) :limit) 50)))
      (:messages-by-user
       (star.api.client:messages-by-user
        client
        :user (require-plan-arg plan :user)
        :limit (or (getf (shell-plan-args plan) :limit) 50)))
      (:messages-by-platform
       (star.api.client:messages-by-platform
        client
        :platform (require-plan-arg plan :platform)
        :limit (or (getf (shell-plan-args plan) :limit) 50)))
      (:messages-by-group
       (star.api.client:messages-by-group
        client :limit (or (getf (shell-plan-args plan) :limit) 50)))
      (:social-by-user
       (star.api.client:social-posts-by-user
        client
        :user (require-plan-arg plan :user)
        :limit (or (getf (shell-plan-args plan) :limit) 50)))
      (:raw-get
       (star.api.client:api-request client (require-plan-arg plan :path)))
      (:raw-post
       (star.api.client:api-request client (require-plan-arg plan :path)
                                    :method :post
                                    :content (getf (shell-plan-args plan) :body)))
      (:raw-put
       (star.api.client:api-request client (require-plan-arg plan :path)
                                    :method :put
                                    :content (getf (shell-plan-args plan) :body)))
      (:raw-delete
       (star.api.client:api-request client (require-plan-arg plan :path)
                                    :method :delete))
      (otherwise
       (error "No executor for operation ~s" (shell-plan-operation plan))))))

(defun make-shell-session (&key client
                                (base-url "http://127.0.0.1:5000")
                                api-key)
  (let* ((client (or client
                     (let ((base (star.api.client:make-star-client
                                  :base-url base-url)))
                       (if api-key
                           (star.api.client:client-with-api-key base api-key)
                           base))))
         (engine (lisa:make-inference-engine))
         (session (make-instance 'shell-session
                                 :client client
                                 :engine engine)))
    (install-shell-rules engine)
    session))

(defparameter *operation-catalog*
  '("health | status"
    "info | server info"
    "doc get ID"
    "doc search QUERY [--limit N] [--bookmark B] [--sort FIELD]"
    "doc submit DTYPE JSON --yes"
    "doc delete ID --yes"
    "target list ACTOR"
    "target get ID"
    "target create ACTOR JSON [--transient] --yes"
    "dataset size DATASET"
    "groups [--limit N]"
    "messages user USER [--limit N]"
    "messages platform PLATFORM [--limit N]"
    "messages group [--limit N]"
    "social user USER [--limit N]"
    "raw get PATH"
    "raw post PATH [JSON] --yes"
    "raw put PATH [JSON] --yes"
    "raw delete PATH --yes"
    "why"
    "rules"
    "help"
    "quit | exit"))

(defun help-text ()
  (with-output-to-string (stream)
    (format stream "StarIntel Lisa expert shell~%~%")
    (format stream "Commands:~%")
    (dolist (entry *operation-catalog*)
      (format stream "  ~a~%" entry))
    (format stream "~%Lisp syntax is also accepted, e.g. (doc search \"alice\" :limit 10).~%")
    (format stream "Mutation rules require --yes (or :yes in Lisp syntax). Input forms are read as data with *READ-EVAL* disabled.~%")))

(defun last-explanation (session)
  (let ((plan (shell-session-last-plan session)))
    (if plan
        (list :rule (shell-plan-rule-name plan)
              :operation (shell-plan-operation plan)
              :risk (shell-plan-risk plan)
              :reason (shell-plan-reason plan)
              :trace (copy-tree (shell-session-trace session)))
        (list :message "No Lisa plan has run in this session yet."))))

(defun meta-command-result (session line)
  (let ((trimmed (string-downcase
                  (string-trim '(#\Space #\Tab #\Newline #\Return) line))))
    (cond
      ((member trimmed '("help" "?") :test #'string=)
       (make-result :success-p t :operation :help :code :ok :value (help-text)))
      ((string= trimmed "rules")
       (make-result :success-p t :operation :rules :code :ok
                    :value (copy-list *operation-catalog*)))
      ((string= trimmed "why")
       (make-result :success-p t :operation :why :code :ok
                    :value (last-explanation session)))
      (t nil))))

(defun run-command (session line)
  "Run one LINE through the Lisa planner and return a SHELL-RESULT."
  (or (meta-command-result session line)
      (handler-case
          (let ((request (parse-command line)))
            (setf (shell-session-last-result session) nil
                  (shell-session-last-plan session) nil
                  (shell-session-trace session) nil)
            (let ((*current-session* session))
              (trace-event :request
                           :verb (shell-request-verb request)
                           :resource (shell-request-resource request)
                           :qualifier (shell-request-qualifier request)
                           :confirmed-p (shell-request-confirmed-p request))
              (lisa:with-inference-engine ((shell-session-engine session))
                (lisa:assert-instance request)
                (lisa:run))
              (setf (shell-session-trace session)
                    (nreverse (shell-session-trace session)))
              (or (shell-session-last-result session)
                  (make-result :success-p nil
                               :code :no-result
                               :message "Lisa reached quiescence without producing a result."))))
        (error (condition)
          (let ((result (make-result :success-p nil
                                     :code :invalid-command
                                     :message (princ-to-string condition))))
            (setf (shell-session-last-result session) result)
            result)))))

(defun json-ish-p (value)
  (and (consp value)
       (member (first value) '(:obj :array) :test #'eq)))

(defun print-value (value stream)
  (cond
    ((null value)
     nil)
    ((stringp value)
     (write-string value stream)
     (unless (and (plusp (length value))
                  (char= (char value (1- (length value))) #\Newline))
       (terpri stream)))
    ((json-ish-p value)
     (write-line (jsown:to-json value) stream))
    (t
     (pprint value stream))))

(defun result-json (result)
  (jsown:to-json
   (jsown:new-js
     ("ok" (if (shell-result-success-p result) :true :false))
     ("operation" (and (shell-result-operation result)
                       (string-downcase
                        (symbol-name (shell-result-operation result)))))
     ("code" (and (shell-result-code result)
                  (string-downcase (symbol-name (shell-result-code result)))))
     ("message" (shell-result-message result))
     ("value" (let ((value (shell-result-value result)))
                (cond
                  ((or (null value) (stringp value) (numberp value)) value)
                  ((json-ish-p value) value)
                  (t (prin1-to-string value)))))))))

(defun render-result (result &key (stream *standard-output*) json)
  (if json
      (write-line (result-json result) stream)
      (if (shell-result-success-p result)
          (if (shell-result-value result)
              (print-value (shell-result-value result) stream)
              (format stream "ok~%"))
          (format stream "error[~(~a~)]: ~a~%"
                  (or (shell-result-code result) :error)
                  (or (shell-result-message result) "operation failed"))))
  result)

(defun quit-command-p (line)
  (member (string-downcase
           (string-trim '(#\Space #\Tab #\Newline #\Return) line))
          '("quit" "exit" ":q")
          :test #'string=))

(defun start-repl (session &key (input *standard-input*) (output *standard-output*))
  (format output "StarIntel expert shell (Lisa). Type 'help' for commands.~%")
  (loop
    (format output "star> ")
    (force-output output)
    (let ((line (read-line input nil :eof)))
      (when (eq line :eof)
        (return t))
      (when (quit-command-p line)
        (return t))
      (unless (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
        (render-result (run-command session line) :stream output)))))

(defun default-base-url ()
  (or (uiop:getenv "STAR_SERVER_URL")
      "http://127.0.0.1:5000"))

(defun parse-main-args (args)
  (let ((base-url (default-base-url))
        (api-key (uiop:getenv "STAR_API_KEY"))
        (command nil)
        (json nil)
        (help nil)
        (positionals '()))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((member arg '("--url" "-u") :test #'string=)
           (setf base-url (or (pop args) (error "--url requires a value"))))
          ((member arg '("--api-key" "-k") :test #'string=)
           (setf api-key (or (pop args) (error "--api-key requires a value"))))
          ((member arg '("--command" "-c") :test #'string=)
           (setf command (or (pop args) (error "--command requires a value"))))
          ((string= arg "--json")
           (setf json t))
          ((member arg '("--help" "-h") :test #'string=)
           (setf help t))
          (t
           (push arg positionals)))))
    (when (and (null command) positionals)
      (setf command (format nil "~{~a~^ ~}" (nreverse positionals))))
    (values base-url api-key command json help)))

(defun main ()
  (handler-case
      (multiple-value-bind (base-url api-key command json help)
          (parse-main-args (uiop:command-line-arguments))
        (when help
          (write-string (help-text))
          (uiop:quit 0))
        (let ((session (make-shell-session :base-url base-url :api-key api-key)))
          (if command
              (let ((result (run-command session command)))
                (render-result result :json json)
                (uiop:quit (if (shell-result-success-p result) 0 2)))
              (progn
                (start-repl session)
                (uiop:quit 0)))))
    (error (condition)
      (format *error-output* "star-expert: ~a~%" condition)
      (uiop:quit 2))))
