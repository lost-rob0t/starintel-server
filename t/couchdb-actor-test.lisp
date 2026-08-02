(in-package :star-server-tests)

(def-suite couchdb-actor-tests
  :description "Hermetic CouchDB actor request, reply, and delete tests")

(in-suite couchdb-actor-tests)

(defun make-couchdb-test-system ()
  (make-actor-system '(:dispatchers
                       (:storage (:workers 2 :strategy :random))
                       :timeout-timer
                       (:resolution 50 :max-size 100))))

(defun wait-for-future-result (future &key (attempts 200) (delay 0.01))
  (loop repeat attempts
        when (sento.future:complete-p future)
          return (sento.future:fresult future)
        do (sleep delay)
        finally (return :not-ready)))

(test couchdb-get-ask-s-with-and-without-revision
  (let ((system (make-couchdb-test-system))
        (calls nil))
    (unwind-protect
         (let* ((handler
                  (star.actors:make-couchdb-get-handler
                   nil
                   :get-fn
                   (lambda (agent database document-id revision)
                     (declare (ignore agent))
                     (push (list database document-id revision) calls)
                     (format nil "~a/~a@~a"
                             database document-id (or revision "current")))))
                (actor
                  (actor-of system
                            :name "couchdb-get-ask-s-test"
                            :dispatcher :storage
                            :receive handler))
                (current
                  (sento.actor:ask-s
                   actor
                   (star.actors:make-couchdb-get-request
                    :database "intel"
                    :document-id "doc-current")
                   :time-out 2))
                (historical
                  (sento.actor:ask-s
                   actor
                   (star.actors:make-couchdb-get-request
                    :database "intel"
                    :document-id "doc-historical"
                    :revision "3-deadbeef")
                   :time-out 2)))
           (is (eq :success (star.actors:couchdb-result-status current)))
           (is (string= "intel/doc-current@current"
                        (star.actors:couchdb-result-value current)))
           (is (eq :success (star.actors:couchdb-result-status historical)))
           (is (string= "intel/doc-historical@3-deadbeef"
                        (star.actors:couchdb-result-value historical)))
           (is (member '("intel" "doc-current" nil) calls :test #'equal))
           (is (member '("intel" "doc-historical" "3-deadbeef")
                       calls
                       :test #'equal)))
      (ac:shutdown system))))

(test couchdb-get-async-asks-return-to-correct-callers
  (let ((system (make-couchdb-test-system)))
    (unwind-protect
         (let* ((handler
                  (star.actors:make-couchdb-get-handler
                   nil
                   :get-fn
                   (lambda (agent database document-id revision)
                     (declare (ignore agent database revision))
                     document-id)))
                (actor
                  (actor-of system
                            :name "couchdb-get-async-test"
                            :dispatcher :storage
                            :receive handler))
                (future-a
                  (sento.actor:ask
                   actor
                   (star.actors:make-couchdb-get-request
                    :document-id "caller-a")
                   :time-out 2))
                (future-b
                  (sento.actor:ask
                   actor
                   (star.actors:make-couchdb-get-request
                    :document-id "caller-b")
                   :time-out 2))
                (result-a (wait-for-future-result future-a))
                (result-b (wait-for-future-result future-b)))
           (is (not (eq :not-ready result-a)))
           (is (not (eq :not-ready result-b)))
           (is (string= "caller-a"
                        (star.actors:couchdb-result-value result-a)))
           (is (string= "caller-b"
                        (star.actors:couchdb-result-value result-b))))
      (ac:shutdown system))))

(test existing-document-insert-completes-deterministically
  (let ((system (make-couchdb-test-system)))
    (unwind-protect
         (let* ((handler
                  (star.actors:make-couchdb-insert-handler
                   nil
                   :exists-fn
                   (lambda (agent database document-id)
                     (declare (ignore agent database document-id))
                     t)
                   :insert-fn
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "Insert must not run for an existing document."))))
                (actor
                  (actor-of system
                            :name "couchdb-existing-insert-test"
                            :dispatcher :storage
                            :receive handler))
                (result
                  (sento.actor:ask-s
                   actor
                   (star.actors:make-couchdb-insert-request
                    :database "intel"
                    :document-id "already-there"
                    :document "{\"_id\":\"already-there\"}")
                   :time-out 2)))
           (is (eq :exists (star.actors:couchdb-result-status result)))
           (is (string= "already-there"
                        (star.actors:couchdb-result-document-id result))))
      (ac:shutdown system))))

(test tell-based-insert-does-not-reply-without-sender
  (let ((system (make-couchdb-test-system))
        (insert-count 0)
        (lock (bt:make-lock)))
    (unwind-protect
         (let* ((handler
                  (star.actors:make-couchdb-insert-handler
                   nil
                   :exists-fn
                   (lambda (agent database document-id)
                     (declare (ignore agent database document-id))
                     (bt:with-lock-held (lock)
                       (plusp insert-count)))
                   :insert-fn
                   (lambda (agent database document)
                     (declare (ignore agent database document))
                     (bt:with-lock-held (lock)
                       (incf insert-count))
                     :inserted)))
                (actor
                  (actor-of system
                            :name "couchdb-tell-insert-test"
                            :dispatcher :storage
                            :receive handler))
                (request
                  (star.actors:make-couchdb-insert-request
                   :database "events"
                   :document-id "event-1"
                   :document "{\"_id\":\"event-1\"}")))
           (tell actor request)
           (loop repeat 100
                 until (bt:with-lock-held (lock)
                         (= 1 insert-count))
                 do (sleep 0.01))
           (is (= 1 (bt:with-lock-held (lock) insert-count)))
           (let ((result (sento.actor:ask-s actor request :time-out 2)))
             (is (eq :exists (star.actors:couchdb-result-status result)))
             (is (= 1 (bt:with-lock-held (lock) insert-count)))))
      (ac:shutdown system))))

(test delete-fetches-current-revision
  (let ((get-calls 0)
        (delete-arguments nil))
    (multiple-value-bind (value revision)
        (star.actors:delete-couchdb-document
         :fake-client
         "intel"
         "doc-1"
         nil
         :get-fn
         (lambda (client database document-id)
           (declare (ignore client))
           (incf get-calls)
           (is (string= "intel" database))
           (is (string= "doc-1" document-id))
           "{\"_id\":\"doc-1\",\"_rev\":\"7-current\"}")
         :delete-fn
         (lambda (client database document-id current-revision)
           (declare (ignore client))
           (setf delete-arguments
                 (list database document-id current-revision))
           :deleted))
      (is (eq :deleted value))
      (is (string= "7-current" revision))
      (is (= 1 get-calls))
      (is (equal '("intel" "doc-1" "7-current")
                 delete-arguments)))))

(test delete-uses-provided-revision-without-fetch
  (let ((get-called-p nil)
        (delete-revision nil))
    (multiple-value-bind (value revision)
        (star.actors:delete-couchdb-document
         :fake-client
         "intel"
         "doc-2"
         "9-explicit"
         :get-fn
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (setf get-called-p t)
           (error "Revision fetch must not run."))
         :delete-fn
         (lambda (client database document-id current-revision)
           (declare (ignore client database document-id))
           (setf delete-revision current-revision)
           :deleted))
      (is (eq :deleted value))
      (is (string= "9-explicit" revision))
      (is-false get-called-p)
      (is (string= "9-explicit" delete-revision)))))

(test couchdb-agent-uses-injected-pool
  (let ((system (make-couchdb-test-system))
        (sentinel-pool (list :injected-pool)))
    (unwind-protect
         (let ((agent (star.actors::make-couchdb-agent
                       system sentinel-pool
                       :dispatcher-id :storage)))
           (is (eq sentinel-pool
                   (sento.agent:agent-get agent #'identity))))
      (ac:shutdown system))))
