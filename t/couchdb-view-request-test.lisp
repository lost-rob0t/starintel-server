(in-package :star-server-tests)

(def-suite couchdb-view-request-tests
  :description "CouchDB view URI, body, validation, and result-shape contracts")

(in-suite couchdb-view-request-tests)

(defun test-view-client ()
  (cl-couch:new-couchdb "couch.example" 5984))

(defun build-test-view-request (&rest arguments)
  (apply #'star.databases.couchdb:build-couchdb-view-request
         (test-view-client) "records" "fixture" "by_key" arguments))

(defun request-uri-contains-p (request fragment)
  (search fragment
          (star.databases.couchdb:couchdb-view-request-uri request)
          :test #'char=))

(test default-request-is-get-with-query-only-false-values
  (let ((request (build-test-view-request)))
    (is (eq :get
            (star.databases.couchdb:couchdb-view-request-method request)))
    (is (string=
         "http://couch.example:5984/records/_design/fixture/_view/by_key?limit=50&skip=0&descending=false&include_docs=false&reduce=false&update=true"
         (star.databases.couchdb:couchdb-view-request-uri request)))
    (is (null (star.databases.couchdb:couchdb-view-request-body request)))
    (is-true (request-uri-contains-p request "descending=false"))
    (is-true (request-uri-contains-p request "include_docs=false"))
    (is-true (request-uri-contains-p request "reduce=false"))))

(test numeric-and-boolean-options-are-query-parameters
  (let ((request
          (build-test-view-request
           :limit 7 :skip 2 :descending t :include-docs t :reduce nil)))
    (is-true (request-uri-contains-p request "limit=7"))
    (is-true (request-uri-contains-p request "skip=2"))
    (is-true (request-uri-contains-p request "descending=true"))
    (is-true (request-uri-contains-p request "include_docs=true"))
    (is-true (request-uri-contains-p request "reduce=false"))
    (is (null (star.databases.couchdb:couchdb-view-request-body request)))))

(test single-and-compound-keys-are-json-query-values
  (let ((single (build-test-view-request :key "alpha"))
        (compound (build-test-view-request :key '("alpha" 2))))
    (is-true (request-uri-contains-p single "key=%22alpha%22"))
    (is-true
     (request-uri-contains-p compound "key=%5B%22alpha%22%2C2%5D"))))

(test key-ranges-use-canonical-couchdb-names
  (let ((request
          (build-test-view-request
           :start-key '("alpha" 1)
           :end-key '("alpha" 3))))
    (is-true
     (request-uri-contains-p request "startkey=%5B%22alpha%22%2C1%5D"))
    (is-true
     (request-uri-contains-p request "endkey=%5B%22alpha%22%2C3%5D"))
    (is-false (request-uri-contains-p request "start_key="))
    (is-false (request-uri-contains-p request "end_key="))))

(test grouped-reduction-options-are-transmitted
  (let ((grouped (build-test-view-request :reduce t :group t))
        (level (build-test-view-request :reduce t :group-level 2)))
    (is-true (request-uri-contains-p grouped "group=true"))
    (is-true (request-uri-contains-p level "group_level=2"))))

(test update-is-a-closed-query-enum
  (let ((false-request (build-test-view-request :update nil))
        (lazy-request (build-test-view-request :update "lazy")))
    (is-true (request-uri-contains-p false-request "update=false"))
    (is-true (request-uri-contains-p lazy-request "update=lazy"))
    (signals star.databases.couchdb:view-query-error
      (build-test-view-request :update "eventually"))))

(test multi-key-requests-use-post-with-keys-only-body
  (let ((request
          (build-test-view-request
           :keys '("alpha" ("beta" 2))
           :limit 4
           :include-docs nil)))
    (is (eq :post
            (star.databases.couchdb:couchdb-view-request-method request)))
    (is (string= "{\"keys\":[\"alpha\",[\"beta\",2]]}"
                 (star.databases.couchdb:couchdb-view-request-body request)))
    (is-true (request-uri-contains-p request "limit=4"))
    (is-true (request-uri-contains-p request "include_docs=false"))
    (is-false (request-uri-contains-p request "keys="))))

(test invalid-requests-fail-before-transport
  (let* ((calls 0)
         (star.databases.couchdb:*couchdb-view-transport*
          (lambda (client request)
            (declare (ignore client request))
            (incf calls)
            (error "transport must not run"))))
    (flet ((rejects (&rest arguments)
             (signals star.databases.couchdb:view-query-error
               (apply #'star.databases.couchdb:query-view
                      (test-view-client) "records" "fixture" "by_key"
                      arguments))))
      (rejects :reduce t :include-docs t)
      (rejects :reduce nil :group t)
      (rejects :reduce nil :group-level 1)
      (rejects :key "alpha" :keys '("alpha"))
      (rejects :key "alpha" :start-key "alpha")
      (rejects :keys '("alpha") :end-key "omega")
      (rejects :limit -1)
      (rejects :skip -1)
      (rejects :reduce t :group-level -1)
      (rejects :descending :false)
      (rejects :include-docs :false)
      (rejects :reduce :false))
    (is (zerop calls))))

(defun typed-result-for-response (name response &rest arguments)
  (let ((star.databases.couchdb:*couchdb-view-transport*
          (lambda (client request)
            (declare (ignore client request))
            response)))
    (apply #'star.databases.couchdb:execute-registered-view
           name (test-view-client) "records" arguments)))

(test map-document-and-reduced-results-have-distinct-types
  (let* ((map-response
           "{\"rows\":[{\"id\":\"map-1\",\"key\":\"alpha\",\"value\":1}]}")
         (document-response
           "{\"rows\":[{\"id\":\"doc-1\",\"key\":\"alpha\",\"value\":null,\"doc\":{\"_id\":\"doc-1\",\"dataset\":\"records\"}}]}")
         (reduced-response
           "{\"rows\":[{\"key\":\"alpha\",\"value\":3}]}")
         (map-result
           (typed-result-for-response
            'star.databases.couchdb:users-by-platform map-response
            :include-docs nil :reduce nil))
         (document-result
           (typed-result-for-response
            'star.databases.couchdb:users-by-platform document-response
            :include-docs t :reduce nil :sort-fn nil))
         (reduced-result
           (typed-result-for-response
            'star.databases.couchdb:count-by-dtype reduced-response
            :include-docs nil :reduce t :group t)))
    (is (typep map-result 'star.databases.couchdb:view-map-result))
    (is (typep document-result
               'star.databases.couchdb:view-document-result))
    (is (typep reduced-result
               'star.databases.couchdb:view-reduced-result))
    (is-false (typep reduced-result
                     'star.databases.couchdb:view-document-result))
    (is (= 1 (length
              (star.databases.couchdb:view-document-result-documents
               document-result))))))

(test view-transport-preserves-json-literals-and-list-contract
  (let* ((raw "{\"rows\":[{\"id\":\"literal-doc\",\"key\":[\"a\",2],\"value\":null,\"doc\":{\"false\":false,\"true\":true,\"null\":null,\"array\":[],\"object\":{},\"nested\":[false,null,[],{}]}}]}")
         (star.databases.couchdb:*couchdb-view-transport*
           (lambda (client request)
             (declare (ignore client request))
             raw))
         (response (star.databases.couchdb:query-view
                    (test-view-client) "records" "fixture" "by_key" :include-docs t))
         (rows (jsown:val response "rows"))
         (row (first rows))
         (doc (jsown:val row "doc")))
    (is (listp rows))
    (is (equal '("a" 2) (jsown:val row "key")))
    (is (eq :null (jsown:val row "value")))
    (is (eq :false (jsown:val doc "false")))
    (is (eq :true (jsown:val doc "true")))
    (is (eq :null (jsown:val doc "null")))
    (is (string= raw (jsown:to-json response)))
    (is (equal '("literal-doc")
               (star.databases.couchdb:map-view-results
                (lambda (key value document)
                  (is (equal '("a" 2) key))
                  (is (eq :null value))
                  (is (eq :false (jsown:val document "false")))
                  "literal-doc")
                (test-view-client) "records" "fixture" "by_key" :include-docs t)))))

(test canonical-nonrecurring-target-recovers-through-view-transport
  (let* ((document
           (jsown:new-js
             ("id" "target:view-literals") ("_id" "target:view-literals")
             ("_rev" "1-view") ("dataset" "view-tests") ("dtype" "target")
             ("schemaVersion" "0.10.1") ("actor" "subfinder")
             ("target" "example.org") ("delay" 1) ("recurring" :false)
             ("options" (jsown:new-js ("false" :false) ("null" :null)
                                      ("array" #()) ("object" (jsown:empty-object))))))
         (raw (jsown:to-json
               (jsown:new-js ("rows" (list (jsown:new-js ("doc" document)))))))
         (quarantines 0)
         (star.databases.couchdb:*couchdb-view-transport*
           (lambda (client request) (declare (ignore client request)) raw)))
    (multiple-value-bind (records invalid)
        (star.actors:load-persisted-target-records
         (test-view-client) "view-tests"
         :quarantine-fn (lambda (&rest args) (declare (ignore args)) (incf quarantines)))
      (is (= 0 invalid quarantines))
      (is (= 1 (length records)))
      (when records
        (is-false (star.actors:target-record-recurring-p (first records)))
        (is (string= (jsown:to-json (jsown:val document "options"))
                     (jsown:to-json (star.actors:target-record-options (first records)))))))))

(test pending-outbox-replay-preserves-view-payload-literals
  (let* ((payload (jsown:new-js ("false" :false) ("null" :null)
                                ("array" #()) ("object" (jsown:empty-object))))
         (entry (jsown:new-js ("event_id" "event:view") ("mutation_id" "mutation:view")
                              ("sequence" 1) ("status" "pending")
                              ("routing_key" "documents.new.note") ("payload" payload)))
         (stored (jsown:new-js ("_id" "outbox:view") ("_rev" "1-view")
                               ("extensions" (jsown:new-js ("_server_outbox" (vector entry))))))
         (raw (jsown:to-json
               (jsown:new-js ("rows" (list (jsown:new-js ("doc" stored)))))))
         (published nil)
         (star.databases.couchdb:*couchdb-view-transport*
           (lambda (client request) (declare (ignore client request)) raw)))
    (star.databases.couchdb:recover-outbox-documents
     (lambda (id) (is (string= "outbox:view" id)) stored)
     (lambda (updated) (setf stored updated))
     (lambda (routing-key replay event-id)
       (is (string= "documents.new.note" routing-key))
       (is (string= "event:view" event-id))
       (setf published (jsown:to-json replay)))
     (star.databases.couchdb::couchdb-pending-outbox-documents
      (test-view-client) "view-tests"))
    (is (string= (jsown:to-json payload) published))
    (is-true (star.databases.couchdb:outbox-entry-published-p
              (first (star.databases.couchdb:document-outbox-entries stored))))))

(test empty-view-rows-remain-an-empty-list
  (let ((star.databases.couchdb:*couchdb-view-transport*
          (lambda (client request)
            (declare (ignore client request)) "{\"rows\":[]}")))
    (is (null (jsown:val (star.databases.couchdb:query-view
                         (test-view-client) "records" "fixture" "by_key")
                        "rows")))))
