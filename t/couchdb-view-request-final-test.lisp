(in-package :star-server-tests)

(in-suite couchdb-view-request-tests)

(test inclusive-end-is-explicit-and-can-be-disabled
  (let ((default-request (build-test-view-request))
        (exclusive-request (build-test-view-request :inclusive-end nil)))
    (is-true (request-uri-contains-p default-request "inclusive_end=true"))
    (is-true (request-uri-contains-p exclusive-request "inclusive_end=false"))))

(test inclusive-end-rejects-non-booleans-before-transport
  (let ((calls 0)
        (star.databases.couchdb:*couchdb-view-transport*
          (lambda (client request)
            (declare (ignore client request))
            (incf calls)
            "{\"rows\":[]}")))
    (signals star.databases.couchdb:view-query-error
      (star.databases.couchdb:query-view
       (test-view-client) "records" "fixture" "by_key"
       :inclusive-end :false))
    (is (zerop calls))))
