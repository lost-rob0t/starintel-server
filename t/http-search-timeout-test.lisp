(in-package :star-server-tests)

(def-suite http-search-timeout-tests
  :description "Graceful degradation of the search route under backend timeouts")

(in-suite http-search-timeout-tests)

#+sbcl
(test search-retries-exactly-once-on-io-timeout
  (let ((attempts 0))
    (is (equal :ok
               (star.frontends.http-api::call-couchdb-search
                (lambda ()
                  (incf attempts)
                  (if (= 1 attempts)
                      (error 'sb-sys:io-timeout :direction :input :seconds 1)
                      :ok)))))
    (is (= 2 attempts))))

#+sbcl
(test search-timeout-propagates-after-retry
  (let ((thunk-calls 0))
    (signals sb-sys:io-timeout
      (star.frontends.http-api::call-couchdb-search
       (lambda ()
         (incf thunk-calls)
         (error 'sb-sys:io-timeout :direction :input :seconds 1))))
    (is (= 2 thunk-calls))))

(test search-returns-first-success-without-retry
  (let ((attempts 0))
    (is (equal :ok
               (star.frontends.http-api::call-couchdb-search
                (lambda ()
                  (incf attempts)
                  :ok))))
    (is (= 1 attempts))))
