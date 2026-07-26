(in-package :star-server-tests)

(def-suite empty-required-suite
  :description "Regression fixture for zero-test suite detection")

(def-suite runner-tests
  :description "Tests for the required-suite runner")

(in-suite runner-tests)

(test required-suite-discovery-counts-tests
  (is (= 2 (length (suite-test-names 'runner-tests)))))

(test empty-required-suite-is-a-hard-failure
  (signals error
    (run-required-suite 'empty-required-suite)))
