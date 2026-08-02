(in-package :star-server-tests)

(in-suite authorization-policy-tests)

(test view-key-decoder-accepts-couchdb-printer-form
  (is (equal '("dataset-1" "tenant-1")
             (star.authorization::decode-view-key
              "[dataset-1 tenant-1]"))))

(test view-key-decoder-accepts-json-array
  (is (equal '("dataset-1" "tenant-1")
             (star.authorization::decode-view-key
              "[\"dataset-1\",\"tenant-1\"]"))))

(test view-key-decoder-accepts-materialized-sequences
  (is (equal '("dataset-1" "tenant-1")
             (star.authorization::decode-view-key
              '("dataset-1" "tenant-1"))))
  (is (equal '("dataset-1" "tenant-1")
             (star.authorization::decode-view-key
              #("dataset-1" "tenant-1")))))
