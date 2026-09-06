(in-package :star-server-tests)

(in-suite http-boundary-tests)

(test json-false-and-null-survive-http-boundary-round-trip
  (let* ((payload "{\"handling\":{\"pii\":false},\"missing\":null}")
         (document
           (star.frontends.http-api:parse-json-octets
            (babel:string-to-octets payload :encoding :utf-8)
            "application/json"))
         (handling (jsown:val document "handling"))
         (serialized (jsown:to-json document)))
    (is (eq :false (jsown:val handling "pii")))
    (is (eq :null (jsown:val document "missing")))
    (is (search "\"pii\":false" serialized))
    (is (search "\"missing\":null" serialized))
    (is (null (search "\"pii\":[]" serialized)))))
