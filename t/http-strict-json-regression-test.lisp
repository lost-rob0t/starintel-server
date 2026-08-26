(in-package :star-server-tests)

(in-suite http-boundary-tests)

(test trailing-comma-json-is-a-400-client-error
  "Re-prove the exact malformed-JSON shape observed by security PR #105 on current master."
  (let ((condition
          (capture-http-input-error
           (lambda ()
             (star.frontends.http-api:parse-json-octets
              (babel:string-to-octets
               "{\"_id\":\"security-json-comma\",\"dataset\":\"security-a\",\"dtype\":\"note\",\"schema_version\":\"0.9.0\",}"
               :encoding :utf-8)
              "application/json")))))
    (is-true condition)
    (is (= 400
           (star.frontends.http-api:http-input-error-status condition)))
    (is (string= "malformed_json"
                 (star.frontends.http-api:http-input-error-code condition)))))

(test trailing-comma-json-array-is-a-400-client-error
  "Prove the same permissive-parser defect applies to JSON arrays, not just objects."
  (let ((condition
          (capture-http-input-error
           (lambda ()
             (star.frontends.http-api:parse-json-octets
              (babel:string-to-octets "[1,]" :encoding :utf-8)
              "application/json")))))
    (is-true condition)
    (is (= 400
           (star.frontends.http-api:http-input-error-status condition)))
    (is (string= "malformed_json"
                 (star.frontends.http-api:http-input-error-code condition)))))
