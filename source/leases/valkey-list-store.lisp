(in-package :star.leases)

(defparameter +valkey-list-validate-script+
  "local encoded = redis.call('GET', KEYS[1])
if not encoded then return '' end
local ttl = redis.call('PTTL', KEYS[1])
if ttl < 0 then return '' end
local ok, record = pcall(cjson.decode, encoded)
if not ok or type(record) ~= 'table' then return '' end
local expires_at = tonumber(record.expires_at)
if not expires_at then return '' end
local clock = redis.call('TIME')
local now = (tonumber(clock[1]) * 1000) + math.floor(tonumber(clock[2]) / 1000)
if now >= expires_at then return '' end
return encoded")

(defun valkey-validated-active-record (store key deadline)
  "Atomically validate KEY as the current active lease on its Valkey node.
The script receives exactly one declared key, so release/reacquire between SCAN
and validation cannot cause an older record to inherit a successor's TTL."
  (multiple-value-bind (response failure)
      (call-valkey-request
       store deadline nil
       (list "EVAL" +valkey-list-validate-script+ 1 key))
    (cond
      (failure (values nil failure))
      ((or (null response)
           (and (stringp response) (zerop (length response))))
       (values nil nil))
      ((not (stringp response))
       (values nil :backend-unavailable))
      (t
       (let ((record
               (handler-case
                   (deserialize-lease-record response)
                 (error () nil))))
         (if (and record
                  (string=
                   key
                   (valkey-active-key store (lease-record-identity record))))
             (values record nil)
             (values nil nil)))))))

(defmethod list-leases
    ((store valkey-lease-store)
     &key owner-principal-id target-id program-id deadline request-id)
  (unless (and (valid-valkey-operation-p deadline request-id nil)
               (valid-lease-filter-p owner-principal-id)
               (valid-lease-component-filter-p target-id)
               (valid-lease-component-filter-p program-id))
    (return-from list-leases (valkey-outcome :invalid-request)))
  (let ((normalized-owner owner-principal-id)
        (normalized-target
          (and target-id
               (normalize-identity-component "target-id" target-id)))
        (normalized-program
          (and program-id
               (normalize-identity-component "program-id" program-id))))
    (handler-case
        (let ((cursor "0")
              (records nil)
              (pattern
                (format nil "~a:*:lease" (valkey-store-key-prefix store))))
          ;; SCAN is node-local. A cluster-aware caller must fan out this scan
          ;; across primaries and merge results. Per-key validation below is
          ;; cluster-safe because the Lua script accesses only declared KEYS[1].
          (loop
            (let ((page
                    (valkey-test-command
                     store deadline "SCAN" cursor "MATCH" pattern "COUNT" 100)))
              (setf cursor (first page))
              (dolist (key (second page))
                (multiple-value-bind (record failure)
                    (valkey-validated-active-record store key deadline)
                  (when failure
                    (return-from list-leases
                      (emit-valkey-hooks
                       store :list request-id (valkey-outcome failure))))
                  (when (and record
                             (valkey-record-matches-p
                              record normalized-owner
                              normalized-target normalized-program))
                    (push record records)))))
            (when (string= cursor "0") (return)))
          (emit-valkey-hooks
           store :list request-id
           (valkey-outcome
            :listed
            :leases
            (sort records #'string< :key #'lease-record-lock-key))))
      (error ()
        (emit-valkey-hooks
         store :list request-id (valkey-outcome :backend-unavailable))))))
