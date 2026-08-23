(in-package :star.leases)

(defparameter +valkey-fenced-commit-script+
  "local existing = redis.call('GET', KEYS[2])
if existing then
  if existing == ARGV[5] then return 'committed' end
  return 'idempotency-conflict'
end
local encoded = redis.call('GET', KEYS[1])
if not encoded then return 'expired' end
local record = cjson.decode(encoded)
if record.lock_key ~= ARGV[6] then error('lease identity mismatch') end
if tonumber(ARGV[4]) ~= tonumber(record.fencing_token) or ARGV[1] ~= record.lease_id then
  return 'stale-token'
end
if ARGV[2] ~= record.owner_principal_id or ARGV[3] ~= record.service_instance_id then
  return 'not-owner'
end
local ttl = redis.call('PTTL', KEYS[1])
if ttl == -1 then return 'backend-unavailable' end
local clock = redis.call('TIME')
local now = (tonumber(clock[1]) * 1000) + math.floor(tonumber(clock[2]) / 1000)
if now >= tonumber(record.expires_at) then return 'expired' end
redis.call('SET', KEYS[2], ARGV[5])
return 'committed'")

(defun valkey-fenced-intent-key (store identity intent-id)
  "Return the immutable same-slot key for one authoritative side-effect intent."
  (unless (and (typep store 'valkey-lease-store)
               (typep identity 'lease-identity)
               (valid-lease-identifier-p intent-id))
    (error "Invalid fenced commit identity"))
  (valkey-key
   store identity
   (format nil "intent:~a" (digest-string intent-id))))

(defun valkey-fenced-commit
    (store identity record intent-id value &key deadline request-id)
  "Atomically validate RECORD as current authority and persist immutable VALUE.

The Valkey EVAL is the commit linearization point: stale/expired holders cannot
create the intent after a successor lease is active. Replaying INTENT-ID with the
same VALUE is idempotent; changing VALUE fails closed."
  (unless (and (typep store 'valkey-lease-store)
               (typep identity 'lease-identity)
               (typep record 'lease-record)
               (valid-lease-identifier-p intent-id)
               (non-empty-string-p value)
               (valid-valkey-operation-p deadline request-id nil))
    (return-from valkey-fenced-commit :invalid-request))
  (let ((intent-key (valkey-fenced-intent-key store identity intent-id)))
    (multiple-value-bind (response failure)
        (valkey-eval
         store deadline +valkey-fenced-commit-script+
         (list (valkey-active-key store identity) intent-key)
         (list (lease-record-lease-id record)
               (lease-record-owner-principal-id record)
               (lease-record-service-instance-id record)
               (lease-record-fencing-token record)
               value
               (canonical-target-lock-key identity)))
      ;; This is a side-effect authority result, not a lease transition. Keep
      ;; it separate from +LEASE-OUTCOME-CODES+ rather than pretending a commit
      ;; is an acquire/renew/release result.
      (if failure failure (or (valkey-code response) :backend-unavailable)))))
