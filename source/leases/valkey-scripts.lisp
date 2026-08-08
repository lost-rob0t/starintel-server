(in-package :star.leases)

(defparameter +valkey-acquire-script+
  "local prior = redis.call('GET', KEYS[3])
if prior then
  local saved = cjson.decode(prior)
  if saved.digest ~= ARGV[1] then return {'idempotency-conflict', '', '-1'} end
  if saved.record then
    local historical = cjson.decode(saved.record)
    if historical.lock_key ~= ARGV[3] then error('lease identity mismatch') end
    local live = redis.call('GET', KEYS[1])
    if live and cjson.decode(live).lock_key ~= ARGV[3] then error('lease identity mismatch') end
    if not live or cjson.decode(live).lease_id ~= historical.lease_id then
      historical.state = 'expired'
      saved.record = cjson.encode(historical)
    end
  end
  return {saved.code, saved.record or '', tostring(redis.call('PTTL', KEYS[1]))}
end
local current = redis.call('GET', KEYS[1])
if current then
  if cjson.decode(current).lock_key ~= ARGV[3] then error('lease identity mismatch') end
  local ttl = redis.call('PTTL', KEYS[1])
  if ttl == -1 then
    -- An active key with no TTL is corrupt/inconsistent state, not an expired
    -- lease. Fail closed: do not delete, do not increment the fencing counter,
    -- and do not replace the existing record. The stable backend error result
    -- is returned so raw Valkey state never crosses the adapter boundary.
    redis.call('SET', KEYS[3], cjson.encode({digest=ARGV[1], code='backend-unavailable'}), 'PX', ARGV[2])
    return {'backend-unavailable', '', '-1'}
  end
  if ttl > 0 then
    redis.call('SET', KEYS[3], cjson.encode({digest=ARGV[1], code='conflict', record=current}), 'PX', ARGV[2])
    return {'conflict', current, tostring(ttl)}
  end
  -- ttl == 0 (expired) or ttl == -2 (key removed concurrently): reclaimable.
  redis.call('DEL', KEYS[1])
end
local clock = redis.call('TIME')
local now = (tonumber(clock[1]) * 1000) + math.floor(tonumber(clock[2]) / 1000)
local ttl = math.min(tonumber(ARGV[8]), tonumber(ARGV[9]))
local token = redis.call('INCR', KEYS[2])
local record = {
  record_version=1, lock_key=ARGV[3], identity=cjson.decode(ARGV[4]),
  lease_id=ARGV[5], owner_principal_id=ARGV[6], owner_client_id=ARGV[7],
  owner_credential_id=ARGV[10], service_instance_id=ARGV[11],
  fencing_token=token, acquired_at=now, renewed_at=now, expires_at=now + ttl,
  ttl_ms=ttl, maximum_lifetime_ms=tonumber(ARGV[9]), execution_id=ARGV[12],
  job_id=ARGV[13], trace_id=ARGV[14], request_id=ARGV[15],
  metadata=cjson.decode(ARGV[16]), state='active'
}
local encoded = cjson.encode(record)
redis.call('SET', KEYS[1], encoded, 'PX', ttl)
redis.call('SET', KEYS[3], cjson.encode({digest=ARGV[1], code='acquired', record=encoded}), 'PX', ARGV[2])
return {'acquired', encoded, tostring(redis.call('PTTL', KEYS[1]))}")

(defparameter +valkey-renew-script+
  "local prior = redis.call('GET', KEYS[2])
if prior then
  local saved = cjson.decode(prior)
  if saved.digest ~= ARGV[1] then return {'idempotency-conflict', ''} end
  if saved.record and cjson.decode(saved.record).lock_key ~= ARGV[3] then error('lease identity mismatch') end
  return {saved.code, saved.record or ''}
end
local encoded = redis.call('GET', KEYS[1])
if not encoded then
  redis.call('SET', KEYS[2], cjson.encode({digest=ARGV[1], code='expired'}), 'PX', ARGV[2])
  return {'expired', ''}
end
local record = cjson.decode(encoded)
if record.lock_key ~= ARGV[3] then error('lease identity mismatch') end
local code = nil
if tonumber(ARGV[7]) ~= tonumber(record.fencing_token) or ARGV[4] ~= record.lease_id then
  code = 'stale-token'
elseif ARGV[5] ~= record.owner_principal_id or ARGV[6] ~= record.service_instance_id then
  code = 'not-owner'
end
if code then
  redis.call('SET', KEYS[2], cjson.encode({digest=ARGV[1], code=code, record=encoded}), 'PX', ARGV[2])
  return {code, encoded}
end
local clock = redis.call('TIME')
local now = (tonumber(clock[1]) * 1000) + math.floor(tonumber(clock[2]) / 1000)
local expires = math.min(now + tonumber(ARGV[8]), tonumber(record.acquired_at) + tonumber(record.maximum_lifetime_ms))
if expires <= now then
  redis.call('DEL', KEYS[1])
  record.state = 'expired'
  encoded = cjson.encode(record)
  redis.call('SET', KEYS[2], cjson.encode({digest=ARGV[1], code='expired', record=encoded}), 'PX', ARGV[2])
  return {'expired', encoded}
end
record.renewed_at = now
record.expires_at = expires
record.ttl_ms = expires - now
record.request_id = ARGV[9]
encoded = cjson.encode(record)
redis.call('SET', KEYS[1], encoded, 'PX', expires - now)
redis.call('SET', KEYS[2], cjson.encode({digest=ARGV[1], code='renewed', record=encoded}), 'PX', ARGV[2])
return {'renewed', encoded}")

(defparameter +valkey-release-script+
  "local prior = redis.call('GET', KEYS[2])
if prior then
  local saved = cjson.decode(prior)
  if saved.digest ~= ARGV[1] then return {'idempotency-conflict', ''} end
  if saved.record and cjson.decode(saved.record).lock_key ~= ARGV[3] then error('lease identity mismatch') end
  return {saved.code, saved.record or ''}
end
local encoded = redis.call('GET', KEYS[1])
if not encoded then
  redis.call('SET', KEYS[2], cjson.encode({digest=ARGV[1], code='expired'}), 'PX', ARGV[2])
  return {'expired', ''}
end
local record = cjson.decode(encoded)
if record.lock_key ~= ARGV[3] then error('lease identity mismatch') end
local code = nil
if tonumber(ARGV[7]) ~= tonumber(record.fencing_token) or ARGV[4] ~= record.lease_id then
  code = 'stale-token'
elseif ARGV[5] ~= record.owner_principal_id or ARGV[6] ~= record.service_instance_id then
  code = 'not-owner'
end
if code then
  redis.call('SET', KEYS[2], cjson.encode({digest=ARGV[1], code=code, record=encoded}), 'PX', ARGV[2])
  return {code, encoded}
end
redis.call('DEL', KEYS[1])
record.state = 'released'
record.request_id = ARGV[8]
encoded = cjson.encode(record)
redis.call('SET', KEYS[2], cjson.encode({digest=ARGV[1], code='released', record=encoded}), 'PX', ARGV[2])
return {'released', encoded}")

(defparameter +valkey-revoke-script+
  "local prior = redis.call('GET', KEYS[2])
if prior then
  local saved = cjson.decode(prior)
  if saved.digest ~= ARGV[1] then return {'idempotency-conflict', ''} end
  if saved.record and cjson.decode(saved.record).lock_key ~= ARGV[3] then error('lease identity mismatch') end
  return {saved.code, saved.record or ''}
end
local encoded = redis.call('GET', KEYS[1])
if not encoded then
  redis.call('SET', KEYS[2], cjson.encode({digest=ARGV[1], code='not-found'}), 'PX', ARGV[2])
  return {'not-found', ''}
end
local record = cjson.decode(encoded)
if record.lock_key ~= ARGV[3] then error('lease identity mismatch') end
if tonumber(ARGV[5]) ~= tonumber(record.fencing_token) or ARGV[4] ~= record.lease_id then
  redis.call('SET', KEYS[2], cjson.encode({digest=ARGV[1], code='stale-token', record=encoded}), 'PX', ARGV[2])
  return {'stale-token', encoded}
end
redis.call('DEL', KEYS[1])
record.state = 'revoked'
record.request_id = ARGV[7]
encoded = cjson.encode(record)
redis.call('SET', KEYS[2], cjson.encode({digest=ARGV[1], code='revoked', record=encoded}), 'PX', ARGV[2])
return {'revoked', encoded}")

(defparameter +valkey-get-script+
  "local encoded = redis.call('GET', KEYS[1])
if not encoded then return {'not-found', '', '-2', '0'} end
if cjson.decode(encoded).lock_key ~= ARGV[1] then error('lease identity mismatch') end
local clock = redis.call('TIME')
local now = (tonumber(clock[1]) * 1000) + math.floor(tonumber(clock[2]) / 1000)
return {'found', encoded, tostring(redis.call('PTTL', KEYS[1])), tostring(now)}")

(defparameter +valkey-fenced-set-script+
  "local encoded = redis.call('GET', KEYS[1])
if not encoded then return 'expired' end
local record = cjson.decode(encoded)
if record.lock_key ~= ARGV[6] then error('lease identity mismatch') end
if tonumber(ARGV[4]) ~= tonumber(record.fencing_token) or ARGV[1] ~= record.lease_id then
  return 'stale-token'
end
if ARGV[2] ~= record.owner_principal_id or ARGV[3] ~= record.service_instance_id then
  return 'not-owner'
end
local clock = redis.call('TIME')
local now = (tonumber(clock[1]) * 1000) + math.floor(tonumber(clock[2]) / 1000)
if now >= tonumber(record.expires_at) then return 'expired' end
redis.call('SET', KEYS[2], ARGV[5])
return 'committed'")
