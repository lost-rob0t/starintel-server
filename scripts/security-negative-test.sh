#!/usr/bin/env bash
set -Eeuo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
corpus="${STAR_SECURITY_PAYLOAD_CORPUS:-${repo_root}/security/payloads.json}"
artifact_dir="${STAR_SECURITY_ARTIFACT_DIR:-${repo_root}/zap-artifacts}"
results_file="${artifact_dir}/security-negative-results.jsonl"
server_url="${STAR_SECURITY_SERVER_URL:?STAR_SECURITY_SERVER_URL is required}"
admin_key="${STAR_SECURITY_ADMIN_KEY:?STAR_SECURITY_ADMIN_KEY is required}"
project_name="${COMPOSE_PROJECT_NAME:-}"

if [[ "${STAR_SECURITY_DISPOSABLE:-0}" != "1" || "$project_name" != starintel-zap-* ]]; then
  printf 'security-negative-test refuses to run outside a disposable starintel-zap-* Compose project\n' >&2
  exit 64
fi

command -v jq >/dev/null
command -v python3 >/dev/null
[[ -r "$corpus" ]]

mkdir -p "$artifact_dir"
: > "$results_file"

curl_max_time="$(jq --exit-status --raw-output '.dos.curl_max_time_seconds' "$corpus")"
auth_header="Authorization: Bearer ${admin_key}"
probe_suffix="${GITHUB_RUN_ID:-$$}-${GITHUB_RUN_ATTEMPT:-0}"
allowed_dataset="security-a"
denied_dataset="security-b"
allowed_id="security-couch-a-${probe_suffix}"
denied_id="security-couch-b-${probe_suffix}"
search_term="securitycouchprobe${GITHUB_RUN_ID:-$$}${GITHUB_RUN_ATTEMPT:-0}"
reader_owner="security-couch-reader-${probe_suffix}"

record_result() {
  local category="$1"
  local name="$2"
  local status="$3"
  local result="$4"
  jq -nc \
    --arg category "$category" \
    --arg name "$name" \
    --arg status "$status" \
    --arg result "$result" \
    '{category:$category,name:$name,status:$status,result:$result}' \
    >> "$results_file"
}

request_status() {
  local output="$1"
  shift
  curl \
    --silent --show-error \
    --connect-timeout 3 \
    --max-time "$curl_max_time" \
    --output "$output" \
    --write-out '%{http_code}' \
    "$@"
}

assert_healthy() {
  local body="${artifact_dir}/security-health.json"
  local status
  status="$(request_status "$body" "${server_url}/health")"
  if [[ "$status" != "200" ]]; then
    printf 'security probe health check failed: HTTP %s\n' "$status" >&2
    cat "$body" >&2 || true
    return 1
  fi
  rm -f "$body"
}

submit_document() {
  local id="$1"
  local dataset="$2"
  local content="$3"
  local body
  body="$(
    jq -nc \
      --arg id "$id" \
      --arg dataset "$dataset" \
      --arg content "$content" \
      '{_id:$id,dataset:$dataset,tenant_id:"default",dtype:"note",version:"0.9.0",content:$content}'
  )"
  curl \
    --fail --silent --show-error \
    --connect-timeout 3 \
    --max-time "$curl_max_time" \
    --request POST \
    --header "$auth_header" \
    --header 'Content-Type: application/json' \
    --data "$body" \
    "${server_url}/new/document/note" \
    >/dev/null
}

wait_for_search_id() {
  local bearer="$1"
  local term="$2"
  local id="$3"
  local response="${artifact_dir}/security-search-wait.json"
  local status

  for _ in $(seq 1 60); do
    status="$(
      request_status \
        "$response" \
        --header "Authorization: Bearer ${bearer}" \
        --get \
        --data-urlencode "q=content:${term}" \
        "${server_url}/search"
    )"
    if [[ "$status" == "200" ]] &&
       jq --exit-status --arg id "$id" \
         '.. | objects | select(._id? == $id)' \
         "$response" >/dev/null 2>&1; then
      rm -f "$response"
      return
    fi
    sleep 2
  done

  printf 'security fixture %s did not appear in search\n' "$id" >&2
  cat "$response" >&2 || true
  return 1
}

printf '==> security negative suite: establish scoped CouchDB/Clouseau fixtures\n'
reader_response="${artifact_dir}/security-reader.json"
reader_request="$(
  jq -nc \
    --arg owner "$reader_owner" \
    --arg dataset "$allowed_dataset" \
    '{owner:$owner,principal_type:"api_client",scopes:["search:read","views:read","tenant:default",("dataset:"+$dataset)]}'
)"
reader_status="$(
  request_status \
    "$reader_response" \
    --request POST \
    --header "$auth_header" \
    --header 'Content-Type: application/json' \
    --data "$reader_request" \
    "${server_url}/auth/credentials"
)"
if [[ "$reader_status" != "201" ]]; then
  printf 'failed to create security scoped reader: HTTP %s\n' "$reader_status" >&2
  cat "$reader_response" >&2 || true
  exit 1
fi
reader_key="$(jq --exit-status --raw-output '.api_key' "$reader_response")"
rm -f "$reader_response"

submit_document "$allowed_id" "$allowed_dataset" "$search_term"
submit_document "$denied_id" "$denied_dataset" "$search_term"
wait_for_search_id "$admin_key" "$search_term" "$allowed_id"
wait_for_search_id "$admin_key" "$search_term" "$denied_id"

printf '==> security negative suite: CouchDB/Clouseau query injection probes\n'
while IFS=$'\t' read -r name raw_query; do
  query="${raw_query//\{\{TERM\}\}/$search_term}"
  query="${query//\{\{DENIED_DATASET\}\}/$denied_dataset}"
  body="${artifact_dir}/security-couch-query.json"
  status="$(
    request_status \
      "$body" \
      --header "Authorization: Bearer ${reader_key}" \
      --get \
      --data-urlencode "q=${query}" \
      "${server_url}/search"
  )"

  if [[ "$status" == 2* ]]; then
    if jq --exit-status --arg denied "$denied_id" \
         '([.. | objects | ._id? // empty] | index($denied)) == null' \
         "$body" >/dev/null; then
      record_result couchdb-search "$name" "$status" scoped
    else
      record_result couchdb-search "$name" "$status" leaked-denied-document
      printf 'CouchDB/Clouseau scope escape succeeded for %s\n' "$name" >&2
      cat "$body" >&2 || true
      exit 1
    fi
  elif [[ "$status" == 4* ]]; then
    record_result couchdb-search "$name" "$status" rejected
  else
    record_result couchdb-search "$name" "$status" server-error
    printf 'CouchDB/Clouseau injection probe %s returned HTTP %s\n' "$name" "$status" >&2
    cat "$body" >&2 || true
    exit 1
  fi
  rm -f "$body"
done < <(jq --raw-output '.couchdb_search[] | [.name,.query] | @tsv' "$corpus")

explicit_scope_body="${artifact_dir}/security-explicit-scope.json"
explicit_scope_status="$(
  request_status \
    "$explicit_scope_body" \
    --header "Authorization: Bearer ${reader_key}" \
    --get \
    --data-urlencode "q=content:${search_term}" \
    --data-urlencode "dataset=${denied_dataset}" \
    "${server_url}/search"
)"
if [[ "$explicit_scope_status" != "403" ]]; then
  printf 'explicit cross-dataset search expected 403, got %s\n' "$explicit_scope_status" >&2
  cat "$explicit_scope_body" >&2 || true
  exit 1
fi
record_result couchdb-search explicit-cross-dataset "$explicit_scope_status" rejected
rm -f "$explicit_scope_body"

printf '==> security negative suite: CouchDB view path injection probes\n'
while IFS=$'\t' read -r name design view; do
  body="${artifact_dir}/security-couch-view.json"
  status="$(
    request_status \
      "$body" \
      --path-as-is \
      --header "Authorization: Bearer ${reader_key}" \
      --get \
      --data-urlencode "dataset=${allowed_dataset}" \
      "${server_url}/views/${design}/${view}"
  )"
  if [[ "$status" == 4* ]]; then
    record_result couchdb-view-path "$name" "$status" rejected
  else
    record_result couchdb-view-path "$name" "$status" unexpected
    printf 'CouchDB view path probe %s expected 4xx, got HTTP %s\n' "$name" "$status" >&2
    cat "$body" >&2 || true
    exit 1
  fi
  rm -f "$body"
done < <(jq --raw-output '.couchdb_view_paths[] | [.name,.design,.view] | @tsv' "$corpus")

printf '==> security negative suite: malformed JSON probes\n'
while IFS=$'\t' read -r name raw_body; do
  body="${artifact_dir}/security-json-invalid.json"
  status="$(
    request_status \
      "$body" \
      --request POST \
      --header "$auth_header" \
      --header 'Content-Type: application/json' \
      --data-binary "$raw_body" \
      "${server_url}/new/document/note"
  )"
  if [[ "$status" != "400" ]]; then
    record_result json-invalid "$name" "$status" unexpected
    printf 'malformed JSON probe %s expected 400, got HTTP %s\n' "$name" "$status" >&2
    cat "$body" >&2 || true
    exit 1
  fi
  record_result json-invalid "$name" "$status" rejected
  rm -f "$body"
done < <(jq --raw-output '.json_invalid[] | [.name,.body] | @tsv' "$corpus")

printf '==> security negative suite: duplicate-key JSON observations\n'
while IFS=$'\t' read -r name raw_body; do
  body="${artifact_dir}/security-json-ambiguous.json"
  status="$(
    request_status \
      "$body" \
      --request POST \
      --header "$auth_header" \
      --header 'Content-Type: application/json' \
      --data-binary "$raw_body" \
      "${server_url}/new/document/note"
  )"
  if [[ "$status" == 5* || "$status" == "000" ]]; then
    record_result json-ambiguous "$name" "$status" server-error
    printf 'duplicate-key JSON probe %s destabilized the server: HTTP %s\n' "$name" "$status" >&2
    cat "$body" >&2 || true
    exit 1
  fi
  if [[ "$status" == 2* ]]; then
    printf 'warning: duplicate-key JSON probe %s was accepted (HTTP %s)\n' "$name" "$status" >&2
    record_result json-ambiguous "$name" "$status" accepted-review-recommended
  else
    record_result json-ambiguous "$name" "$status" rejected
  fi
  rm -f "$body"
done < <(jq --raw-output '.json_ambiguous[] | [.name,.body] | @tsv' "$corpus")

printf '==> security negative suite: invalid UTF-8 JSON\n'
invalid_utf8="${artifact_dir}/security-invalid-utf8.bin"
python3 - "$invalid_utf8" <<'PY'
from pathlib import Path
import sys

Path(sys.argv[1]).write_bytes(
    b'{"_id":"security-invalid-utf8","dataset":"security-a","dtype":"note","version":"0.9.0","content":"\xff"}'
)
PY
utf8_body="${artifact_dir}/security-invalid-utf8-response.json"
utf8_status="$(
  request_status \
    "$utf8_body" \
    --request POST \
    --header "$auth_header" \
    --header 'Content-Type: application/json' \
    --data-binary "@${invalid_utf8}" \
    "${server_url}/new/document/note"
)"
if [[ "$utf8_status" != "400" ]]; then
  printf 'invalid UTF-8 JSON expected 400, got %s\n' "$utf8_status" >&2
  cat "$utf8_body" >&2 || true
  exit 1
fi
record_result json-invalid invalid-utf8 "$utf8_status" rejected
rm -f "$invalid_utf8" "$utf8_body"

printf '==> security negative suite: oversized request body\n'
oversized_bytes="$(jq --exit-status --raw-output '.dos.oversized_body_bytes' "$corpus")"
oversized_file="${artifact_dir}/security-oversized.json"
python3 - "$oversized_file" "$oversized_bytes" <<'PY'
from pathlib import Path
import sys

path = Path(sys.argv[1])
target = int(sys.argv[2])
prefix = b'{"_id":"security-oversized","dataset":"security-a","dtype":"note","version":"0.9.0","content":"'
suffix = b'"}'
fill = max(1, target - len(prefix) - len(suffix))
path.write_bytes(prefix + (b'A' * fill) + suffix)
PY
oversized_response="${artifact_dir}/security-oversized-response.json"
oversized_status="$(
  request_status \
    "$oversized_response" \
    --request POST \
    --header "$auth_header" \
    --header 'Content-Type: application/json' \
    --data-binary "@${oversized_file}" \
    "${server_url}/new/document/note"
)"
if [[ "$oversized_status" != "413" ]]; then
  printf 'oversized JSON expected 413, got %s\n' "$oversized_status" >&2
  cat "$oversized_response" >&2 || true
  exit 1
fi
record_result dos oversized-body "$oversized_status" bounded
rm -f "$oversized_file" "$oversized_response"
assert_healthy

printf '==> security negative suite: bounded bulk amplification\n'
bulk_count="$(jq --exit-status --raw-output '.dos.bulk_document_count' "$corpus")"
bulk_file="${artifact_dir}/security-bulk.json"
python3 - "$bulk_file" "$bulk_count" <<'PY'
from pathlib import Path
import json
import sys

path = Path(sys.argv[1])
count = int(sys.argv[2])
docs = [
    {
        "_id": f"security-bulk-{index}",
        "dataset": "security-a",
        "tenant_id": "default",
        "dtype": "note",
        "version": "0.9.0",
    }
    for index in range(count)
]
path.write_text(json.dumps(docs, separators=(",", ":")), encoding="utf-8")
PY
bulk_response="${artifact_dir}/security-bulk-response.json"
bulk_status="$(
  request_status \
    "$bulk_response" \
    --request POST \
    --header "$auth_header" \
    --header 'Content-Type: application/json' \
    --data-binary "@${bulk_file}" \
    "${server_url}/documents/bulk"
)"
if [[ "$bulk_status" != "413" ]]; then
  printf 'bulk amplification probe expected 413, got %s\n' "$bulk_status" >&2
  cat "$bulk_response" >&2 || true
  exit 1
fi
record_result dos bulk-document-limit "$bulk_status" bounded
rm -f "$bulk_file" "$bulk_response"
assert_healthy

printf '==> security negative suite: deep JSON parser resilience\n'
deep_depth="$(jq --exit-status --raw-output '.dos.deep_json_depth' "$corpus")"
deep_file="${artifact_dir}/security-deep.json"
python3 - "$deep_file" "$deep_depth" <<'PY'
from pathlib import Path
import json
import sys

path = Path(sys.argv[1])
depth = int(sys.argv[2])
value = "leaf"
for _ in range(depth):
    value = [value]
body = {
    "_id": "security-deep-json",
    "dataset": "security-a",
    "tenant_id": "default",
    "dtype": "note",
    "version": "0.9.0",
    "content": value,
}
path.write_text(json.dumps(body, separators=(",", ":")), encoding="utf-8")
PY
deep_response="${artifact_dir}/security-deep-response.json"
set +e
deep_status="$(
  request_status \
    "$deep_response" \
    --request POST \
    --header "$auth_header" \
    --header 'Content-Type: application/json' \
    --data-binary "@${deep_file}" \
    "${server_url}/new/document/note"
)"
deep_rc=$?
set -e
if ((deep_rc != 0)) || [[ "$deep_status" == 5* || "$deep_status" == "000" ]]; then
  printf 'deep JSON probe destabilized request handling: curl=%s HTTP=%s\n' "$deep_rc" "$deep_status" >&2
  cat "$deep_response" >&2 || true
  exit 1
fi
record_result dos deep-json "$deep_status" survived
rm -f "$deep_file" "$deep_response"
assert_healthy

printf '==> security negative suite: bounded concurrent parser pressure\n'
concurrency_requests="$(jq --exit-status --raw-output '.dos.concurrency_requests' "$corpus")"
concurrency_parallelism="$(jq --exit-status --raw-output '.dos.concurrency_parallelism' "$corpus")"
concurrency_dir="${artifact_dir}/security-concurrency"
rm -rf "$concurrency_dir"
mkdir -p "$concurrency_dir"

concurrent_probe() {
  local index="$1"
  local status rc
  set +e
  status="$(
    curl \
      --silent --show-error \
      --connect-timeout 3 \
      --max-time "$curl_max_time" \
      --output /dev/null \
      --write-out '%{http_code}' \
      --request POST \
      --header "$auth_header" \
      --header 'Content-Type: application/json' \
      --data-binary '{"_id":' \
      "${server_url}/new/document/note"
  )"
  rc=$?
  set -e
  if ((rc != 0)); then
    status="000"
  fi
  printf '%s\n' "$status" > "${concurrency_dir}/${index}.status"
  return 0
}

for index in $(seq 1 "$concurrency_requests"); do
  concurrent_probe "$index" &
  if ((index % concurrency_parallelism == 0)); then
    wait
  fi
done
wait

for status_file in "$concurrency_dir"/*.status; do
  status="$(cat "$status_file")"
  if [[ "$status" != "400" ]]; then
    printf 'concurrent malformed-JSON probe returned HTTP %s\n' "$status" >&2
    exit 1
  fi
done
record_result dos concurrent-json-parser "400" "${concurrency_requests}-requests-parallel-${concurrency_parallelism}"
rm -rf "$concurrency_dir"
assert_healthy

printf 'Security negative suite passed CouchDB scope, JSON robustness, and bounded DoS probes.\n'