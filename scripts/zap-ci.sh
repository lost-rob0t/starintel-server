#!/usr/bin/env bash
set -Eeuo pipefail

mode="${1:-passive}"
case "$mode" in
  passive|active) ;;
  *)
    printf 'usage: %s [passive|active]\n' "$0" >&2
    exit 64
    ;;
esac

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
credentials_dir="$(mktemp -d)"
artifact_dir="${repo_root}/zap-artifacts"
run_suffix="${GITHUB_RUN_ID:-$$}-${GITHUB_RUN_ATTEMPT:-0}"
project_name="starintel-zap-${run_suffix}"
port_base=$((22000 + $$ % 20000))
zap_image="${ZAP_IMAGE:-ghcr.io/zaproxy/zaproxy:2.17.0}"

export CREDENTIALS_DIR="$credentials_dir"
export COMPOSE_PROJECT_NAME="$project_name"
export STAR_SERVER_PORT="$port_base"
export COUCHDB_PORT="$((port_base + 1))"
export RABBITMQ_PORT="$((port_base + 2))"
export RABBITMQ_MANAGEMENT_PORT="$((port_base + 3))"
export VALKEY_PORT="$((port_base + 4))"
export STAR_AUTH_PASSWORD_ITERATIONS="${STAR_AUTH_PASSWORD_ITERATIONS:-10000}"

mkdir -p "$artifact_dir"
rm -f "$artifact_dir"/*
chmod 0777 "$artifact_dir"

cleanup() {
  status=$?
  docker compose --project-directory "$repo_root" ps --all \
    > "$artifact_dir/compose-ps.txt" 2>&1 || true
  docker compose --project-directory "$repo_root" logs --no-color star-server \
    > "$artifact_dir/star-server.log" 2>&1 || true
  docker compose --project-directory "$repo_root" down \
    --volumes --remove-orphans >/dev/null 2>&1 || true
  rm -rf "$credentials_dir"
  exit "$status"
}
trap cleanup EXIT

write_secret() {
  local name="$1"
  local value="$2"
  printf '%s\n' "$value" > "$credentials_dir/$name"
  chmod 0600 "$credentials_dir/$name"
}

expect_access_denied() {
  local label="$1"
  local bearer="$2"
  local payload="$3"
  local response_body="${artifact_dir}/security-regression.json"
  local status

  status="$(
    curl --silent --show-error \
      --output "$response_body" \
      --write-out '%{http_code}' \
      --request POST \
      --header "Authorization: Bearer ${bearer}" \
      --header 'Content-Type: application/json' \
      --data "$payload" \
      "${server_url}/auth/credentials"
  )"

  if [[ "$status" != "403" ]]; then
    printf '%s: expected 403, got %s\n' "$label" "$status" >&2
    cat "$response_body" >&2 || true
    return 1
  fi
  jq --exit-status -e '.code == "access_denied"' "$response_body" >/dev/null
  rm -f "$response_body"
}

write_secret couchdb_password "zap-couchdb-${run_suffix}"
write_secret couchdb_secret "zap-couchdb-secret-${run_suffix}"
write_secret erlang_cookie "ZAPERLANGCOOKIE${GITHUB_RUN_ID:-$$}"
write_secret rabbitmq_password "zap-rabbitmq-${run_suffix}"
write_secret valkey_password "zap-valkey-${run_suffix}"
write_secret auth_pepper "zap-auth-pepper-${run_suffix}-$(date +%s%N)"
auth_bootstrap_secret="zap-bootstrap-${run_suffix}-$(date +%s%N)"
write_secret auth_bootstrap_secret "$auth_bootstrap_secret"

cd "$repo_root"

printf '==> loading Nix-built images\n'
nix run .#load-images

printf '==> validating compose configuration\n'
docker compose config --quiet

printf '==> starting disposable StarIntel stack\n'
docker compose up --detach --wait --wait-timeout 300

server_url="http://127.0.0.1:${STAR_SERVER_PORT}"

printf '==> bootstrapping ephemeral administrator credential\n'
bootstrap_response="$(
  curl --fail --silent --show-error \
    --request POST \
    --header 'Content-Type: application/json' \
    --header "X-Star-Bootstrap-Secret: ${auth_bootstrap_secret}" \
    --data '{"owner":"zap-ci-administrator"}' \
    "${server_url}/auth/bootstrap"
)"
api_key="$(jq --exit-status --raw-output '.api_key' <<<"$bootstrap_response")"
[[ "$api_key" == star_sk_v1_* ]]

printf '==> creating delegated credential-management fixture\n'
delegator_response="$(
  curl --fail --silent --show-error \
    --request POST \
    --header "Authorization: Bearer ${api_key}" \
    --header 'Content-Type: application/json' \
    --data '{"owner":"zap-ci-delegator","principal_type":"api_client","scopes":["credentials:create"]}' \
    "${server_url}/auth/credentials"
)"
delegator_key="$(jq --exit-status --raw-output '.api_key' <<<"$delegator_response")"
[[ "$delegator_key" == star_sk_v1_* ]]

printf '==> verifying delegated credential creation cannot escalate authority\n'
expect_access_denied \
  "delegated admin scope escalation" \
  "$delegator_key" \
  '{"owner":"zap-ci-delegator","principal_type":"api_client","scopes":["admin"]}'

printf '==> verifying delegated credential creation cannot impersonate another principal\n'
expect_access_denied \
  "delegated owner impersonation" \
  "$delegator_key" \
  '{"owner":"zap-ci-other-principal","principal_type":"api_client","scopes":["credentials:create"]}'

printf '==> verifying delegated credential creation cannot change principal class\n'
expect_access_denied \
  "delegated principal type mutation" \
  "$delegator_key" \
  '{"owner":"zap-ci-delegator","principal_type":"actor_component","scopes":["credentials:create"]}'

printf '==> creating disposable login principal for request-body coverage\n'
curl --fail --silent --show-error \
  --request POST \
  --header "Authorization: Bearer ${api_key}" \
  --header 'Content-Type: application/json' \
  --data '{"username":"zap-user","password":"zap-ci-password-123456789","principal_type":"user","scopes":["documents:read","search:read"],"must_change_password":false}' \
  "${server_url}/auth/users" >/dev/null

network_name="${COMPOSE_PROJECT_NAME}_backend"
export ZAP_TARGET_URL="http://star-server:5000"
export ZAP_TARGET_REGEX='http://star-server:5000.*'
export ZAP_API_KEY="$api_key"
export ZAP_REPORT_DIR="/zap/wrk/zap-artifacts"

if [[ "$mode" == passive ]]; then
  export ZAP_ACTIVE_SCAN=false
  export ZAP_WARN_LEVEL=High
  export ZAP_WARN_EXIT_VALUE=0
else
  export ZAP_ACTIVE_SCAN=true
  export ZAP_WARN_LEVEL=Medium
  export ZAP_WARN_EXIT_VALUE=2
fi

printf '==> clearing Common Lisp RCE marker inside disposable server\n'
docker compose exec -T star-server sh -lc \
  'rm -f /tmp/starintel-zap-rce-canary'

printf '==> validating ZAP Automation Framework plan\n'
docker run --rm \
  --network "$network_name" \
  --volume "$repo_root:/zap/wrk:rw" \
  --env ZAP_TARGET_URL \
  --env ZAP_TARGET_REGEX \
  --env ZAP_API_KEY \
  --env ZAP_REPORT_DIR \
  --env ZAP_ACTIVE_SCAN \
  --env ZAP_WARN_LEVEL \
  --env ZAP_WARN_EXIT_VALUE \
  "$zap_image" \
  zap.sh -cmd -autocheck /zap/wrk/.zap/automation.yaml

printf '==> running ZAP %s Automation Framework plan with %s\n' "$mode" "$zap_image"
set +e
docker run --rm \
  --network "$network_name" \
  --volume "$repo_root:/zap/wrk:rw" \
  --env ZAP_TARGET_URL \
  --env ZAP_TARGET_REGEX \
  --env ZAP_API_KEY \
  --env ZAP_REPORT_DIR \
  --env ZAP_ACTIVE_SCAN \
  --env ZAP_WARN_LEVEL \
  --env ZAP_WARN_EXIT_VALUE \
  "$zap_image" \
  zap.sh -cmd -autorun /zap/wrk/.zap/automation.yaml
zap_status=$?
set -e

printf '==> checking Common Lisp RCE canary\n'
for _ in $(seq 1 10); do
  if docker compose exec -T star-server sh -lc \
       'test -e /tmp/starintel-zap-rce-canary'; then
    docker compose exec -T star-server sh -lc \
      'cat /tmp/starintel-zap-rce-canary' \
      > "$artifact_dir/rce-canary.txt" 2>&1 || true
    printf 'RCE CANARY TRIPPED: a Lisp injection payload executed in star-server\n' >&2
    cat "$artifact_dir/rce-canary.txt" >&2 || true
    exit 1
  fi
  sleep 1
done

if ((zap_status != 0)); then
  printf 'ZAP Automation Framework exited with status %s\n' "$zap_status" >&2
  exit "$zap_status"
fi

printf 'ZAP %s automation scan and Lisp RCE canaries completed successfully.\n' "$mode"
