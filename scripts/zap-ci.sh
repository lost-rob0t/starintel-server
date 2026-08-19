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

printf '==> verifying delegated credential creation cannot escalate to admin\n'
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

escalation_body="${artifact_dir}/delegation-escalation.json"
escalation_status="$(
  curl --silent --show-error \
    --output "$escalation_body" \
    --write-out '%{http_code}' \
    --request POST \
    --header "Authorization: Bearer ${delegator_key}" \
    --header 'Content-Type: application/json' \
    --data '{"owner":"zap-ci-escalated-admin","principal_type":"administrator","scopes":["admin"]}' \
    "${server_url}/auth/credentials"
)"
if [[ "$escalation_status" != "403" ]]; then
  printf 'expected delegated admin issuance to return 403, got %s\n' \
    "$escalation_status" >&2
  cat "$escalation_body" >&2 || true
  exit 1
fi
jq --exit-status -e '.code == "access_denied"' "$escalation_body" >/dev/null
rm -f "$escalation_body"

printf '==> creating disposable login principal for request-body coverage\n'
curl --fail --silent --show-error \
  --request POST \
  --header "Authorization: Bearer ${api_key}" \
  --header 'Content-Type: application/json' \
  --data '{"username":"zap-user","password":"zap-ci-password-123456789","principal_type":"user","scopes":["documents:read","search:read"],"must_change_password":false}' \
  "${server_url}/auth/users" >/dev/null

network_name="${COMPOSE_PROJECT_NAME}_backend"
scan_args=(
  zap-api-scan.py
  -t /zap/wrk/.zap/openapi.yaml
  -f openapi
  -c /zap/wrk/.zap/rules.tsv
  -r zap-artifacts/report.html
  -w zap-artifacts/report.md
  -J zap-artifacts/report.json
  -T 15
)

if [[ "$mode" == passive ]]; then
  # Safe mode imports the OpenAPI endpoints and runs passive checks only.
  # WARNs are retained in reports; selected systemic rules in rules.tsv gate CI.
  scan_args+=(-S -I)
else
  # Active mode runs only against this disposable Compose network and fails on
  # both FAIL and WARN results so scheduled/manual scans demand triage.
  scan_args+=(-a)
fi

printf '==> running ZAP %s API scan with %s\n' "$mode" "$zap_image"
docker run --rm \
  --network "$network_name" \
  --volume "$repo_root:/zap/wrk:rw" \
  --env ZAP_AUTH_HEADER=Authorization \
  --env "ZAP_AUTH_HEADER_VALUE=Bearer ${api_key}" \
  "$zap_image" \
  "${scan_args[@]}"

printf 'ZAP %s scan completed successfully.\n' "$mode"
