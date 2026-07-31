#!/usr/bin/env bash
set -Eeuo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
credentials_dir="$(mktemp -d)"
artifact_dir="${repo_root}/stack-test-artifacts"
project_name="starintel-auth-stack-$$"
port_base=$((20000 + $$ % 20000))
stage="initialize"
failure_line=""
failure_status="0"

export CREDENTIALS_DIR="$credentials_dir"
export COMPOSE_PROJECT_NAME="$project_name"
export STAR_SERVER_PORT="$port_base"
export COUCHDB_PORT="$((port_base + 1))"
export RABBITMQ_PORT="$((port_base + 2))"
export RABBITMQ_MANAGEMENT_PORT="$((port_base + 3))"

mkdir -p "$artifact_dir"
rm -f "$artifact_dir"/*

set_stage() {
  stage="$1"
  printf 'stage=%s\nstatus=running\n' "$stage" > "$artifact_dir/status.txt"
  printf '==> %s\n' "$stage"
}

record_error() {
  failure_status="$?"
  failure_line="$1"
  return "$failure_status"
}
trap 'record_error "$LINENO"' ERR

capture_diagnostics() {
  {
    printf 'stage=%s\n' "$stage"
    printf 'status=failed\n'
    printf 'exit_status=%s\n' "$failure_status"
    printf 'line=%s\n' "$failure_line"
  } > "$artifact_dir/status.txt"

  docker compose --project-directory "$repo_root" ps --all \
    > "$artifact_dir/compose-ps.txt" 2>&1 || true
  docker compose --project-directory "$repo_root" logs --no-color star-server \
    > "$artifact_dir/star-server.log" 2>&1 || true
  docker compose --project-directory "$repo_root" logs --no-color couchdb \
    > "$artifact_dir/couchdb.log" 2>&1 || true
}

cleanup() {
  status=$?
  if ((status != 0)); then
    if ((failure_status == 0)); then
      failure_status="$status"
    fi
    capture_diagnostics
    cat "$artifact_dir/status.txt" >&2 || true
    cat "$artifact_dir/compose-ps.txt" >&2 || true
    tail -n 200 "$artifact_dir/star-server.log" >&2 || true
  else
    printf 'stage=complete\nstatus=passed\n' > "$artifact_dir/status.txt"
  fi
  docker compose --project-directory "$repo_root" down \
    --volumes --remove-orphans >/dev/null 2>&1 || true
  rm -rf "$credentials_dir"
  exit "$status"
}
trap cleanup EXIT

couchdb_password="stack-couchdb-$$"
rabbitmq_password="stack-rabbitmq-$$"
auth_pepper="stack-auth-pepper-$$-$(date +%s%N)"
auth_bootstrap_secret="stack-bootstrap-$$-$(date +%s%N)"

set_stage "write-secrets"
printf '%s\n' "$couchdb_password" > "$credentials_dir/couchdb_password"
printf '%s\n' "stack-couchdb-secret-$$" > "$credentials_dir/couchdb_secret"
printf '%s\n' "STACKERLANGCOOKIE$$" > "$credentials_dir/erlang_cookie"
printf '%s\n' "$rabbitmq_password" > "$credentials_dir/rabbitmq_password"
printf '%s\n' "$auth_pepper" > "$credentials_dir/auth_pepper"
printf '%s\n' "$auth_bootstrap_secret" > "$credentials_dir/auth_bootstrap_secret"
chmod 0600 "$credentials_dir"/*

cd "$repo_root"

set_stage "load-images"
nix run .#load-images

set_stage "validate-compose"
docker compose config --quiet

set_stage "start-stack"
docker compose up --detach --wait --wait-timeout 300

fixture_id="auth-stack-fixture"
fixture_term="authstacksearchfixture"
couchdb_url="http://127.0.0.1:${COUCHDB_PORT}"
server_url="http://127.0.0.1:${STAR_SERVER_PORT}"

set_stage "bootstrap-administrator"
bootstrap_response="$(
  curl --fail --silent --show-error \
    --request POST \
    --header "Content-Type: application/json" \
    --header "X-Star-Bootstrap-Secret: ${auth_bootstrap_secret}" \
    --data '{"owner":"stack-administrator"}' \
    "${server_url}/auth/bootstrap"
)"
api_key="$(jq --exit-status --raw-output '.api_key' <<<"$bootstrap_response")"
[[ "$api_key" == star_sk_v1_* ]]
auth_header="Authorization: Bearer ${api_key}"

set_stage "verify-unauthenticated-denial"
unauthenticated_status="$(
  curl --silent --output /dev/null --write-out '%{http_code}' \
    --get --data-urlencode "q=content:${fixture_term}" \
    "${server_url}/search"
)"
[[ "$unauthenticated_status" == "401" ]]

set_stage "verify-one-time-bootstrap"
second_bootstrap_status="$(
  curl --silent --output /dev/null --write-out '%{http_code}' \
    --request POST \
    --header "Content-Type: application/json" \
    --header "X-Star-Bootstrap-Secret: ${auth_bootstrap_secret}" \
    --data '{"owner":"second-administrator"}' \
    "${server_url}/auth/bootstrap"
)"
[[ "$second_bootstrap_status" == "409" ]]

set_stage "verify-authenticated-context"
curl --fail --silent --show-error \
  --header "$auth_header" \
  "${server_url}/auth/context" |
  jq --exit-status \
    '.principal_id == "stack-administrator" and .principal_type == "administrator"' \
    >/dev/null

set_stage "insert-search-fixture"
curl --fail --silent --show-error \
  --user "${COUCHDB_USER:-admin}:${couchdb_password}" \
  --request PUT \
  --header "Content-Type: application/json" \
  --data "{\"dtype\":\"note\",\"content\":\"${fixture_term}\"}" \
  "${couchdb_url}/${COUCHDB_DATABASE:-starintel}/${fixture_id}" >/dev/null

wait_for_search() {
  local response
  for _ in $(seq 1 60); do
    response="$(
      curl --fail --silent --show-error \
        --header "$auth_header" \
        --get --data-urlencode "q=content:${fixture_term}" \
        "${server_url}/search" || true
    )"
    if jq --exit-status --arg id "$fixture_id" \
      '.. | objects | select(._id? == $id)' <<<"$response" >/dev/null 2>&1; then
      return
    fi
    sleep 2
  done
  printf 'fixture did not appear in authenticated full-text search\n' >&2
  return 1
}

wait_for_healthy_stack() {
  local container_id health status
  local -a container_ids

  for _ in $(seq 1 150); do
    mapfile -t container_ids < <(docker compose ps --all --quiet)
    status=1

    if ((${#container_ids[@]} == 4)); then
      status=0
      for container_id in "${container_ids[@]}"; do
        health="$(docker inspect --format '{{if .State.Health}}{{.State.Health.Status}}{{else}}{{.State.Status}}{{end}}' "$container_id")"
        if [[ "$health" != "healthy" ]]; then
          status=1
          break
        fi
      done
    fi

    if ((status == 0)); then
      return
    fi
    sleep 2
  done

  printf 'stack did not recover after restart\n' >&2
  return 1
}

set_stage "verify-authenticated-search"
wait_for_search

set_stage "restart-stack"
docker compose restart

set_stage "wait-after-restart"
wait_for_healthy_stack

set_stage "verify-document-persistence"
curl --fail --silent --show-error \
  --user "${COUCHDB_USER:-admin}:${couchdb_password}" \
  "${couchdb_url}/${COUCHDB_DATABASE:-starintel}/${fixture_id}" |
  jq --exit-status --arg term "$fixture_term" '.content == $term' >/dev/null

set_stage "verify-credential-persistence"
curl --fail --silent --show-error \
  --header "$auth_header" \
  "${server_url}/auth/context" |
  jq --exit-status '.principal_id == "stack-administrator"' >/dev/null

set_stage "verify-search-after-restart"
wait_for_search

printf 'Authenticated Nix-built stack passed bootstrap, denial, FTS, and restart persistence checks.\n'
