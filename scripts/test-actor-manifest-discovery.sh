#!/usr/bin/env bash
# Sourced by the per-run isolated stack test, never against a shared deployment.
set_stage "verify-persisted-python-actor-discovery"

registry_json="$(curl --fail --silent --show-error "${server_url}/client-manifest.json")"
create_path="$(jq -er '.operations[] | select(.operation_id == "documents.create") | .path' <<<"$registry_json")"
get_template="$(jq -er '.operations[] | select(.operation_id == "documents.get") | .path' <<<"$registry_json")"
actors_path="$(jq -er '.operations[] | select(.operation_id == "actors.list") | .path' <<<"$registry_json")"
native_catalog="$(curl --fail --silent --show-error --header "$auth_header" "${server_url}${actors_path}")"
manifest_id="actor-manifest:stack-search"
manifest_body='{
  "id":"actor-manifest:stack-search","dataset":"starintel-system",
  "dtype":"actor-manifest","schemaVersion":"0.10.1",
  "actor":"search-stack-fixture","actorVersion":"0.3.0",
  "accepts":["target"],"produces":["url","observation"],
  "capabilities":["native-yacy-json-api"],
  "metadata":{"operatorVisible":true,"endpoint":"must-not-leak"},
  "targetOptions":{"backend":{"description":"Fixture selector","required":false,"schema":{"type":"string"}}}
}'
curl --fail --silent --show-error --header "$auth_header" \
  --header "Content-Type: application/json" --data "$manifest_body" \
  "${server_url}${create_path}" >/dev/null
encoded_manifest_id="$(jq -nr --arg id "$manifest_id" '$id | @uri')"
manifest_path="${get_template//:id/$encoded_manifest_id}"
persisted=false
for _ in $(seq 1 60); do
  if [[ "$(http_status --header "$auth_header" "${server_url}${manifest_path}")" == "200" ]]; then
    persisted=true
    break
  fi
  sleep 1
done
[[ "$persisted" == "true" ]]
catalog="$(curl --fail --silent --show-error --header "$auth_header" "${server_url}${actors_path}")"
jq -e --argjson native "$native_catalog" '
  [.data.actors[] | select(.semantic.name == "search-stack-fixture")] as $found |
  ($found | length) == 1 and
  $found[0].status == "unavailable" and $found[0].ready == false and
  $found[0].options[0].key == "backend" and
  $found[0].options[0].schema.type == "string" and
  .data.count == ($native.data.count + 1)
' <<<"$catalog" >/dev/null
[[ "$catalog" != *must-not-leak* ]]
jq -e --argjson native "$native_catalog" '
  [.data.actors[].resourceUri] as $now |
  all($native.data.actors[]; .resourceUri as $uri | $now | index($uri) != null)
' <<<"$catalog" >/dev/null

private_body="$(jq '.id="actor-manifest:private-search" | ._id=.id |
  .actor="private-search-fixture" | .tenant_id="other-tenant"' <<<"$manifest_body")"
couch_put "actor-manifest:private-search" "$private_body"
catalog="$(curl --fail --silent --show-error --header "$auth_header" "${server_url}${actors_path}")"
jq -e 'all(.data.actors[]; .semantic.name != "private-search-fixture")' <<<"$catalog" >/dev/null

discovery_reader="$(curl --fail --silent --show-error --header "$auth_header" \
  --header "Content-Type: application/json" \
  --data '{"owner":"actor-manifest-reader","principal_type":"api_client","scopes":["actors:read","documents:read","tenant:default","dataset:starintel-system","actor:*"]}' \
  "${server_url}/auth/credentials")"
discovery_key="$(jq -er '.api_key' <<<"$discovery_reader")"
curl --fail --silent --show-error --header "Authorization: Bearer ${discovery_key}" \
  "${server_url}${actors_path}" | jq -e '
  any(.data.actors[]; .semantic.name == "search-stack-fixture") and
  all(.data.actors[]; .semantic.name != "private-search-fixture")' >/dev/null

hintless_reader="$(curl --fail --silent --show-error --header "$auth_header" \
  --header "Content-Type: application/json" \
  --data '{"owner":"native-discovery-reader","principal_type":"api_client","scopes":["actors:read"]}' \
  "${server_url}/auth/credentials")"
hintless_key="$(jq -er '.api_key' <<<"$hintless_reader")"
curl --fail --silent --show-error --header "Authorization: Bearer ${hintless_key}" \
  "${server_url}${actors_path}" | jq -e \
  'all(.data.actors[]; .semantic.name != "search-stack-fixture")' >/dev/null

if [[ -n "${STAR_STACK_SEED_PLAN:-}" ]]; then
  set_stage "verify-reseed-target-options-contract"
  target_path="$(jq -er '.operations[] | select(.operation_id == "targets.create") | .path' <<<"$registry_json")"
  seed_count=0
  while IFS= read -r request; do
    # Exercise the real reviewed requests without scheduling real websites,
    # even if someone later adds a worker to this isolated fixture stack.
    fixture_request="$(jq '.target=("https://example.invalid/" + .idempotency_key)' <<<"$request")"
    receipt="$(curl --fail --silent --show-error --header "$auth_header" \
      --header "Content-Type: application/json" --data "$fixture_request" \
      "${server_url}${target_path}")"
    target_id="$(jq -er '.target_id' <<<"$receipt")"
    encoded_target_id="$(jq -nr --arg id "$target_id" '$id | @uri')"
    target_document_path="${get_template//:id/$encoded_target_id}"
    curl --fail --silent --show-error --header "$auth_header" \
      "${server_url}${target_document_path}" | jq -e --argjson expected "$fixture_request" '
      .options == $expected.options and .recurring == true and .delay == $expected.delay' >/dev/null
    seed_count=$((seed_count + 1))
  done < "$STAR_STACK_SEED_PLAN"
  [[ "$seed_count" == "16" ]]
fi
