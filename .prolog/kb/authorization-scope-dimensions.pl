% Authorization scope dimensions and the creator/owner dimension (GitHub #401,
% verified by t/authorization-policy-test.lisp owner-dimension tests).
% Also carries the HTTP route-action mapping protocol (GitHub #386).
:- multifile invariant/2, method/2, fact/2.

invariant(route_action_mapping_protocol,
          'star.frontends.http-api::route-action is the single middleware route->action authority: exact-path auth admin routes map to capability strings (GET/POST /auth/users and POST /auth/users/:username/password -> principals:manage), keyword markers :public and :authenticated (POST /auth/password) are boundary classes not capabilities, and any unmapped method/path falls to nil -> authorize! "unmapped:http-route" which the closed +capabilities+ vocabulary rejects for EVERY principal including admin scope (decision-reason checks unknown_action before capability-granted-p).').
invariant(authenticated_marker_fail_closed,
          'The :authenticated marker in authorize-http-route! admits only when star.auth:current-request-principal is bound; anonymous requests go through authorize! "unauthenticated:http-route" (not in +capabilities+, ungrantable) so the deny stays on the audited default-deny path. The handler keeps the real proof: change-user-password re-verifies the current password.').
invariant(admin_scope_no_unknown_action_bypass,
          'administrator-scopes-p ("admin") bypasses capability-granted-p ONLY; it never legitimizes an action outside +capabilities+. Adding a mounted route therefore REQUIRES an explicit route-action clause or admins are locked out with it — exactly the #386 regression class.').

invariant(owner_dimension_digest_keyed,
          'The creator/owner match is keyed on the extensions.submitted_by SHA-256 digest stamped at target creation, never a raw principal id; research authority STAR-SERVER-059/STAR-RESEARCH-059 forbids raw principal identity in target extensions.').
invariant(owner_dimension_target_only,
          'targets:own is a modifier capability, not an action: it waives ONLY the target: resource dimension when digest(request-principal-id) == creator-digest. The action capability and tenant/dataset/actor/account/credential-type/namespace/program dimensions still apply.').
invariant(owner_dimension_legacy_fallback,
          'Targets without extensions.submitted_by (legacy, policy-emitted, or /new/target path) keep scope-only semantics; the owner match simply fails closed.').
invariant(creator_digest_cross_layer,
          'star.authorization:principal-creator-digest must stay byte-identical to star.frontends.http-api::target-v1-digest; owner-digest-matches-target-v1-submitted-by-stamp pins the equality and resource-from-document wires extensions.submitted_by into the resource creator-digest slot.').
method(create_path_target_dimension,
       'The v1 create path authorizes an in-flight document whose id key is "id" (no "_id"), so resource-from-document target-id is NIL there and the target: dimension is trivially satisfied; the owner dimension matters for PERSISTED targets (fetch/update/delete/views) and for restrict-only targets.list rescues where the caller holds other target: scopes.').
