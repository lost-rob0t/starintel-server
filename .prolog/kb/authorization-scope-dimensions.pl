% Authorization scope dimensions and the creator/owner dimension (GitHub #401,
% verified by t/authorization-policy-test.lisp owner-dimension tests).
:- multifile invariant/2, method/2, fact/2.

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
