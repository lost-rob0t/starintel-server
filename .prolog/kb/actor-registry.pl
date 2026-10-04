% Actor discovery is a public semantic catalog, not a projection of runtime routing.

:- multifile invariant/2, method/2.

invariant(actor_registry_manifest_authority,
          'Only explicitly operator-visible emitted actor/service manifests create public catalog membership.').

invariant(actor_registry_liveness_separation,
          'Runtime observations and Sento bindings can annotate registered resources but cannot create catalog entries.').

invariant(actor_registry_safe_projection,
          'GET /v1/actors exposes semantic identity, contracts, capabilities, provenance, and bounded liveness; it never exposes actor references, queues, routing keys, endpoints, or secrets.').

method(actor_registry_focused_gate,
       'Run nix run .#star-actor-registry-tests for the isolated actor registry, liveness, authorization, and HTTP contract suite.').
