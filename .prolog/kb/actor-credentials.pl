:- multifile invariant/2, method/2.

invariant(actor_credential_identity,
          'Credential providers are registered by actor, account, and credential type in the optional starintel-actor-credentials add-on.').
invariant(actor_credential_authorization,
          'Only service principals with actor-credentials:read and matching actor:, account:, and credential-type: scopes may resolve a credential.').
invariant(actor_credential_placement,
          'Providers live in private operator configuration; target documents and actor manifests contain only an account selector.').
method(actor_credential_gate,
       'Run nix run .#star-unit-tests and nix build .#starintel-actor-credentials --no-link.').
