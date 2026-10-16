invariant(target_exact_id_readback,
          'HTTP Target acceptance must persist the canonical Target under its document ID; an acceptance record alone is not readable by the document endpoint.').

invariant(target_retry_document,
          'Equivalent retries must use the original accepted Target document and reject conflicting content at the same document ID.').

method(target_readback_regression,
       'nix run .#star-unit-tests exercises accepted canonical readback, tenant denial, duplicate original-document reuse, and content conflict.').

invariant(one_shot_target_recovery,
          'On startup, a persisted scheduled one-shot Target is rearmed for its remaining delay with the accepted execution ID; a dispatched Target is not rearmed.').

invariant(target_recovery_source,
          'Recovery validates the stored canonical Target against its acceptance, then dispatches the original accepted document so CouchDB revision and tenant stamping do not change the acceptance fingerprint.').
