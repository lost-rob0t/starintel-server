% Task: reconcile-master (merge github/master f8caa4d into origin/master 8533dc4, base 4721efaf)
% Merge head: 1599100 (branch reconcile/master-2026-09-14). Started at 8533dc4.
% narrative + attribution evidence; machine observations below.

task(reconcile_master).
merge_parents('8533dc40943dc7230b62533458e287a58db527ce', 'f8caa4dfd5c6bc32be1e621a9ed27bcba7339dc5').

% Textual merge clean (merge-tree + ort). Gates at merge head:
%   schema-lock PASS, doc-coverage 851/851 PASS, gen-api-docs no drift
%   (DOC_API_NO_DRIFT via chained git diff --exit-code -- doc/api/).
% Integration suite RED: HTTP-API-TESTS 33/36 (3 failed) - see observations.

% ATTRIBUTION (proves merge introduced no regression):
% Suite run at canonical parent 8533dc4 in worktree /tmp/opencode/wt-canon
% (machine record in that worktree's .prolog/runs/, observe exit 1):
%   VALKEY-LEASE 24/24 PASS, COUCHDB-VIEW 9/9 PASS,
%   HTTP-API-TESTS 33/36 - SAME 3 failures as merge head.
% Root cause (pre-existing at 8533dc4, from PR #11 observability hooks):
%   source/actors.lisp:318 calls (observability-active-p) unqualified in
%   package STAR.ACTORS, which neither uses nor imports it -> runtime
%   "STAR.ACTORS::OBSERVABILITY-ACTIVE-P is undefined" on every ingest
%   publish path (POST /new/document/*, /documents/bulk).
%   actors.lisp is byte-identical at 8533dc4 and merge head.
% FIX: qualify as star:observability-active-p (defun in observability-gserver.lisp:49,
%   package STAR, exported; let-binding is parallel so the properties
%   self-reference is legal and refers to the defun keyarg).

observation('e36178a8831323f2', command(['python', 'scripts/check-starintel-schema-lock.py']), exit(0), '063646fba004fd42710d4de512183cb2a6d8bf2f29f071e435d327e1e61672a3', '1599100ec1c9267d773573fdd9d679d00791106c', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855').
observation('ebdb4f91bf97efc5', command(['nix', 'run', '.#doc-coverage-test']), exit(0), '30708366ec3a7cce44081fb7b2624bd80050364f8c433d0294b9dcecdeddc4af', '1599100ec1c9267d773573fdd9d679d00791106c', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855').
observation('a47f21363c1bc7f6', command(['nix', 'run', '.#gen-api-docs']), exit(0), '8f463cfdce2d219010cf461cbf9987bd174b6df1da4f33316721e0a151949275', '1599100ec1c9267d773573fdd9d679d00791106c', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855').
observation('ce461618f6648445', command(['env', 'COUCHDB_USER=admin', 'COUCHDB_PASSWORD_FILE=/home/unseen/starintel/starintel-server/secrets/couchdb_password', 'nix', 'run', '.#star-integration-tests']), exit(1), 'f66d58fe4c74e74b7683880fb7ade01cec8583c793851eb993215aab496664b2', '1599100ec1c9267d773573fdd9d679d00791106c', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855').
observation('3bcf4ad86946125d', command(['env', 'COUCHDB_USER=admin', 'COUCHDB_PASSWORD_FILE=/home/unseen/starintel/starintel-server/secrets/couchdb_password', 'nix', 'run', '.#star-integration-tests']), exit(1), '19aad819053853c944568d8e9102759f54e77f50f0d75e1e52c91bca4627e19f', '1599100ec1c9267d773573fdd9d679d00791106c', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855').
