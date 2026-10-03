% Tooling and verification commands verified by machine evidence.

% Local integration test runs. nix run .#star-integration-tests spawns its
% OWN valkey (plain+TLS) on dynamic free ports with password
% valkey-integration-secret (flake.nix ~400-560), so concurrent suite runs
% are safe and the docker staging valkey is NOT used by tests. CouchDB,
% however, is NOT provisioned by the runner: it expects COUCHDB_USER /
% COUCHDB_PASSWORD_FILE (source/gserver-settings.lisp). CI injects
% admin/password; locally point it at the staging secrets file.
cmd(integration_tests_local,
    "COUCHDB_USER=admin COUCHDB_PASSWORD_FILE=$PWD/secrets/couchdb_password nix run .#star-integration-tests",
    "full required suites: valkey-lease, couchdb-view, http-api (~3-4 min)").

cmd(unit_tests, "nix run .#star-unit-tests", "all unit suites").
cmd(python_contract, "python3 -m unittest discover -s tests -p test_*.py", "operational salvage + v09 runtime contract").
cmd(schema_lock, "python3 scripts/check-starintel-schema-lock.py", "canonical schema pin verification (needs network)").
cmd(api_docs, "make docs-api", "regenerate doc/api/*.org; CI doc-coverage fails if stale").

% Local CouchDB staging password: docker compose stack 'starintel' from
% ./docker-compose.yml; password in ./secrets/couchdb_password (NOT
% admin/password - that is CI-only).

% Load sensitivity: the 100-way valkey concurrency test failed ~100% of
% runs on a 32-core machine at load 15-22 and ~1/1 on 4-vCPU CI runners
% also hosting CouchDB/RabbitMQ/Clouseau. Reproduce flakes by running the
% suite under load, not by adding sleeps.