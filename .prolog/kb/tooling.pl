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

% star-unit-tests is SLOW (~10-12 min/run): it boots SBCL and recompiles
% the whole ASDF dependency tree through the live loader before any
% assertion runs; the nix build cache does not help. Never pipe a live
% run through grep - capture full output to a file instead:
%   nix run .#star-unit-tests > /tmp/unit-tests.log 2>&1
% Fast compile-only check: nix build .#starintel-gserver-tests.
cmd(unit_tests_slow_reason, "live ASDF recompile before every suite", "budget ~12 min per run").

% Production ingest host access (verified 2026-10-30): NEVER ssh the
% 10.60.60.x VLAN directly - no route. Reach the starintel guest through
% the Proxmox relay: ssh -J proxmox root@10.60.60.248. Deploy scripts
% accept STARINFRA_SSH_CONFIG with a ProxyJump 'Host starintel' entry.
cmd(ingest_host, "ssh -J proxmox root@10.60.60.248", "production ingest host via Proxmox relay").

% Production CouchDB (verified 2026-10-30): credentials live inside the
% star-server container as COUCHDB_PASSWORD_FILE=/run/secrets/couchdb_password
% (user 'admin'); secrets/couchdb_secret on the host is a different 65-char
% token that does NOT authenticate. Dataset counts use the canonical view:
% GET /starintel/_design/data/_view/dataset_size?group=true (server-owned
% design doc; do not create ad-hoc temp views for this).
cmd(prod_couchdb_count,
    "docker exec starintel-star-server-1 sh -c 'curl -s -u admin:$(cat /run/secrets/couchdb_password) couchdb:5984/starintel/_design/data/_view/dataset_size?group=true'",
    "per-dataset document counts on the ingest host").


% Load sensitivity: the 100-way valkey concurrency test failed ~100% of
% runs on a 32-core machine at load 15-22 and ~1/1 on 4-vCPU CI runners
% also hosting CouchDB/RabbitMQ/Clouseau. Reproduce flakes by running the
% suite under load, not by adding sleeps.