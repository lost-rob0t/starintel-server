# Synthetic load evidence (#343 / #52)

`scripts/readiness_load.py` exercises `readiness_ingest.run_fixture`: persistent,
confirmed RabbitMQ publication followed by supported-client retrieval and canonical
comparison. It targets an already running, isolated local Compose project. It does
not create or destroy the stack, rotate credentials, or contact a collector.

Supply the same private JSON configuration accepted by `readiness_ingest.py`:
`run_id` (unique per run), `dataset`, `rabbit_host`, `rabbit_port`, `rabbit_user`,
`rabbit_password_file`, `api_url`, `api_key_file`, `client_command`, and
`timeout_seconds`. Passwords and API keys belong in permission-restricted files,
never literal JSON values, command arguments, tracked files, or Nix expressions.
Use the environment/client command prepared for the ingestion smoke test. The
RabbitMQ virtual host must be `/`, which is the queue sampler's current scope.

From the checkout with those dependencies and an already running project:

```sh
python3 scripts/readiness_load.py --config /tmp/readiness/config.json \
  --project readiness-test --count 100 --concurrency 2 --duration 60 \
  --sample-interval 5 --output /tmp/readiness/smoke.json

python3 scripts/readiness_load.py --config /tmp/readiness/soak-config.json \
  --project readiness-test --count 1000000 --concurrency 8 --duration 7200 \
  --sample-interval 5 --output /tmp/readiness/soak.json
```

Each invocation needs a fresh `run_id` and output path. Output is created with mode
0600 and fails if the destination exists. No configuration or exception messages
are printed or copied to the report. Nonzero exit means fixture verification,
sampling, setup, or report writing failed; inspect the sanitized error types and
run the existing stack diagnostics without exposing secrets.

Count and duration are upper bounds on submissions, not promises of throughput.
Concurrency is closed-loop: each worker waits for complete publication/retrieval
verification before starting another fixture. At duration expiry, outstanding
fixtures drain within their configured timeout. Sampling commands also have
15-second timeouts. The driver does not replace an external process deadline.

The JSON report records exact source HEAD and dirty state, machine resources,
settings, completed/submitted counts, individual completion latencies, nearest-rank
p95, failures, memory samples for server/CouchDB/RabbitMQ, and RabbitMQ ready and
unacknowledged queue counts. Samples include initial and final observations;
sampler errors fail the result. Missing or unverified completions fail it too.

A run with fewer than 3600 seconds of actual submission activity is labelled
`smoke`, even if a longer duration was requested. `sustained-load` only describes
elapsed workload activity; it does not establish acceptable memory growth,
bounded backlog, a latency SLO, or production readiness. Inspect time series and
compare with explicitly chosen release requirements. A count limit hit early is
reported. Failed/time-out operations are reported separately from successful
latency percentiles, and must never be omitted from capacity conclusions.

Report only the tested concurrency, payload, duration, machine and achieved rate.
Docker memory accounting is not process RSS and interval samples can miss peaks.
The workload includes supported-client process startup, so p95 is end-to-end
fixture completion, not HTTP request latency. No sustained-load result has been
established by the deterministic unit tests:

```sh
python3 -m unittest discover -s tests -p test_readiness_load.py
```
