# Synthetic ingestion acceptance (issue #343, child of #52)

This harness sends one synthetic message through the existing `documents` exchange,
`documents.ingest.message` routing key, and retrieves it using the repository's
`starintel-gserver-client` `documents.get` operation. It does not collect data or
create another persistence/API path. Rabbit publication is persistent, mandatory,
and publisher-confirmed using Pika. Confirmation proves broker acceptance; the
subsequent exact canonical read proves persistence. Only public `rev` metadata is
excluded from equality; unsolicited fields fail.

Prerequisites: the pinned repository server stack and schema lock, existing Rabbit
exchange/bindings, an isolated test dataset, a read credential, and an SBCL ASDF
closure containing `starintel-gserver-client`. Plain host SBCL without that closure
is insufficient. Create a disposable Python environment and install `pika==1.3.2`;
this dependency pin does not replace the repository's Nix dependency lock.

```sh
python3 scripts/check-starintel-schema-lock.py schema/starintel-schema.lock.json
python3 -m venv /tmp/star-readiness-venv
/tmp/star-readiness-venv/bin/pip install pika==1.3.2
/tmp/star-readiness-venv/bin/python scripts/readiness_ingest.py --config /tmp/readiness.json --duplicate
```

Configuration is JSON, containing references only (never credential values):

```json
{
  "run_id": "unique-ci-run",
  "dataset": "readiness",
  "rabbit_host": "127.0.0.1",
  "rabbit_port": 5672,
  "rabbit_user": "starintel",
  "rabbit_password_file": "/tmp/private/rabbit_password",
  "api_url": "http://127.0.0.1:5000",
  "api_key_file": "/tmp/private/api_key",
  "client_command": ["sbcl", "--script"],
  "timeout_seconds": 30
}
```

Credential files must be owner-only. The process reads them at runtime; values are
not command arguments, generated Lisp, result JSON, or diagnostics. Client stdout
and stderr are suppressed. Use a local isolated stack; default transport is plain
loopback HTTP/AMQP, not a remote production transport configuration.

`run_fixture(config: dict, index: int)` is reusable by the load runner. It returns
`id`, `latency_ms`, `canonical_equal`, `publisher_confirmed`, and `revision` only
after a successful read. The latency includes connection setup and supported-client
process startup; this is an end-to-end harness measurement, not isolated server
latency. Each call opens one Rabbit connection and a bounded client subprocess.
Timeout is 1–300 seconds, with broker connection setup allowed a one-second margin.
Use a unique run ID for each experiment; the fixture ID is deterministic within a
run. `fixture_document(run_id,index,dataset)` produces the canonical message.

`--duplicate` republishes the identical mutation and compares the returned revision.
A stable immediately observed revision alone does not prove the second delivery was
settled: the full stack gate must also drain the ingress queue and repeat readback.
No exactly-once publication claim is made. Restart verification should retain the
same dataset/run ID and independently read the original ID after restarting the
server and broker before publishing anything else. Never use another invocation
that republishes first as restart survival proof.

Current helper tests use injected publisher/read callbacks. They prove fixture
identity, equality strictness, and failure handling, not live ingestion, invalid
schema quarantine, durable retry, broker restart or crash safety. Invalid input
requires a separate strict quarantine/settlement oracle; merely failing to observe
a document is insufficient. These service-backed gates remain required and must
not be marked passed from helper tests.
