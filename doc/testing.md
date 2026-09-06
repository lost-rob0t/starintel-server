# Testing

The canonical hermetic test entry point is:

```lisp
(asdf:test-system :starintel-gserver-tests)
```

From the repository, run it through the pinned Nix environment:

```sh
nix run .#star-unit-tests
```

Unit tests do not require CouchDB or RabbitMQ. The runner prints
`discovered`, `executed`, `passed`, `failed`, and `skipped` test counts for
every required suite. A required suite fails when it discovers or executes
zero tests, when any discovered test does not execute, or when a test fails
or skips.

Service-backed HTTP and persistence coverage is a separate ASDF system:

```lisp
(asdf:test-system :starintel-gserver-integration-tests)
```

Run it only after provisioning CouchDB and RabbitMQ:

```sh
nix run .#star-integration-tests
```

CI runs the unit system without services, then provisions CouchDB and
RabbitMQ for the integration system. Dependency startup failures are test
failures; the required integration suite never silently skips them.
