# Create a StarIntel website actor

Use this skill when adding a public website collector under `actors/website`.

## Non-negotiable contract

A site implementation is a `SiteAdapter`, not a standalone Scrapy project. It must:

1. research the current live site before writing selectors;
2. prefer stable APIs, JSON-LD, semantic HTML, or embedded state over generated CSS classes;
3. use HTTP first and Playwright Chromium only when JavaScript/rendering is actually needed;
4. emit canonical StarIntel v0.9.0 documents with deterministic IDs and provenance;
5. return discovered URLs to the runtime rather than recursively fetching inside parser code;
6. publish durable output through `documents.ingest.<dtype>` in production;
7. never write CouchDB directly;
8. never invent emails/contact details or other facts the source does not publish;
9. never add CAPTCHA solving, fingerprint evasion, or anti-bot bypass behavior.

## Create the adapter

Start from `templates/site_adapter.py` and place the implementation in `actors/website/src/starintel_web_actor/sites/<site>.py`.

Implement `name`, `hosts`, `default_seeds`, `browser_policy`, `matches(url)`, and `parse(page)`. Register built-in adapters in `starintel_web_actor.default_registry`. Third-party packages should register through the `starintel.website_actors` entry-point group instead of editing the core registry.

## Current-site research gate

Check representative index/list and detail pages. Record canonical URL/redirect behavior, static HTTP content, JS requirements, pagination, structured data, stable selectors, challenge behavior, and a minimum live result count suitable for drift detection. Keep a small fixture representing the current structure under `actors/website/tests/fixtures`.

## Browser rules

Default to `BrowserPolicy.AUTO`. Set a stable `wait_for` selector if rendering needs synchronization. Do not use arbitrary sleeps as the main synchronization strategy.

The Docker runtime pins Playwright and Chromium together. Do not independently upgrade the Python Playwright version without updating `actors/website/Dockerfile`, tests, live smoke workflow, and the deployment seccomp version.

## Docker/Compose registration

Every deployable actor gets a service in `actors/compose.yaml` using the shared `x-web-actor` base. Add an immutable image tag, command `worker <site>`, `STARINTEL_ACTOR_NAME=<site>`, no plaintext secrets, the existing RabbitMQ Docker secret, and the external `starintel_backend` network. Add the same service name to `actors/compose.seccomp.yaml` so browser workers use the Playwright crawler profile in production.

## Test gates

From `actors/website` run `python -m compileall src tests`, `pytest -q`, `ruff check .`, and `mypy src`. Then build and live-verify the exact image:

```bash
cd ..
./scripts/fetch-playwright-seccomp.sh
docker compose -f compose.yaml -f compose.seccomp.yaml build <service>
docker compose -f compose.yaml -f compose.seccomp.yaml run --rm --no-deps <service> \
  verify <site> --browser auto --min-people <minimum>
```

Do not merge a new site whose fixture tests pass but live verification produces empty/drastically reduced results.
