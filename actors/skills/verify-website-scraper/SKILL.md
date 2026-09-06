# Verify or repair a StarIntel website actor

Use this when a website adapter may have drifted.

1. Inspect the current live site before editing selectors.
2. Compare static HTTP with the Playwright-rendered DOM when JavaScript may matter.
3. Check redirects, structured data, semantic headings, pagination, and representative details.
4. Run the fixture suite first.
5. Run `starintel-web-actor verify <site> --browser auto` in the built Docker image.
6. Prefer semantic parsing/API recovery over chasing generated class names.
7. Keep deterministic IDs and canonical v0.9 provenance.
8. If a site stops publishing a field, remove/leave it empty; never synthesize a replacement.
9. Do not add CAPTCHA solving or anti-bot bypasses. Surface blocks as collection failures.
10. Update the reduced fixture and minimum-live-count gate when legitimate site behavior changes.

A repair is done only when fixture tests, image build, Compose validation, and live smoke all pass.
