# External StarIntel actors

This directory is the Docker-first home for normal external StarIntel actors.

- `website/` contains the reusable modern Python website actor runtime.
- `compose.yaml` is the shared actor service plane.
- `compose.seccomp.yaml` applies the pinned Playwright crawler sandbox profile.
- `skills/` contains agent instructions for creating, verifying, and deploying actors.
- `DEPLOY.md` is the operator deployment runbook.

Website adapters are parser plugins, not standalone spider frameworks. The runtime owns actor mailboxes, browser/HTTP acquisition, target transport, bounded crawl scheduling, canonical StarIntel v0.9 output, and sinks. New sites add an adapter and Compose service/profile rather than reimplementing those layers.
