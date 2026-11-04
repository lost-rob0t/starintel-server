#!/usr/bin/env bash
set -euo pipefail

version="${PLAYWRIGHT_VERSION:-1.62.0}"
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
url="https://raw.githubusercontent.com/microsoft/playwright/v${version}/utils/docker/seccomp_profile.json"

curl --fail --silent --show-error --location "$url" --output "$root/seccomp_profile.json"
python3 -m json.tool "$root/seccomp_profile.json" >/dev/null
printf 'Installed Playwright %s seccomp profile at %s\n' "$version" "$root/seccomp_profile.json"
