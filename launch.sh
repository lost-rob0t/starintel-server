#!/usr/bin/env bash
set -euo pipefail

# Launch one long-lived StarIntel process with in-image live patching enabled.
# Unlike the old entr loop, source changes do not restart SBCL.

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

if [ ! -f "flake.nix" ]; then
    echo -e "${RED}Error: flake.nix not found. Are you in the project directory?${NC}" >&2
    exit 1
fi

if [ ! -f "init.lisp" ]; then
    echo -e "${YELLOW}Warning: init.lisp not found. Server startup may create/use defaults.${NC}"
fi

export STAR_HOT_RELOAD="${STAR_HOT_RELOAD:-true}"
export STAR_HOT_RELOAD_DIRECTORY="${STAR_HOT_RELOAD_DIRECTORY:-$PWD/.star-hot-reload}"
mkdir -p "$STAR_HOT_RELOAD_DIRECTORY"

echo -e "${GREEN}==> Starting StarIntel with live in-image hot reload${NC}"
echo -e "${YELLOW}==> PID will stay stable across patches${NC}"
echo -e "${YELLOW}==> Patch directory: $STAR_HOT_RELOAD_DIRECTORY${NC}"
echo -e "${YELLOW}==> Publish: make reload PATCH=source/file.lisp${NC}"
echo ""

exec nix run .# -- start ./init.lisp
