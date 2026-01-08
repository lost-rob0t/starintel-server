#!/usr/bin/env bash
# Launch script for star-server with auto-reload on code changes

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${GREEN}==> Star Server Auto-Reload Launcher${NC}"

# Check if we're in the right directory
if [ ! -f "flake.nix" ]; then
    echo -e "${RED}Error: flake.nix not found. Are you in the project directory?${NC}"
    exit 1
fi

# Check if init.lisp exists
if [ ! -f "init.lisp" ]; then
    echo -e "${YELLOW}Warning: init.lisp not found. Server will create a default one.${NC}"
fi

echo -e "${GREEN}==> Starting with auto-reload (Ctrl+C to stop)${NC}"
echo -e "${YELLOW}==> Watching: source/**/*.lisp, *.lisp, *.asd${NC}"
echo ""

# Use entr to watch and restart
# The -r flag tells entr to restart the command when files change
# The -c flag clears the screen before each run
while true; do
    (
        find source/ -name '*.lisp' 2>/dev/null
        find . -maxdepth 1 \( -name '*.lisp' -o -name '*.asd' \) 2>/dev/null
    ) |
        nix shell nixpkgs#entr -c entr -rc sh -c "
        echo '${GREEN}==> [$(date +%H:%M:%S)] Loading star-server...${NC}' && \
        nix run .# -- start ./init.lisp    " || {
        EXIT_CODE=$?
        if [ $EXIT_CODE -eq 130 ]; then
            # Ctrl+C was pressed (SIGINT)
            echo -e "\n${YELLOW}==> Shutting down...${NC}"
            exit 0
        fi
        echo -e "${YELLOW}==> Server crashed or exited. Waiting for file changes to restart...${NC}"
        sleep 1
    }
done
