#!/usr/bin/env bash
# StarIntel API Test Suite
# Tests all HTTP endpoints using curl
#
# Usage:
#   ./test-api.sh                           # Test localhost:5000
#   STAR_HOST=example.com ./test-api.sh     # Test custom host
#   STAR_PORT=8080 ./test-api.sh            # Test custom port
#
# Requirements:
#   - Running star-server instance
#   - curl command available
#
# Available via nix:
#   nix build .#test-api
#   ./result/bin/test-api

set -e

# Configuration
HOST="${STAR_HOST:-localhost}"
PORT="${STAR_PORT:-5000}"
BASE_URL="http://${HOST}:${PORT}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test counters
PASSED=0
FAILED=0
TOTAL=0

# Helper functions
log_test() {
    echo -e "${BLUE}[TEST]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
    ((PASSED++))
}

log_failure() {
    echo -e "${RED}[FAIL]${NC} $1"
    ((FAILED++))
}

log_info() {
    echo -e "${YELLOW}[INFO]${NC} $1"
}

# Test function that checks response code
test_endpoint() {
    local method="$1"
    local endpoint="$2"
    local expected_code="${3:-200}"
    local data="$4"
    local description="$5"

    ((TOTAL++))
    log_test "$description"

    local url="${BASE_URL}${endpoint}"
    local response
    local http_code

    if [ -z "$data" ]; then
        response=$(curl -s -w "\n%{http_code}" -X "$method" "$url" 2>&1)
    else
        response=$(curl -s -w "\n%{http_code}" -X "$method" \
            -H "Content-Type: application/json" \
            -d "$data" \
            "$url" 2>&1)
    fi

    http_code=$(echo "$response" | tail -n1)
    body=$(echo "$response" | sed '$d')

    if [ "$http_code" -eq "$expected_code" ]; then
        log_success "$method $endpoint → $http_code"
        log_info "Response: $(echo "$body" | head -c 200)"
        echo ""
        return 0
    else
        log_failure "$method $endpoint → $http_code (expected $expected_code)"
        log_info "Response: $(echo "$body" | head -c 200)"
        echo ""
        return 1
    fi
}

# Banner
echo "========================================"
echo "  StarIntel API Test Suite"
echo "========================================"
echo "Target: $BASE_URL"
echo ""

# Health check
log_info "Testing server connectivity..."
if ! curl -s -f "$BASE_URL/health" > /dev/null 2>&1; then
    echo -e "${RED}ERROR: Cannot connect to server at $BASE_URL${NC}"
    echo "Make sure the server is running:"
    echo "  ./result/bin/star-server start --init ./init.lisp"
    exit 1
fi
log_success "Server is reachable"
echo ""

# Test GET endpoints
echo "========================================"
echo "  GET Endpoints"
echo "========================================"

test_endpoint "GET" "/health" 200 "" \
    "Health check endpoint"

test_endpoint "GET" "/" 200 "" \
    "Root endpoint - server info"

test_endpoint "GET" "/targets/test-actor" 200 "" \
    "Get targets by actor"

test_endpoint "GET" "/document/test-doc-id" 404 "" \
    "Get document by ID (expect 404 for non-existent)"

test_endpoint "GET" "/search?q=test&limit=10" 200 "" \
    "Full-text search"

test_endpoint "GET" "/documents/messages/by-user?user=testuser&limit=10" 200 "" \
    "Get messages by user"

test_endpoint "GET" "/documents/messages/by-channel?channel=testchan&group=testgroup&limit=10" 200 "" \
    "Get messages by channel"

test_endpoint "GET" "/documents/messages/by-platform?platform=telegram&limit=10" 200 "" \
    "Get messages by platform"

test_endpoint "GET" "/documents/messages/groups?limit=100" 200 "" \
    "Get message groups"

test_endpoint "GET" "/documents/socialmpost/by-user?user=testuser&limit=10" 200 "" \
    "Get social posts by user"

test_endpoint "GET" "/dataset-size?reduce=true" 200 "" \
    "Get dataset size"

# Test POST endpoints
echo ""
echo "========================================"
echo "  POST Endpoints"
echo "========================================"

# Test target creation
TARGET_JSON='{
  "type": "ipv4",
  "data": "192.168.1.100",
  "scope": "in-scope",
  "dataset": "test-dataset",
  "tags": ["test", "api-test"]
}'

test_endpoint "POST" "/new/target/test-actor" 200 "$TARGET_JSON" \
    "Create new target for actor"

# Test document creation - domain
DOMAIN_JSON='{
  "type": "domain",
  "domain": "example.com",
  "dataset": "test-dataset",
  "tags": ["test", "api-test"]
}'

test_endpoint "POST" "/new/document/domain" 200 "$DOMAIN_JSON" \
    "Create new domain document"

# Test document creation - person
PERSON_JSON='{
  "type": "person",
  "name": "Test User",
  "handle": "testuser",
  "platform": "telegram",
  "dataset": "test-dataset",
  "tags": ["test", "api-test"]
}'

test_endpoint "POST" "/new/document/person" 200 "$PERSON_JSON" \
    "Create new person document"

# Test document creation - message
MESSAGE_JSON='{
  "type": "message",
  "text": "Test message from API test suite",
  "platform": "telegram",
  "channel": "test-channel",
  "group": "test-group",
  "dataset": "test-dataset",
  "tags": ["test", "api-test"]
}'

test_endpoint "POST" "/new/document/message" 200 "$MESSAGE_JSON" \
    "Create new message document"

# Test edge cases
echo ""
echo "========================================"
echo "  Edge Cases & Error Handling"
echo "========================================"

test_endpoint "GET" "/nonexistent" 404 "" \
    "Non-existent endpoint (expect 404)"

test_endpoint "GET" "/search" 500 "" \
    "Search without query parameter (expect error)"

INVALID_JSON='{"invalid json'
test_endpoint "POST" "/new/document/test" 500 "$INVALID_JSON" \
    "Invalid JSON payload (expect error)"

# Summary
echo ""
echo "========================================"
echo "  Test Summary"
echo "========================================"
echo -e "Total:  $TOTAL"
echo -e "${GREEN}Passed: $PASSED${NC}"
if [ $FAILED -gt 0 ]; then
    echo -e "${RED}Failed: $FAILED${NC}"
else
    echo -e "Failed: $FAILED"
fi
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi
