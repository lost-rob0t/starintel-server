#!/usr/bin/env bash
# StarIntel BBP/Network/Web HTTP API Test Suite
# Tests all BBP, network, and web endpoints using curl
#
# Usage:
#   ./test-http-api.sh                           # Test localhost:5000
#   STAR_HOST=example.com ./test-http-api.sh     # Test custom host
#   STAR_PORT=8080 ./test-http-api.sh            # Test custom port
#
# Requirements:
#   - Running star-server instance with CouchDB
#   - curl command available
#   - jq (optional, for better JSON output)

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
        log_info "Response: $(echo "$body" | head -c 150)"
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
echo "  StarIntel BBP/Network/Web API Tests"
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

# Test Host Endpoints
echo "========================================"
echo "  Host Endpoints"
echo "========================================"

test_endpoint "GET" "/documents/hosts/by-ip?ip=192.168.1.1&limit=10" 200 "" \
    "Get hosts by IP address"

test_endpoint "GET" "/documents/hosts/by-port?port=22&limit=10" 200 "" \
    "Get hosts by port (SSH)"

test_endpoint "GET" "/documents/hosts/by-port?port=443&limit=10" 200 "" \
    "Get hosts by port (HTTPS)"

test_endpoint "GET" "/documents/hosts/by-service?service=ssh&limit=10" 200 "" \
    "Get hosts by service name"

# Test Email Endpoints
echo ""
echo "========================================"
echo "  Email Endpoints"
echo "========================================"

test_endpoint "GET" "/documents/emails/by-email?email=test@example.com&limit=10" 200 "" \
    "Get emails by full address"

test_endpoint "GET" "/documents/emails/by-domain?domain=example.com&limit=10" 200 "" \
    "Get emails by domain"

test_endpoint "GET" "/documents/emails/with-password?limit=10" 200 "" \
    "Get emails with passwords"

# Test Domain Endpoints
echo ""
echo "========================================"
echo "  Domain Endpoints"
echo "========================================"

test_endpoint "GET" "/documents/domains/by-record?record=example.com&limit=10" 200 "" \
    "Get domains by record name"

test_endpoint "GET" "/documents/domains/by-resolved-address?ip=1.2.3.4&limit=10" 200 "" \
    "Get domains by resolved IP (reverse DNS)"

# Test User Endpoints
echo ""
echo "========================================"
echo "  User Endpoints"
echo "========================================"

test_endpoint "GET" "/documents/users/by-name?name=testuser&limit=10" 200 "" \
    "Get users by name"

test_endpoint "GET" "/documents/users/by-platform?platform=github&limit=10" 200 "" \
    "Get users by platform (GitHub)"

test_endpoint "GET" "/documents/users/by-platform?platform=twitter&limit=10" 200 "" \
    "Get users by platform (Twitter)"

# Test Network Endpoints
echo ""
echo "========================================"
echo "  Network Endpoints"
echo "========================================"

test_endpoint "GET" "/documents/networks/by-asn?asn=12345&limit=10" 200 "" \
    "Get networks by ASN"

test_endpoint "GET" "/documents/networks/by-org?org=Example%20Organization&limit=10" 200 "" \
    "Get networks by organization"

# Test URL Endpoints
echo ""
echo "========================================"
echo "  URL Endpoints"
echo "========================================"

test_endpoint "GET" "/documents/urls/by-url?url=https://example.com&limit=10" 200 "" \
    "Get URLs by full URL"

test_endpoint "GET" "/documents/urls/by-domain?domain=example.com&limit=10" 200 "" \
    "Get URLs by domain"

# Test Breach Endpoints
echo ""
echo "========================================"
echo "  Breach Endpoints"
echo "========================================"

test_endpoint "GET" "/documents/breaches/by-size?descending=true&limit=10" 200 "" \
    "Get breaches by size (largest first)"

# Test Document Creation
echo ""
echo "========================================"
echo "  Document Creation (POST)"
echo "========================================"

# Test host creation
HOST_JSON='{
  "_id": "test-host-api-1",
  "dtype": "host",
  "ip": "10.0.0.1",
  "hostname": "testhost.local",
  "os": "Linux",
  "ports": [
    {"port": 22, "name": "ssh"},
    {"port": 80, "name": "http"}
  ],
  "dataset": "api-test"
}'

test_endpoint "POST" "/new/document/host" 200 "$HOST_JSON" \
    "Create new host document"

# Test email creation
EMAIL_JSON='{
  "_id": "test-email-api-1",
  "dtype": "email",
  "user": "testuser",
  "domain": "example.com",
  "password": "testpass123",
  "dataset": "api-test"
}'

test_endpoint "POST" "/new/document/email" 200 "$EMAIL_JSON" \
    "Create new email document"

# Test domain creation
DOMAIN_JSON='{
  "_id": "test-domain-api-1",
  "dtype": "domain",
  "record": "example.com",
  "recordType": "A",
  "resolvedAddresses": ["1.2.3.4", "5.6.7.8"],
  "dataset": "api-test"
}'

test_endpoint "POST" "/new/document/domain" 200 "$DOMAIN_JSON" \
    "Create new domain document"

# Test user creation
USER_JSON='{
  "_id": "test-user-api-1",
  "dtype": "user",
  "name": "testuser",
  "platform": "github",
  "url": "https://github.com/testuser",
  "bio": "Test user bio",
  "dataset": "api-test"
}'

test_endpoint "POST" "/new/document/user" 200 "$USER_JSON" \
    "Create new user document"

# Test network creation
NETWORK_JSON='{
  "_id": "test-network-api-1",
  "dtype": "network",
  "asn": 12345,
  "org": "Test Organization",
  "subnet": "10.0.0.0/8",
  "dataset": "api-test"
}'

test_endpoint "POST" "/new/document/network" 200 "$NETWORK_JSON" \
    "Create new network document"

# Test URL creation
URL_JSON='{
  "_id": "test-url-api-1",
  "dtype": "url",
  "url": "https://example.com/test",
  "path": "/test",
  "content": "Test page content",
  "dataset": "api-test"
}'

test_endpoint "POST" "/new/document/url" 200 "$URL_JSON" \
    "Create new URL document"

# Test breach creation
BREACH_JSON='{
  "_id": "test-breach-api-1",
  "dtype": "breach",
  "url": "https://example.com/breach",
  "description": "Test breach",
  "total": 10000,
  "dataset": "api-test"
}'

test_endpoint "POST" "/new/document/breach" 200 "$BREACH_JSON" \
    "Create new breach document"

# Test email-message creation
EMAIL_MSG_JSON='{
  "_id": "test-email-msg-api-1",
  "dtype": "email-message",
  "from": "sender@example.com",
  "to": "recipient@example.com",
  "subject": "Test Email",
  "body": "Test email body",
  "dataset": "api-test"
}'

test_endpoint "POST" "/new/document/email-message" 200 "$EMAIL_MSG_JSON" \
    "Create new email-message document"

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
