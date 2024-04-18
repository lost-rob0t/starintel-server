# [[file:source.org::*Nix][Nix:2]]
use flake
# When we run make in prod it will gen defaults
# Debug is just for testing
export BUILD_MODE="DEV"
use flake
export COUCHDB_HOST="127.0.0.1"
export COUCHDB_PORT=5984
export COUCHDB_SCHEME="http"
export COUCHDB_USER="admin"
export COUCHDB_PASSWORD="password"
export COUCHDB_DATABASE="starintel"
# Nix:2 ends here
