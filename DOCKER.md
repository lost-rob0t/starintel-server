# Docker Setup Guide

This document describes how to build and run StarIntel-Gserver using Docker.

## Overview

The Docker setup uses a multi-stage build with Nix to ensure reproducible builds:
- **Build stage**: Uses `nixos/nix` to build the server binary via the flake
- **Runtime stage**: Uses `debian:bookworm-slim` with only the necessary runtime libraries

## Building the Docker Image

```bash
# Build the image
docker build -t star-server:latest .

# Or use docker-compose to build
docker-compose build star-server
```

## Running with Docker Compose

The `docker-compose.yml` file sets up the complete stack:
- **CouchDB**: Document database (port 5984)
- **RabbitMQ**: Message queue (ports 5672, 15672)
- **star-server**: The StarIntel API server (port 5000)

### Start all services:

```bash
docker-compose up -d
```

### View logs:

```bash
# All services
docker-compose logs -f

# Specific service
docker-compose logs -f star-server
```

### Stop services:

```bash
docker-compose down
```

## Configuration

### Init File

The server expects an `init.lisp` configuration file. You can:

1. **Mount your own init.lisp** (recommended):
   ```bash
   # Edit docker-compose.yml to point to your config
   volumes:
     - ./my-init.lisp:/config/init.lisp
   ```

2. **Use default config**: If no `/config/init.lisp` is found, the server will use `/root/example_configs/init.lisp`

### Environment Variables

Default credentials (⚠️ **CHANGE IN PRODUCTION**):
- CouchDB: `admin:password`
- RabbitMQ: `guest:guest`

Edit these in `docker-compose.yml` before deploying.

## Network

All services run on the `star` network. Services can communicate using their hostnames:
- `couchdb:5984`
- `rabbitmq:5672`
- `star-server:5000`

## Volumes

- `couchdb_data`: Persists CouchDB data across container restarts

## Health Checks

Verify services are running:

```bash
# CouchDB
curl http://localhost:5984

# RabbitMQ Management
open http://localhost:15672

# Star Server API
curl http://localhost:5000/api/v1/health
```

## Rebuilding After Changes

After modifying the code:

```bash
# Rebuild and restart
docker-compose build star-server
docker-compose up -d star-server
```

## Troubleshooting

### Check container logs:
```bash
docker-compose logs -f star-server
```

### Access container shell:
```bash
docker-compose exec star-server /bin/bash
```

### Verify binary:
```bash
docker-compose exec star-server /usr/local/bin/star-server --help
```

### Remove all containers and volumes:
```bash
docker-compose down -v
```

## Production Considerations

⚠️ **This setup is EXPERIMENTAL - DO NOT EXPOSE TO WEB**

Before production use:
1. Change default passwords in `docker-compose.yml`
2. Configure proper init.lisp with production settings
3. Set up proper volume backups
4. Configure TLS/SSL termination
5. Implement proper monitoring and logging
6. Review security settings for all services
