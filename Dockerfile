# Build stage using Nix
FROM nixos/nix:latest as build

# Enable flakes and configure Nix
RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

WORKDIR /build

# Copy flake files first for better caching
COPY flake.nix flake.lock ./

# Copy source code
COPY . .

# Build the star-server binary using nix
RUN nix --extra-experimental-features "nix-command flakes" build .#default

# Extract the built binary
RUN cp -L result/bin/star-server /tmp/star-server


# Runtime stage using Debian slim
FROM debian:bookworm-slim as star-server

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    liblmdb0 \
    libssl3 \
    librabbitmq4 \
    libffi8 \
    libsqlite3-0 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Copy the built binary
COPY --from=build /tmp/star-server /usr/local/bin/star-server

# Copy example configs for fallback
COPY example_configs /root/example_configs

VOLUME /config
EXPOSE 5000

# Create entrypoint script
RUN echo '#!/bin/sh\n\
set -e\n\
# Check if /config/init.lisp exists; if not, use /root/example_configs/init.lisp\n\
INIT_FILE="/config/init.lisp"\n\
if [ ! -f "$INIT_FILE" ]; then\n\
  echo "/config/init.lisp not found. Using default /root/example_configs/init.lisp."\n\
  INIT_FILE="/root/example_configs/init.lisp"\n\
fi\n\
# Execute the server with the determined init file\n\
exec /usr/local/bin/star-server start -i "$INIT_FILE"' > /usr/local/bin/entrypoint.sh \
    && chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
