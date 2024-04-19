FROM fukamachi/sbcl:latest as build
# Install required packages
RUN --mount=type=cache,target=/var/cache/apt --mount=type=cache,target=/var/lib/apt set -x; \
    apt-get update && \
    apt-get install -y \
    make \
    libffi-dev \
    pkg-config \
    librabbitmq-dev \
    build-essential

# Create a user and set up working directory
RUN ros setup

# Copy dependencies
COPY . /root/

# Set up symbolic links using ln
RUN ln -s /root/deps/ /root/common-lisp
WORKDIR /root/
RUN make build
RUN make install
FROM build as star-router
ENV BUILD_MODE="DEV" \
    COUCHDB_HOST="127.0.0.1" \
    COUCHDB_PORT=5984 \
    COUCHDB_SCHEME="http" \
    COUCHDB_USER="admin" \
    COUCHDB_PASSWORD="password" \
    COUCHDB_DATABASE="starintel" \
    HTTP_API_LISTEN_ADDRESS="localhost" \
    RABBITMQ_HOST="rabbitmq" \
    RABBITMQ_PORT=5672
# Expose port 5000
EXPOSE 5000

# Entrypoint to start the server
ENTRYPOINT ./star-server
CMD [./star-server]
