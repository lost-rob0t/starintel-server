FROM fukamachi/qlot:latest as build

RUN --mount=type=cache,target=/var/cache/apt \
    --mount=type=cache,target=/var/lib/apt \
    set -x; \
    apt-get update && \
    apt-get install -y \
        make \
        libffi-dev \
        pkg-config \
        librabbitmq-dev \
        build-essential

RUN ros setup

COPY . /root/

WORKDIR /root/
RUN qlot install
RUN qlot exec sbcl --non-interactive \
    --load source/starintel-gserver.asd \
    --eval '(ql:quickload :starintel-gserver)' \
    --eval "(sb-ext:save-lisp-and-die \"star-server\" :toplevel 'star::main :executable t)"


FROM build as star-server

VOLUME /config
EXPOSE 5000
RUN echo '#!/bin/sh\n\
set -e\n\
# Check if /config/init.lisp exists; if not, use /root/init.lisp\n\
INIT_FILE="/config/init.lisp"\n\
if [ ! -f "$INIT_FILE" ]; then\n\
  echo "/config/init.lisp not found. Using default /root/example_configs/init.lisp."\n\
  INIT_FILE="/root/example_configs/init.lisp"\n\
fi\n\
# Execute the server with the determined init file\n\
exec /root/star-server start -i "$INIT_FILE"' > /usr/local/bin/entrypoint.sh \
    && chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
