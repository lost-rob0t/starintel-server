FROM fukamachi/sbcl:latest as build
RUN --mount=type=cache,target=/var/cache/apt --mount=type=cache,target=/var/lib/apt set -x; \
    apt-get update && \
    apt-get install -y \
    make \
    libffi-dev \
    pkg-config \
    librabbitmq-dev \
    build-essential

RUN ros setup

COPY . /root/

RUN ln -s /root/deps/ /root/common-lisp
WORKDIR /root/
RUN make build
RUN make install
FROM build as star-server
EXPOSE 5000

ENTRYPOINT ./star-server
CMD [./star-server]
