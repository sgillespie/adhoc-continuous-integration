FROM docker

WORKDIR /tmp/build
VOLUME /usr/src/app

ENV BUILD_TAG=docker-build:latest

COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]

RUN mkdir -p /dockerfiles
COPY dockerfiles /dockerfiles
