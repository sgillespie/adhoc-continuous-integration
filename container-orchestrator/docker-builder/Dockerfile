FROM docker

WORKDIR /tmp/build
VOLUME /usr/src/app /var/run/docker.sock

ENV BUILD_TAG=docker-build:latest

COPY entrypoint.sh /entrypoint.sh

RUN mkdir -p /dockerfiles
COPY dockerfiles /dockerfiles

ENTRYPOINT ["/entrypoint.sh"]
CMD [ \
    "/bin/sh", \
    "-c", \
    "docker build -t $BUILD_TAG ."]
