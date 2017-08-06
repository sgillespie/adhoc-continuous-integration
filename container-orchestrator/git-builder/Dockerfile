FROM debian:stable-slim

RUN apt-get update && \
  apt-get -y install git

WORKDIR /usr/src/app
VOLUME /usr/src/app

COPY entrypoint.sh /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
