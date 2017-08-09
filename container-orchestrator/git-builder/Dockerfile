FROM debian:stable-slim

RUN apt-get update && \
  apt-get -y install git

WORKDIR /usr/src/app
VOLUME /usr/src/app

CMD [ \
    "/bin/sh", \
    "-c", \
    "git init && git fetch $GIT_URL && git reset --hard $FETCH_HEAD"]
