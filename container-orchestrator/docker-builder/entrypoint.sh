#!/bin/sh
cp -r /usr/src/app/build .
cp "$DOCKERFILE" ./Dockerfile

exec "$@"
