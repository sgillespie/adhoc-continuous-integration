#!/bin/sh

cp -r /usr/src/app/build .
cp "$DOCKERFILE" ./Dockerfile

exec docker build -t "$BUILD_TAG" .
