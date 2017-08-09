#!/bin/sh
set -ex

# Create a volume
VOLUME_NAME=orch_build_$(echo ${GIT_URL%.git} | xargs basename)

# Look for existing volumes
VOLUME="$(docker volume ls --format {{.Name}} --filter name="$VOLUME_NAME")"

# Create it only if it doesn't exist
if [[ -z "$VOLUME" ]]; then
    docker volume create "$VOLUME_NAME"
fi

docker run -it \
    --volume "$VOLUME_NAME":/usr/src/app \
    --env GIT_URL=https://github.com/react-boilerplate/react-boilerplate.git \
    git-builder

docker run -it \
    --volume "$VOLUME_NAME":/usr/src/app \
    node-builder

docker run -it \
    --volume "$VOLUME_NAME":/usr/src/app \
    --volume /var/run/docker.sock:/var/run/docker.sock \
    --env BUILD_TAG="$BUILD_TAG" \
    --env DOCKERFILE="$DOCKERFILE" \
    docker-builder
