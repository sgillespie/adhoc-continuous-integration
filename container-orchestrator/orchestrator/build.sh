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
    git-builder \
    $GIT_URL

docker run -it \
    --volume "$VOLUME_NAME":/usr/src/app \
    node-builder \
    /bin/sh -c 'npm install && npm run test && npm run build'

docker run -it \
    --volume "$VOLUME_NAME":/usr/src/app \
    --volume /var/run/docker.sock:/var/run/docker.sock \
    --env BUILD_TAG="$BUILD_TAG" \
    --env DOCKERFILE="$DOCKERFILE" \
    docker-builder
