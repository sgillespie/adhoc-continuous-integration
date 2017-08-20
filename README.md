# Adhoc Continuous Integration
> Building a CI tool from the ground up with Docker

-   [Introduction](#introduction)
-   [What is Continuous Integration?](#what-is-continuous-integration)
-   [Prerequisites](#prerequisites)
-   [Using Docker Build](#using-docker-build)
-   [Orchestrating a Build Across Multiple
    Containers](#orchestrating-a-build-across-multiple-containers)
-   [Next Steps](#next-steps)
-   [References](#references)

Introduction
------------

There are several open source Continuous Integration (CI) tools
available. Unfortunately, when choosing a CI tool, one must make
compromises. I believe that CI software should be

-   Open Source
-   Easy to Install
-   Easy to Scale
-   Usable in the Cloud or on Bare Metal

Fortunately, with the emergence of Docker, we can easily build such a
tool. I present this system in the following sections.

What is Continuous Integration?
-------------------------------

Continuous Integration is more than a piece of software, it’s a
practice. While the focus of this paper is on the software, I briefly
discuss these two topics.

### The Practice

Continuous Integration is the practice of frequently integrating changes
into a shared mainline.[@fowler2006] With modern Version Control Systems
(VCS), this means that developers are merging their commits into
`master` or a shared `develop` branch.

Along with this practice, Continuous Integration also has the following
guidelines:[@fowler2006]

-   Automate the build
-   Make the build self sesting
-   Every commit should build on an integration machine

Continuous Integration software attempts to automate these.

### Continuous Integration Software

The role of CI software is build the project after each commit. The
benefit is that developers are notified of failures as soon as possible.
The sooner a bug is identified, the less expensive it is to
fix.[@sankey2006]

A CI job run generally looks like this:[@engineyard2012]

1.  Check out a clean copy of the repository
2.  Build the code
3.  Run the test suite

Prerequisites
-------------

Our CI system will be built with Docker, Docker Swarm, and Docker
Compose. We use newer features, so our dependencies will be

-   Docker Engine &gt;= 17.05
-   Docker Compose &gt;= 1.10

A basic understanding of Docker is assumed.

Using Docker Build
------------------

Initially, our CI job use the following process:

1.  Check out a clean copy of the repository
2.  Build and test the code
3.  Build a Docker image

The output will be an image that can run our code. We can achieve this
by using docker build, with the new multi-stage feature.

### Multi-stage Builds

Docker 17.05+ contains a new feature called multi-stage builds. With
multi-stage builds, we build multiple containers, called stages, in a
single Dockerfile. We can selectively copy artifacts from one stage to
another. Each stage begins with a `FROM` instruction, and the last stage
results in the output image.[@docker-multistage]

Here is an example from the Docker [User
Guide](https://docs.docker.com/engine/userguide/eng-image/multistage-build/#use-multi-stage-builds):

    FROM golang:1.7.3
    WORKDIR /go/src/github.com/alexellis/href-counter/
    RUN go get -d -v golang.org/x/net/html  
    COPY app.go .
    RUN CGO_ENABLED=0 GOOS=linux go build -a -installsuffix cgo -o app .

    FROM alpine:latest  
    RUN apk --no-cache add ca-certificates
    WORKDIR /root/
    COPY --from=0 /go/src/github.com/alexellis/href-counter/app .
    CMD ["./app"]

### Using Multi-stage Builds

We now create a Node.js builder. The output will be an nginx image,
serving only static content built from the following commands:

    npm install
    npm test
    npm run build

We assume `npm run build` will generate artifacts in `build/`.

First, we check out a clean copy of the repository:

    FROM node:slim as git-builder
    ARG GIT_URL
    RUN apt-get update && \
    apt-get -y install git
    WORKDIR /usr/src/app
    RUN git clone $GIT_URL .

We define a build argument `GIT_URL`, allowing us to pass the git clone
URL of the project we want to build

We now define a second stage to build the project:

    FROM node:slim as node-builder
    COPY --from=git-builder /usr/src/app /usr/src/app
    WORKDIR /usr/src/app
    RUN npm install && \
    npm run test && \
    npm run build

The argument `--from=git-builder` in the `COPY` instruction copies the
source tree from the first stage to the second.

We define a final stage to create the output image:

    FROM nginx:alpine
    COPY --from=node-builder /usr/src/app/build /usr/share/nginx/html

Combining these stages into a single Dockerfile, we have

    FROM node:slim as git-builder
    ARG GIT_URL
    RUN apt-get update && \
        apt-get -y install git
    WORKDIR /usr/src/app
    RUN git clone $GIT_URL .

    FROM node:slim as node-builder
    COPY --from=git-builder /usr/src/app /usr/src/app
    WORKDIR /usr/src/app
    RUN npm install && \
        npm run test && \
        npm run build

    FROM nginx:alpine
    COPY --from=node-builder /usr/src/app/build /usr/share/nginx/html

We can now execute the build using the `docker build` command. In this
example we will build [React
Boilerplate](https://www.reactboilerplate.com/):

    docker build \
        --build-arg "GIT_URL=https://github.com/react-boilerplate/react-boilerplate.git" \
        -t react-boilerplate \
        .

Finally, we can run the output image:

    docker run -it --rm -p 80:80 react-boilerplate

The demo page is now available at <http://localhost>.

### Analysis

Multi-stage builds give us a good starting point. They allow us to
repeatably build and test our projects, resulting in a lightweight,
production-ready image.

However, this approach is not very flexible. We can only update the
behavior of the build by updating this Dockerfile. Because cloning is
done in the Dockerfile, it would not be reasonable to store this in VCS.
Therefore, we have store all build logic in a centralized location.
While some CI software takes this approach, I believe that storing build
logic in VCS improves repeatability.

Finally, docker build will cache each instruction. We will only clone
the source code the first time we run this job. We could address this
shortcoming by either disabling the docker cache entirely, or adding
another build argument `COMMIT_ID`.

Multi-stage builds give us a truly adhoc CI process. Even so, this could
be useful in contexts where a minimal system is desired.

We could overcome many of these shortcomings by adding more build
arguments. Instead, we elect to take another approach.

Orchestrating a Build Across Multiple Containers
------------------------------------------------

While Docker build gave us a good starting point, it lacked the
flexibility to separate the boilerplate from the build logic. In order
to achieve this, we can run the process inside Docker containers.

Recall our CI process from [above](#using-docker-build):

1.  Check out a clean copy of the repository
2.  Build and test the code
3.  Build a Docker image

Instead of running each of these in a `Dockerfile` instruction, they
will execute in a separate Docker container. We will maintain the source
code and build artifacts in a shared volume. Additionally, we’ll create
a container that automates running the stages.

This leaves us with the following containers to create:

-   git-builder
-   node-builder
-   docker-builder
-   orchestrator

The orchestrator will handle creating the shared volume and delegating
tasks to the other containers.

### Creating the Docker Volume

A data volume is a special directory within one more more containers
that can be used to share or persist data, independent of the
container’s lifecycle.[@docker-volumes] We will use a data volume to
persist our source tree between build stages.

Our first step is to create the volume:

    docker volume create orch_build

### Git Builder

The git builder will check out a clean copy of the repository in the
data volume. We use the following `Dockerfile`:

    FROM debian:stable-slim

    RUN apt-get update && \
    apt-get -y install git

    WORKDIR /usr/src/app
    VOLUME /usr/src/app

    CMD [ \
        "/bin/sh", \
        "-c", \
        "git init && git fetch $GIT_URL && git reset --hard FETCH_HEAD"]

By default, this will fetch and checkout the repository defined by the
environment variable `GIT_URL` at `/usr/src/app`. We build it using
standard docker commands:

    docker build -t git-builder .

We can now run it, again using `react-boilerplate` as an example:

    docker run -it \
        --volume orch_build_1:/usr/src/app \
        --env GIT_URL=https://github.com/react-boilerplate/react-boilerplate.git \
        git-builder

### Node Builder

The next step is to build and test the code. For a node.js application,
we will run the standard `npm` scripts `install`, `test`, and `run`. Our
`Dockerfile` is straightforward:

    FROM node:slim

    WORKDIR /usr/src/app
    VOLUME /usr/src/app

    CMD [ \
        "/bin/sh", \
        "-c", \
        "npm install && npm run test && npm run build"]

We again use the data volume to access the source code at
`/usr/src/app`. We now build the image:

    docker build -t node-builder .

Then we run it:

    docker run -it \
        --volume orch_build_1:/usr/src/app \
        node-builder

### Docker Builder

The docker builder creates a docker image using generated artifacts from
the build stage. This means that we will be running docker build from
another container.

We can run docker inside another container by mounting the host’s docker
socket. For example:

    docker run -it --rm -v /var/run/docker.sock:/var/run/docker.sock docker

This allows us to run docker commands from inside the container.

We can now create the `Dockerfile`:

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

This `Dockerfile` has a few interesting instructions in it. First, we
take an environment variable `BUILD_TAG`. This is the the tag we want
docker to build. For example, if we’re building `react-boilerplate`, we
might set this to `react-boilerplate:latest`.

Secondly, we have an `entrypoint.sh` script. This will copy the built
artifacts and the desired `Dockerfile` to `/tmp/build`.

Finally, we copy the directory `dockerfiles` into the image at
`/dockerfiles`. This will allow us to keep a handful of `Dockerfiles` in
order to build different types of projects.

The `entrypoint.sh` script is simple:

    #!/bin/sh
    cp -r /usr/src/app/build .
    cp "$DOCKERFILE" ./Dockerfile

    exec "$@"

Before it can run, we have to make it executable:

    chmod +x entrypoint

We then create our nginx `Dockerfile` at `dockerfiles/nginx.dockerfile`:

    FROM nginx:alpine

    COPY build /usr/share/nginx/html

We can now build it:

    docker build -t docker-builder .

Then run it:

    docker run -it \
        --volume orch_build_1:/usr/src/app \
        --volume /var/run/docker.sock:/var/run/docker.sock \
        --env BUILD_TAG="react-boilerplate:latest" \
        --env DOCKERFILE="/dockerfiles/nginx.dockerfile" \
        docker-builder

We have now manually run the entire CI process. Our next job is to
automate it with the orchestrator.

### Orchestrator

The orchestrator’s job is to run all of the previous steps in sequence.
Our `Dockerfile` is simple:

    FROM docker

    COPY build.sh /build.sh
    ENTRYPOINT ["/build.sh"]

Again, we have an entrypoint script `build.sh`. It’s a small script that
runs all of the build stages:

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
        --env GIT_URL=$GIT_URL
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

First, we generate a name for the data volume. If it doesn’t already
exist, we create it. We then run each of the other stages in docker
containers.

We again make sure that `build.sh` is executable:

    chmod +x build.sh

We build it:

    docker build -t orchestrator .

Then run it:

    docker run -it \
        --volume /var/run/docker.sock:/var/run/docker.sock \
        --env GIT_URL="https://github.com/react-boilerplate/react-boilerplate.git"
        --env BUILD_TAG="react-boilerplate:latest" \
        --env DOCKERFILE="/dockerfiles/nginx.dockerfile" \
        orchestrator

We have now built a repeatable, automated process using only Docker.

Next Steps
----------

While we have a good start, we are by no means done. In order to create
a complete CI system, we need to make several improvements.

**Web Service:** We need a web service in order to trigger jobs from a
VCS hook. A ReST service is the preferred way to achieve this. We should
also provide a web interface.

**Build Definition**: We need a way to configure several build jobs. I
prefer these definitions to be stored in VCS along with the source code.
The YAML format should be considered since it is easy to write and read.

**Docker Build**: In order to account for multiple projects, we must
maintain a centralized collection of `Dockerfile`s. This would be
difficult to maintain, so I would suggest dropping this requirement
altogether. More general alternatives exists, such as [Source to
Image](https://github.com/openshift/source-to-image), but these suffer
from the same problems.

**Scaling**: Our current system can not schedule builds across multiple
nodes. As of Docker 1.12, Swarm Mode is included, which can manage a
cluster of Docker Engines.[@docker-swarm] Extending our containers to
swarm services are trivial.

References {#references .unnumbered}
----------
