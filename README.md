# Adhoc Continuous Integration
> Building a CI tool from the ground up with Docker

-   [Introduction](#introduction)
-   [What is Continuous Integration?](#what-is-continuous-integration)
-   [Prerequisites](#prerequisites)
-   [Using Docker Build](#using-docker-build)
-   [References](#references)

Introduction
------------

There are several open source Continuous Integration (CI) tools
available. Unfortunately, when choosing a CI tool, one must make
compromises. We believe that CI software should be

-   Open Source
-   Easy to Install
-   Easy to Scale
-   Usable in the Cloud or on Bare Metal

Fortunately, with the emergence of Docker, we can easily build such a
tool. We present this system in the following sections.

What is Continuous Integration?
-------------------------------

Continuous Integration is more than a piece of software, it’s a
practice. While the focus of this paper is on the software, we briefly
discuss these two topics.

### The Practice

Continuous Integration is the practice of frequently integrating changes
into a shared mainline.[@fowler2006] With modern Version Control Systems
(VCS), this means that developers are merging their commits into
`master`, or a shared `develop` branch.

Along with this practice, Continuous Integration also has the following
guidelines:[@fowler2006]

-   Automate the Build
-   Make the Build Self Testing
-   Every Commit Should Build on an Integration Machine

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

Furthermore, we assume `npm run build` will generate artifacts in
`build/`.

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
While some CI software takes this approach, we believe that storing
build logic in VCS improves repeatability.

Finally, docker build will cache each instruction. We will only clone
the source code the first time we run this job. We could compensate for
this by using various cache-busting tricks.

Multi-stage build give us a truly adhoc CI process. This could be useful
in several contexts. For example:

-   Early stages of development
-   Environments where a full blown CI system is impractical
-   Small environments

We could overcome many of these shortcomings by adding more build
arguments. Instead, we elect to take another approach entirely.

References {#references .unnumbered}
----------
