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
