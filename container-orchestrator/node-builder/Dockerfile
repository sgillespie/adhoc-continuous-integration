FROM node:slim

WORKDIR /usr/src/app
VOLUME /usr/src/app

CMD [ \
    "/bin/sh", \
    "-c", \
    "npm install && npm run test && npm run build"]
