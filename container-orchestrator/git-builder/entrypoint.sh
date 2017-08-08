#!/bin/bash

exec git init && \
    git fetch $1 && \
    git reset --hard $FETCH_HEAD
