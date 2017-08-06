#!/bin/bash
set -x

echo "$@"

exec git clone $1 .
