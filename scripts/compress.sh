#!/bin/sh

set -e

NAME=$1
gzip $NAME
mv "$NAME.gz" "$NAME"
