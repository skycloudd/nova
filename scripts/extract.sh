#!/bin/sh

set -e

NAME=$1
mv $NAME $NAME.gz
gzip -d "$NAME.gz"
