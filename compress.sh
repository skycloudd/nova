#!/bin/sh

NAME=$1
gzip $NAME
mv "$NAME.gz" "$NAME"
