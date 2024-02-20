#!/bin/sh

set -e

for file in examples/*.nv; do
    cargo run -- "$file" > /dev/null
done
