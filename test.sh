#!/bin/sh

set -e

cargo build

for file in examples/*.nv; do
    ./target/debug/nova "$file" > /dev/null
done
