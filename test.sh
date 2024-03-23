#!/bin/sh

set -e

cargo build

for file in $(find examples -name "*.nv"); do
    echo "[test.sh] running $file"
    ./target/debug/nova "$file"
done
