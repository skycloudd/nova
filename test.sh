#!/bin/sh

for file in examples/*.nv; do
    cargo run -- "$file" > /dev/null
done
