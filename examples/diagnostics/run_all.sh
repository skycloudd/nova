#!/bin/sh

set -e

cargo build

# for every file ending in .nv in the current directory
# run `bat <file>`
# run `cargo run -- <file>`

for file in *.nv; do
    echo "\n"
    bat $file -l rb
    ../../target/debug/nova $file
done
