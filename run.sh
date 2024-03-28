#!/bin/sh

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

SCRIPT_NAME=$(basename $0)
SCRIPT_SUCCESS="${GREEN}[$SCRIPT_NAME]${NC}"
SCRIPT_FAILURE="${RED}[$SCRIPT_NAME]${NC}"

if [ "$#" -lt 1 ]; then
    echo "$SCRIPT_FAILURE Usage: $0 <source_file> [--clean]"
    exit 1
fi

input=$1
obj_file=${input%.nv}.o
executable=${input%.nv}

set -e

echo "$SCRIPT_SUCCESS compiling \`$input\` to \`$obj_file\` and running \`$executable\`"
cargo run -- $input -t -o $obj_file 
cc -o $executable $obj_file

set +e

./$executable

exit_code=$?

if [ $exit_code -ne 0 ]; then
    echo "$SCRIPT_FAILURE \`./$executable\` exited with code $exit_code"
fi

if [ "$2" = "--clean" ]; then
    echo "$SCRIPT_SUCCESS cleaning up \`$obj_file\` and \`$executable\`"

    rm $obj_file $executable
fi

exit $exit_code
