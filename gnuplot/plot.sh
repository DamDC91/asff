#!/bin/bash

function USAGE() {
    echo "$0: USAGE <data file name>"
    exit 2
}

DIR=$(realpath $0)
DIR=$(dirname "$DIR")

if [ $# -lt 1 ]; then
    USAGE
fi
FILENAME=$1

command -v gnuplot >/dev/null 2>&1
ret_code=$?
if [ "$ret_code" -ne 0 ]; then
    echo "gnuplot is not present in PATH"
fi

gnuplot -p -c "$DIR/script.plt" "$FILENAME"
