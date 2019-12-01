#!/bin/bash

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

run() {
    if [[ "$1" == *.rs ]]; then
        rustc $@ -o program && ./program
        rm program 2> /dev/null || true
    else
        ./"$1"
    fi
}

day=$1

program=$(echo "day"$1*)
input=${2:-input/day$1.txt}

if [[ $input == "-" ]]; then
    run $program
else
    cat $input | run $program
fi
