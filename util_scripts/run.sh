#!/bin/bash

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
# IFS=$'\n\t'

day=$1
program=$(echo "day"$1*)

input=${2:-}

if [[ "$program" == *.rs ]]; then
    rustc $program -o program
    ex="timeout 10 $(pwd)/program"
else
    echo "Unsupported program $program"
    exit 1
fi

if [[ $input == "-" ]]; then
    # pipe input from stdin
    $ex
elif [[ $input == "" ]]; then
    for file in input/day${day}_test* input/day${day}.txt; do
        if [[ -f $file ]]; then
            echo Running $file
            cat $file | $ex
        fi
    done
else
    cat $input | $ex
fi

test -f program && rm program
