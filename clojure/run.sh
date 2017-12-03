#!/usr/bin/env bash

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

if [ "$1" == "dev" ]; then
    boot repl -n advent.advent2017 -s -p 1337 watch refresh
elif [ "$1" == "repl" ]; then
    boot repl -c -p 1337
else
    echo "First arg should be one of: dev, repl"
fi
