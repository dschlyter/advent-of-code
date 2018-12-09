#!/bin/bash

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

default_date="${1:-$(date '+%-d')}"
date="${1:-$default_date}"
prev_date=$(($date - 1))

cd "src/2018"
./fetch.sh "$date"

file="day${date}.hs"
test -f "$file" || cp "day${prev_date}.hs" "$file"
stack ghci --ghci-options "$file"
