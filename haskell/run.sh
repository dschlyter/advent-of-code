#!/bin/bash

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

cd "src/2018"
./fetch.sh

curr_date="$(date '+%-d')"
prev_date=$(($curr_date - 1))
file="day${curr_date}.hs"
test -f "$file" || cp "day${prev_date}.hs" "$file"
stack ghci --ghci-options "$file"
