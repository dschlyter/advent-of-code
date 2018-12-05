#!/bin/bash

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

cd "src/2018"
./fetch.sh

curr_date="$(date '+%-d')"
prev_date=$(($curr_date - 1))
cp "day${prev_date}.hs" "day${curr_date}.hs"
stack ghci --ghci-options "day${curr_date}.hs"
