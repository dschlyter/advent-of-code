#!/bin/bash

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

day=$1

while inotifywait -e close_write day$day.rs; do
    clear
    timeout 60 run.sh $day
done
