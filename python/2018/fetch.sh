#!/bin/bash

set -e

curr_date="$(date '+%-d')"
date="${1:-$curr_date}"
cookie="$(cat $HOME/.secrets/advent-cookie)"

curl "https://adventofcode.com/2018/day/$date/input" \
    -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' \
    -H 'accept-encoding: gzip, deflate, br' \
    -H 'accept-language: en-US,en;q=0.9,sv-SE;q=0.8,sv;q=0.7,nb;q=0.6' \
    -H "cookie: session=$cookie" \
    --compressed \
    > "input/day${date}.txt"