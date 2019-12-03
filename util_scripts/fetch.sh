#!/bin/bash

# Setup:
# 1. Fetch an input with the webinspector
# 2. Save your cookie header to ~/.secrets/advent-cookie
# 3. Make sure the "input" dir is created (this will not be created automatically since you might be in the wrong directory)

set -e

curr_date="$(date '+%-d')"
curr_year="$(date '+%-Y')"
date="${1:-$curr_date}"
cookie="$(cat $HOME/.secrets/advent-cookie)"

curl "https://adventofcode.com/$curr_year/day/$date/input" \
    -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' \
    -H 'accept-encoding: gzip, deflate, br' \
    -H 'accept-language: en-US,en;q=0.9,sv-SE;q=0.8,sv;q=0.7,nb;q=0.6' \
    -H "cookie: $cookie" \
    --compressed \
    > "input/day${date}.txt"

echo Fecthed "input/day${date}.txt"
