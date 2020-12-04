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
file_ending="scala"

curl "https://adventofcode.com/$curr_year/day/$date/input" \
    -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' \
    -H 'accept-encoding: gzip, deflate, br' \
    -H 'accept-language: en-US,en;q=0.9,sv-SE;q=0.8,sv;q=0.7,nb;q=0.6' \
    -H "cookie: $cookie" \
    --compressed \
    > "input/day${date}.txt"

echo Fecthed "input/day${date}.txt"

cat input/day$date.txt | head -n 20


ydate=$((date - 1))
old_file="Day$ydate.${file_ending}"
new_file="Day$date.${file_ending}"
if [[ -f "$old_file" ]]; then
    cp "$old_file" "$new_file"
    sed -i -e "s/day$ydate/day$date/" "$new_file"
    sed -i -e "s/Day$ydate/Day$date/" "$new_file"
    rm "$new_file"-e
    # sed -i "s/day.*rs/day$date.rs/" ../Cargo.toml
fi
