#!/bin/bash

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

npx tsc -w --outDir exec src/*.ts
