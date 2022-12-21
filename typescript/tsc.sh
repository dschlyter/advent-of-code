#!/bin/bash

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

# picking es5 target since es3 gives some errors, and higher gives error 'Cannot use import statement outside a module'
npx tsc -w -target es5 --outDir exec src/*.ts
