#!/usr/bin/env bash
set -euo pipefail

# oq is like jq but for xml. It parses xml and yaml files and can convert them
# to xml, yaml, or json (with jq as a backend). It uses the same snytax as jq
# with the exception of the -i and -o arguments to indicate which format is to
# be used. I believe it just uses jq internally for all translations.

if [ $(uname) = 'Darwin' ]; then
  brew tap blacksmoke16/tap
fi

./install-package.sh oq
