#! /usr/bin/env bash

set -e

# Ensure nodenv has occurred so we're on the right version of node in the
# code-injector director.
eval "$(nodenv init -)"

node ./bootstrap-code-injector.js
cd ../code-injector
yarn grunt
