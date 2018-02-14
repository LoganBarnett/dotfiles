#! /usr/bin/env bash

set -e

./install-package.sh nvm

mkdir -p ~/.nvm

# Temporarily setup nvm for this run.

export NVM_DIR="$HOME/.nvm"
. "/usr/local/opt/nvm/nvm.sh"

# Make sure we're using the node version that emacs will use (8.1.3 currently).
nvm use 8.1.3

# avn switches the node version using n based on a .node-version when
# cd/pushd/popd into a directory.
# tern for JS autocomplete and other hinting
# elm-format forces 4 spaces (they claim this is because they are alpha) - bleh.
# https://github.com/avh4/elm-format/issues/226
# elm-test is a unit/fuzz testing framework.
# https://github.com/elm-community/elm-test
# iconv used for encoding conversion to Safari's local storage.
NODE_MODULES="
avn
avn-nvm
bower
elm-format
elm-oracle
elm-test
iconv
js-beautify
marked
"

yarn global add $NODE_MODULES

# This makes nvm work with avn
ln -f -s -n /usr/local/opt/nvm/nvm.sh ~/.nvm/nvm.sh

# avn has this as a post install setup command.
avn setup
