#! /usr/bin/env bash

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
marked
tern
"

npm i -g $NODE_MODULES

ln -s -n $PWD/jshintrc ~/.jshintrc
