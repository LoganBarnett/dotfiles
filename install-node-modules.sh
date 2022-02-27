#! /usr/bin/env bash

set -e

# TODO: This script is disabled. Convert the necessary packages to nix as I need
# them.

# Ensure we aren't going to have a fight between nodenv and nvm. Old setups of
# mine will have nvm so it will need to be removed.
slog "Ensuring nvm is not installed. This will show an error message if it is
missing. Disregard."
brew remove nvm || true
./install-package.sh nodenv
# This plugin gives us `nodenv update-version-defs` to upgrade available node
# versions. See also the zshrc file for setting up its require environment
# variable, NODE_BUILD_DEFINITIONS.
./install-package.sh nodenv/nodenv/node-build-update-defs

# nodenv must be initialized in part because an upgrade will cause the old path
# to fail.
eval "$(nodenv init -)"
slog 'Other shells you have will need to run the following:

eval "$(nodenv init -)"
'

nodenv install -s 10.15.3

# nodenv switches the node version based on a .node-version when
# cd/pushd/popd into a directory.
# tern for JS autocomplete and other hinting
# https://github.com/avh4/elm-format/issues/226
# elm-test is a unit/fuzz testing framework.
# https://github.com/elm-community/elm-test
NODE_MODULES="
js-beautify
marked
tern
"

npm i -g $NODE_MODULES
