#! /usr/bin/env bash

set -e

# rustup needs to manage rust, so don't actually install rust here.
./install-package.sh rustup-init

# This potentially modifies some files. I should keep an eye on this in case I
# want to make the edits manually and then disable the automatic modifications
# with --no-modify-path.
rustup-init -y

source $HOME/.cargo/env

# Needed for LSP support.
rustup component add rls --toolchain stable-x86_64-apple-darwin