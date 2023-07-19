#! /usr/bin/env bash

set -e

# rustup needs to manage rust, so don't actually install rust here.
./install-package.sh rustup-init

# This potentially modifies some files. I should keep an eye on this in case I
# want to make the edits manually and then disable the automatic modifications
# with --no-modify-path.
rustup-init -y

# Needed for LSP support.
# TODO: Make some installation functions to capture things such as the
# architecture.
[[ $(uname -p) == 'arm' ]] && arch='aarch64' || arch='x86_64'
rustup component add rls --toolchain stable-$arch-apple-darwin
