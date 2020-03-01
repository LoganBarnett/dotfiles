#! /usr/bin/env bash

set -e

# Needed for LSP support.
rustup component add rls --toolchain stable-x86_64-apple-darwin