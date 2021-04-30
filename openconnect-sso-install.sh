#!/usr/bin/env bash
set -euo pipefail

# This must be run from within venv until I can figure out how to properly
# install this via nix.
nix-shell -p python3 --command 'python -m venv venv'

# The PyQt dependencies are not explicit dependencies but openconnect-sso won't
# run without them.
./venv/bin/pip3 install PyQt5
./venv/bin/pip3 install PyQtWebEngine
./venv/bin/pip3 install openconnect-sso

# openconnect-sso must be run from ./venv/bin/pip3/openconnect-sso
