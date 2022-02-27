#! /usr/bin/env bash

set -euo pipefail

# I found the pip package installs without issue whereas the homebrew package
# seems to depend on an older version of openssl that doesn't get installed
# properly.
# pip3 install youtube-dl
