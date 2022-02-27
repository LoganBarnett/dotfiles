#!/usr/bin/env bash
set -euo pipefail

firefoxDir="$HOME/Library/Application Support/Firefox/Profiles/"
mkdir -p "$firefoxDir"
# Firefox uses garbled directory names for profiles. We'll just apply this to
# all of them.
# TODO: This is a "read only filesystem", so we need to figure out if we want
# this still at all.
exit 0
profiles=$(find "$firefoxDir" -type d -depth 1)
for profile in "$profiles" ; do
  ln -snf $PWD/custom-css/firefox.css "$profile/userChrome.css"
done
