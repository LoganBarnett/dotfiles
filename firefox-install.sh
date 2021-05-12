#!/usr/bin/env bash
set -euo pipefail

# Firefox uses garbled directory names for profiles. We'll just apply this to
# all of them.
profiles=$(find ~/Library/Application\ Support/Firefox/Profiles/ -type d -depth 1)
for profile in "$profiles" ; do
  ln -snf $PWD/custom-css/firefox.css "$profile/userChrome.css"
done
