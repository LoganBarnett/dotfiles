#!/usr/bin/env bash

set -euo pipefail

base_path="$HOME/Dropbox/Apps/BattleScribe/data"
dir=$(mktemp --directory)
cd $dir
lastversion download https://github.com/BSData/wh40k-10e
# -x for extract.  No long argument for it.
tar --gunzip -x --file wh40k-*.tar.gz
# Remove so the poor glob we have doesn't pick it up on the move.
# rm *.tar.gz
mv *.tar.gz "$base_path"
data_path="$base_path/Warhammer 40,000 10th Edition"
rm -rf "$data_path.old"
mv "$data_path" "$data_path.old"
mkdir "$data_path"
mv wh40k-*/* "$data_path"
