#!/usr/bin/env bash

dir=$(mktemp -d)
cd $dir
latestversion download BSData/wh40k-10e
tar -xzvf wh40k-*.tar.gz
# Remove so the poor glob we have doesn't pick it up on the move.
rm *.tar.gz
data_path="$HOME/Dropbox/Apps/BattleScribe/data/Warhammer 40,000 10th Edition"
mv "$data_path" "$data_path.old"
mkdir "$data_path"
mv wh40k-*/* "$data_path"
