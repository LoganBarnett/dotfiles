#! /usr/bin/env bash

# tern for JS autocomplete and other hinting
NODE_MODULES="tern marked bower avn avn-nvm iconv"

npm i -g $NODE_MODULES

ln -s -n $PWD/jshintrc ~/.jshintrc
