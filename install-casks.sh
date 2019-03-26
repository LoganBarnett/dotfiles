#!/usr/bin/env bash

set -e

brew cask upgrade

# NOTE: I omitted razer-synpase due to its intrusive nature. Install manually
# and then uninstall when keyboard config is complete.
# TODO: Find a better way to reinstall istat-menus (cannot be stopped once
# running, short of an "uninstall").
CASKS="
adobe-connect
alfred
arduino
chromium
diffmerge
discord
electric-sheep
font-source-code-pro
gimp
google-chrome
haskell-platform
hipchat
mumble
obs
silverlight
skype
slack
steam
stride
virtualbox
xbox360-controller-driver-unofficial
zoomus
"

echo "installing homebrew casks"
brew cask install $CASKS

echo "done installing casks"
