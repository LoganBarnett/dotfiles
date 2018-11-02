#!/usr/bin/env bash

set -e

# drivers is necessary for tools such as razer-synapse
echo "tapping drivers"
brew tap caskroom/drivers

# NOTE: I omitted razer-synpase due to its intrusive nature. Install manually
# and then uninstall when keyboard config is complete.
# TODO: Find a better way to reinstall istat-menus (cannot be stopped once
# running, short of an "uninstall").
CASKS="
adobe-connect
alfred
arduino
aws-vault
chromium
diffmerge
discord
electric-sheep
gimp
google-chrome
haskell-platform
hipchat
microsoft-lync
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

echo "installing Source Code Pro font"
brew tap caskroom/fonts && brew cask install font-source-code-pro

echo "done installing casks"
