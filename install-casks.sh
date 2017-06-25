#!/usr/bin/env bash

# TODO: add alfred and istat-menus
CASKS="
slack
chromium
discord
skype
haskell-platform
hipchat
microsoft-lync
mumble
screenhero
silverlight
google-chrome
diffmerge
robomongo
steam
virtualbox
gimp
xbox360-controller-driver
"

echo "installing homebrew casks"
brew cask install $CASKS
