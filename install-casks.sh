#!/usr/bin/env bash


# drivers is necessary for tools such as razer-synapse
echo "tapping drivers"
brew tap caskroom/drivers

# TODO: add alfred and istat-menus
# NOTE: I omitted razer-synpase due to its intrusive nature. Install manually
# and then uninstall when keyboard config is complete.
CASKS="
chromium
diffmerge
discord
docker
gimp
google-chrome
haskell-platform
hipchat
microsoft-lync
mumble
robomongo
screenhero
silverlight
skype
slack
steam
virtualbox
xbox360-controller-driver
"

echo "installing homebrew casks"
brew cask install $CASKS

echo "done installing casks"
