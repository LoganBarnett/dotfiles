#!/usr/bin/env bash


# drivers is necessary for tools such as razer-synapse
echo "tapping drivers"
brew tap caskroom/drivers

# TODO: add alfred and istat-menus
# NOTE: I omitted razer-synpase due to its intrusive nature. Install manually
# and then uninstall when keyboard config is complete.
CASKS="
adobe-connect
arduino
aws-vault
chromium
diffmerge
discord
docker
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
virtualbox
xbox360-controller-driver
zoomus
"

echo "installing homebrew casks"
brew cask install $CASKS

echo "done installing casks"
