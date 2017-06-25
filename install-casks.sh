#!/usr/bin/env bash


# drivers is necessary for tools such as razer-synapse
echo "tapping drivers"
brew tap caskroom/drivers

# TODO: add alfred and istat-menus
# NOTE: I omitted razer-synpase due to its intrusive nature. Install manually
# and then uninstall when keyboard config is complete.
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
