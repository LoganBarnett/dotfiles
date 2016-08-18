#!/usr/bin/env bash

# TODO: add alfred and istat-menus
CASKS="slack chromium discord skype hipchat microsoft-lync mumble screenhero silverlight google-chrome diffmerge"

echo "installing homebrew casks"
brew cask install $CASKS
