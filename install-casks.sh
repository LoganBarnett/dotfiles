#!/usr/bin/env bash

set -e

# This fixes outdated tasks using the "undent" method. I've found Homebrew
# doesn't do much to preserve backwards compatibility with older packages (or
# more accurately: older Homebrew package definitions). I expect many
# workarounds such as this will pile up in this file and the install-packages.sh
# file. This particular workaround can be found (and adapted to work with this
# sed) from here:
# https://github.com/Homebrew/homebrew-cask/issues/49716#issuecomment-413515303
echo "Fixing outdated Homebrew package definitions..."
find "$(brew --prefix)/Caskroom/"*'/.metadata' -type f -name '*.rb' | \
    xargs grep 'EOS.undent' --files-with-matches | \
    xargs sed -i 's/EOS.undent/EOS/'

echo "Outdated Homebrew package definitions should be fixed."

brew cask upgrade

# TODO: Find a better way to reinstall istat-menus (cannot be stopped once
# running, short of an "uninstall").
CASKS="
adobe-connect
alfred
chromium
diffmerge
electric-sheep
font-source-code-pro
gimp
google-chrome
haskell-platform
slack
virtualbox
zoomus
"

echo "installing homebrew casks"
brew cask install $CASKS

# For my personal machines I can install packages but these are not appropriate
# for work machines.
if [[ "$HOST" =~ "lbarnett" ]]; then
  WORK_CASKS="
razer-synapse
zoomus
"
else
  PERSONAL_CASKS="
arduino
discord
obs
silverlight
skype
steam
xbox360-controller-driver-unofficial
"
  brew cask install $PERSONAL_CASKS

fi


echo "done installing casks"
