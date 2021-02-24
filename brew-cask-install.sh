#!/usr/bin/env bash

set -e

dir="$(dirname "${BASH_SOURCE[0]}")"
source $dir/dotfiles-functions.sh

# This fixes outdated tasks using the "undent" method. I've found Homebrew
# doesn't do much to preserve backwards compatibility with older packages (or
# more accurately: older Homebrew package definitions). I expect many
# workarounds such as this will pile up in this file and the install-packages.sh
# file. This particular workaround can be found (and adapted to work with this
# sed) from here:
# https://github.com/Homebrew/homebrew-cask/issues/49716#issuecomment-413515303
log "Fixing outdated Homebrew package definitions..."
find "$(brew --prefix)/Caskroom/"*'/.metadata' -type f -name '*.rb' | \
    xargs grep 'EOS.undent' --files-with-matches | \
    xargs sed -i 's/EOS.undent/EOS/'

log "Outdated Homebrew package definitions should be fixed."

brew cask upgrade

# TODO: Find a better way to reinstall istat-menus (cannot be stopped once
# running, short of an "uninstall").
#
# mongodb is locked in version 4.0.3_1 due to licensing issues that I don't
# think can impact me. I don't mind if it updates, but if I have trouble
# installing mongodb on my various machines, it could be due to having
# installed a version with the later license. In this case I should
# reinstall mongodb in order to make my life easier. I am not aware of fixes
# between the patch-versions here that I need.
#
# I'd like to move these to Nix but I don't think they exist yet.
CASKS="
alfred
chromium
diffmerge
font-source-code-pro
gimp
google-chrome
slack
virtualbox
zoomus
"

log "Installing homebrew casks."
brew cask install $CASKS

# For my personal machines I can install packages but these are not appropriate
# for work machines.
if [[ "$HOST" =~ "lbarnett" ]]; then
  MACHINE_CASKS="
razer-synapse
"
else

  # The following are forbidden:
  # corsair-icue - This hogs resources and causes audio to skip when other
  # processor intensive activities are going. Perhaps a later update will fix
  # this, but my limited research shows MacOs support to be relatively new.
  MACHINE_CASKS="
arduino
discord
mixxx
obs
silverlight
steam
xbox360-controller-driver-unofficial
"
fi

brew cask install $MACHINE_CASKS

log "Done installing casks."
