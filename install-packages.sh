#!/usr/bin/env bash
set -e

# TODO: Lock postgres to 9.5 until I can upgrade the DB.
# This looks promising: https://gist.github.com/giannisp/ebaca117ac9e44231421f04e7796d5ca
if [ $(uname) = 'Darwin' ]; then

    which brew || \
        /usr/bin/ruby -e \
            "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    xcode-select --install || true

    # Upgrade existing packages before installing (potentially) new ones.
    brew outdated | xargs brew reinstall

    BREWS="
cask
"

    log "Updating homebrew..."
    brew update

    log "Installing brews..."
    for brew in $BREWS
    do
      ./install-package.sh $brew
    done

    ./install-gpg.sh

    ./install-aws-cli-tools.sh

elif [ $(uname) = 'Linux' ]; then
  if [ $(which apt-get) != '' ]; then
    log "Installing packages via apt-get"
    # how can you not have curl? ugh
    APTS="
    jenv
    thefuck
    "
    sudo apt install -y -qq $APTS
  else
    # we must be in some redhat based distro
    log "yum not supported yet!"
  fi

  # thefuck is installed via pip on linux
  sudo -H pip install thefuck

else
  log "Skipping brew install - not on a supported OS."
fi

log "Installing misc python packages..."

pip install upsidedown # Needed for tflip.
pip3 install xdice # xdice requires python3.

# pip3 doesn't have a PATH location, or xdice doesn't provide a binary (the docs
# suggest that pip3 is at fault though). Either way we're left fixing it
# manually for now.
ln -f -s /usr/local/lib/python3.7/site-packages/xdice.py /usr/local/bin/xdice
chmod +x /usr/local/lib/python3.7/site-packages/xdice.py

./youtube-dl-install.sh
