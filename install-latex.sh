#! /usr/bin/env bash

set -e

echo "Installing latex support..."
# mactex adds latex support. pandoc requires latex to export to PDF amongst
# other things. It also installs dvipng needed for inline latex previews.
brew cask install mactex

# I have added /Library/TeX/texbin to the PATH since I need other utilities from
# there as well. See zshrc for the PATH addition.

# Make sure the LateX package manager is up to date.
wget http://mirror.ctan.org/systems/texlive/tlnet/update-tlmgr-latest.sh
chmod +x update-tlmgr-latest.sh
./update-tlmgr-latest.sh
# Setup circuit support circuitikz.
sudo tlmgr install circuitikz
echo "[DONE] Installed latex support!"
