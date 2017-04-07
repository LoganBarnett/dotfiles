#! /usr/bin/env bash

set -e

# gpg 2.0 doesn't work with emacs easypg or whatever it is. gpg 2.1 works well
# with it, but homebrew doesn't want to make it easy because of backwards
# compatibility reasons.

echo "Installing correct version of gnupg (2.1)..."
# Ensure we don't have an existing gpg in the way.
brew uninstall gnupg2 gpg-agent dirmngr --force
# Forcefully install gpg 2.1 (yes, they force us even with the version number).
brew install gnupg21 --force
# Forcefully link the tool - seriously.
brew link gnupg21 --force
# Create a symlink so emacs can find it. I've had trouble finding the config
# variable for the emacs plugin.
ln -s -n -f /usr/local/bin/gpg2 /usr/local/bin/gpg

echo "linking gpg settings..."
mkdir -p ~/.gnupg
if [ ! -f ~/.gnupg/gpg-agent.conf ] || [ ! -f ~/.gnupg/gpg.conf  ]; then
    echo "~/.gnupg config files are present but not symlinks, aborting..."
    exit 1
else
    ln -s -n -f $PWD/gpg-agent.conf ~/.gnupg/gpg-agent.conf
    ln -s -n -f $PWD/gpg.conf ~/.gnupg/gpg.conf
fi
