#! /usr/bin/env bash

# This script handles any setup for email that extends beyond just installing
# vanilla packages.

echo "Copying certificates for openssl..."
cp $PWD/certs/* /usr/local/etc/openssl/certs/

./link-dotfile.sh mbsyncrc
./link-dotfile.sh notmuch-config
brew-upstall isync
brew-upstall mu --with-emacs
brew-upstall notmuch --with-emacs

echo "Creating notmuch index. This may take a while."
notmuch new

echo "Installing Spacemacs notmuch layer..."
# Consider https://github.com/tj/git-extras/blob/master/man/git-force-clone.md
git clone git@github.com:cmiles74/spacemacs-notmuch-layer.git notmuch || true
# Use a commit and we can update manually.
cd spacemacs-notmuch-layer && git checkout c81b2fd1bdb4294edef6d16041bfc880d26b2774

echo "Done setting up email support."
