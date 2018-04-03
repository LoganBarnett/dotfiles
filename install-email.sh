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
cd ~/.emacs.d/layers && git clone git@github.com:cmiles74/spacemacs-notmuch-layer.git notmuch || true
# Use a commit and we can update manually.
cd ~/.emacs.d/layers/notmuch && git checkout c81b2fd1bdb4294edef6d16041bfc880d26b2774

echo "Setting up email authentication..."
# ~/.email-creds.txt comes from install-private.sh Per instructions here:
# http://www.macs.hw.ac.uk/~rs46/posts/2014-01-13-mu4e-email-client.html
# ~/.email-creds.txt is a file that needs to match the following template:
# machine <smtp-host> login <email-address> port 587 password <password> Once
# encrypted, the ~/.email-creds.txt is no longer needed. TODO: Make the
# plaintext storage of the email password more secure, or move it to the
# keychain perhaps.
echo "machine smtp.gmail.com login logustus@gmail.com port 587 password $(echo ~/.email-creds.txt)" > ~/.authinfo.txt
gpg --output ~/.authinfo.gpg --symmetric ~/.authinfo.txt
rm ~/.authinfo.txt
rm ~/.email-creds.txt
echo "Done setting up email support."
