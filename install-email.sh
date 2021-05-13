#! /usr/bin/env bash

# This script handles any setup for email that extends beyond just installing
# vanilla packages.

echo "Copying certificates for openssl..."
cp $PWD/certs/* /usr/local/etc/openssl/certs/

./link-dotfile.sh mbsyncrc
./link-dotfile.sh notmuch-config
touch mailrc # in case the file doesn't exist. How to best keep this private?
./link-dotfile.sh mailrc

echo "Setting up email authentication..."
# ~/.email-creds.txt comes from install-private.sh Per instructions here:
# http://www.macs.hw.ac.uk/~rs46/posts/2014-01-13-mu4e-email-client.html
# ~/.email-creds.txt is a file that needs to match the following template:
# machine <smtp-host> login <email-address> port 587 password <password> Once
# encrypted, the ~/.email-creds.txt is no longer needed. TODO: Make the
# plaintext storage of the email password more secure, or move it to the
# keychain perhaps.
echo "machine smtp.gmail.com login logustus@gmail.com port 587 password $(cat ~/.email-creds.txt)" > ~/.gmail-authinfo.txt
# TODO this needs some improvement because it seems to crush the existing auth
# settings. I want to use authinfo for more than one thing.
gpg --batch --yes --output ~/.authinfo.gpg --symmetric ~/.gmail-authinfo.txt
gpg --batch --yes \
    --output ~/.gmail-imap-authinfo.gpg --symmetric ~/.email-creds.txt
# gpg --output ~/.authinfo.gpg --symmetric ~/.email-creds.txt
rm ~/.gmail-authinfo.txt
# Leaving this in for a little while longer.
rm ~/.email-creds.txt
echo "Done setting up email support."

# Potentially:
mu init --maildir=~/mail --my-address=logustus@gmail.com
