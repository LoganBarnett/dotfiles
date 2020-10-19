#!/usr/bin/env bash
set -euo pipefail

# gpg 2.0 doesn't work with emacs easypg or whatever it is. gpg 2.1 works well
# with it, but homebrew doesn't want to make it easy because of backwards
# compatibility reasons.

echo "linking gpg settings..."
mkdir -p ~/.gnupg
ln -s -n -f $PWD/gpg-agent.conf ~/.gnupg/gpg-agent.conf
ln -s -n -f $PWD/gpg.conf ~/.gnupg/gpg.conf
echo "fixing .gnupg perms..."

chmod 700 ~/.gnupg
