#! /usr/bin/env bash

set -e

echo "Installing nix..."

curl -o install-nix https://nixos.org/nix/install
curl -o install-nix.sig https://nixos.org/nix/install.sig
gpg --recv-keys B541D55301270E0BCF15CA5D8170B4726D7198DE
gpg --verify ./install-nix.sig

echo "nix will require sudoer..."
sh ./install-nix

rm ./install-nix{,.sig}

echo "Setting up the shell for nix"
. ~/.nix-profile/etc/profile.d/nix.sh
