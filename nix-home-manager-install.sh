
#!/usr/bin/env bash
set -eo pipefail

source nix/bash-logging

# Establish the NIX_PATH for home-manager. Probably a symptom of running out of
# a stale terminal session. I do see some errors using this, but they aren't
# fatal errors.
export NIX_PATH="$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH:\
nixpkgs-overlays=$HOME/.config/nixpkgs/overlays/"
export PATH="\
/nix/var/nix/profiles/default/bin:\
$HOME/.nix""-profile/bin:\
${PATH}\
"

slog "First, ensure we are using the right certs in the nix-daemon..."
# There is some advice to do this per https://github.com/NixOS/nix/issues/2780
# but it doesn't work because the System Integrity Protection must be disabled
# to use setenv.
# sudo launchctl setenv \
#      NIX_SSL_CERT_FILE \
#      "/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt"
sudo launchctl kickstart -k system/org.nixos.nix-daemon
# Per https://github.com/NixOS/nix/issues/2899 the TLS certificates can get
# borked.  This unborks them:
sudo rm /etc/ssl/certs/ca-certificates.crt
sudo ln -s \
     /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt \
     /etc/ssl/certs/ca-certificates.crt
# I think restarting might not be a blocking operation.  Just wait a second just
# in case.
sleep 1
slog "Installing nix packages for this host..."
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
# Somehow the main nixpkgs channel gets stomped. This makes sure it's still in.
nix-channel --add https://github.com/NixOS/nixpkgs/archive/master.tar.gz nixpkgs
nix-channel --update
nix-shell '<home-manager>' -A install
# home-manager appears to crush this, so set it afterwards.
mkdir -p ~/.config/home-manager
# The home.nix appears to need to go here now. Older versions use the other
# symlink, but keeping it causes a warning. Kept for posterity.
# ln -snf $PWD/nix/home.nix ~/.config/nixpkgs/home.nix
ln -snf $PWD/nix/home.nix ~/.config/home-manager/home.nix
ln -snf $PWD/nix/flake.nix ~/.config/home-manager/flake.nix
ln -snf $PWD/nix/flake.lock ~/.config/home-manager/flake.lock
# If you see the "overlays must be a directory" error, look further. This is
# likely an error of an error.
ln -snf $PWD/nix/overlays ~/.config/nixpkgs/overlays
mkdir -p ~/.config/nix
ln -snf $PWD/nix/nix.conf ~/.config/nix/nix.conf

. $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh

slog "Building and using the home-manager settings..."
# "switch" implies "build".
home-manager switch --show-trace
