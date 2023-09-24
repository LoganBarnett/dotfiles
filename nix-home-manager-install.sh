
#!/usr/bin/env bash
set -eo pipefail

source bash-logging

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

slog "Installing nix packages for this host..."
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
# Somehow the main nixpkgs channel gets stomped. This makes sure it's still in.
nix-channel --add https://github.com/NixOS/nixpkgs/archive/master.tar.gz nixpkgs
nix-channel --update
nix-shell '<home-manager>' -A install
# home-manager appears to crush this, so set it afterwards.
mkdir -p ~/.config/home-manager
ln -snf $PWD/nix/overlays ~/.config/nixpkgs/overlays
mkdir -p ~/config/nix
ln -snf $PWD/nix/nix.conf ~/.config/nix/nix.conf

. $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh

slog "Building and using the home-manager settings..."
# "switch" implies "build".
home-manager switch --show-trace
