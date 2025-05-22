# If you run into this issue:
#
# All overlays passed to nixpkgs must be functions.
#
# 1. Move this file (ie default.nix.old).
# 2. Run the installer/switch again.
# 3. Observe it break on overlays/default.nix not existing.
# 4. Move the file back.
# 5. Run the installer/switch yet another time.
#
# Hopefully we one day find out what is causing this.
#
# See: https://github.com/NixOS/nix/issues/8443
{ flake-inputs, system, ... }: [
  (import ./test-script.nix)
  (import ./amdvlk-348903-fix.nix)
  (import ./battlescribe-update-data.nix)
  (import ./blueutil.nix)
  (import ./cacert.nix)
  (import ./proton-deploy.nix)
  # (import ./crystal.nix)
  (import ./gnupg.nix)
  (import ./hiera-eyaml.nix)
  (import ./lastversion.nix)
  (import ./man-pages-fix.nix)
  (import ./maven.nix)
  (import ./nightlight.nix)
  (import ./percol.nix)
  (import ./prusa-slicer.nix)
  # (import ./python-hatch-vcs-fix.nix)
  # (import ./tmux.nix)
  (import ./wine.nix)
  # Give us rust-docs.
  (import ./rust.nix)
  (import ./signal-desktop.nix { inherit flake-inputs system; } )
  # (import (builtins.fetchTarball
  #   "https://github.com/oxalica/rust-overlay/archive/master.tar.gz"))
  # (import ./openconnect-sso.nix)
  # Kept as an example of using someone else's overlay remotely.
  # (import "${builtins.fetchTarball https://github.com/vlaci/openconnect-sso/archive/master.tar.gz}/overlay.nix")
  (import ./wireguard.nix)
  (import ./zig.nix)
  # Needed until https://github.com/NixOS/nixpkgs/pull/391654 is merged.
  (final: prev: {
    nc4nix = prev.nc4nix.overrideAttrs (old: {
      meta.platforms = final.lib.platforms.unix;
    });
  })
  (final: prev: {
    dness = final.callPackage ../derivations/dness.nix {};
  })
]
