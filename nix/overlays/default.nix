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
[
  (import ./blueutil.nix)
  (import ./cacert.nix)
  # (import ./crystal.nix)
  (import ./gnupg.nix)
  (import ./man-pages-fix.nix)
  (import ./maven.nix)
  (import ./nixos-generators.nix)
  (import ./percol.nix)
  # (import ./python-hatch-vcs-fix.nix)
  (import ./speedtest-cli.nix)
  # (import ./tmux.nix)
  (import ./wine.nix)
  # Give us rust-docs.
  (import ./rust.nix)
  # (import (builtins.fetchTarball
  #   "https://github.com/oxalica/rust-overlay/archive/master.tar.gz"))
  # (import ./openconnect-sso.nix)
  # Kept as an example of using someone else's overlay remotely.
  # (import "${builtins.fetchTarball https://github.com/vlaci/openconnect-sso/archive/master.tar.gz}/overlay.nix")
  (import ./vpnc.nix)
  (import ./zig.nix)
]
