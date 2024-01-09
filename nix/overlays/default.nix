[
  (import ./blueutil.nix)
  (import ./cacert.nix)
  # (import ./crystal.nix)
  (import ./gnupg.nix)
  (import ./maven.nix)
  (import ./percol.nix)
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
]
