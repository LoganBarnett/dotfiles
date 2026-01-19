################################################################################
# Setup a Nix cache via Attic (https://github.com/zhaofengli/attic).
#
# It's easy to miss the docs since the README is sparse, but there is still some
# good documentation: https://docs.attic.rs
################################################################################
{ atticd-port, flake-inputs, ... }: {
  imports = [
    flake-inputs.attic.nixosModules.attic
  ];
  services.atticd = {
    enable = true;

    settings = {
      # Use HTTPS to expose.  See ./https.nix for an easy shoe-in HTTPS
      # forwarding.
      listen = "127.0.0.1:${atticd-port}";
    };
  };
}
