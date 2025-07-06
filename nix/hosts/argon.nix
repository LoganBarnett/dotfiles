################################################################################
# argon is the wireguard host.
# argon used to be an OctoPi server running octo-print.
################################################################################
{ flake-inputs, host-id, ... }: let
  system = "aarch64-linux";
in {
  imports = [
    ../nixos-modules/raspberry-pi-4.nix
    ../nixos-modules/raspberry-pi-host.nix
    ../nixos-modules/nix-builder-provide.nix
    ../nixos-modules/server-host.nix
    ../nixos-modules/wireguard-server-standard.nix
    ../nixos-modules/dns-dynamic-ip-home.nix
  ];
  nixpkgs.overlays = [
    (final: prev: {
      # This is a heavy build for a little Pi.  Reduce the jobs to prevent
      # memory thrashing.
      dness = prev.dness.overrideAttrs (old: {
        CARGO_BUILD_JOBS = "1";
        buildPhase = ''
          runHook preBuild
          cargo build --profile release --frozen --offline --jobs 1 --target aarch64-unknown-linux-gnu
          runHook postBuild
        '';
        checkPhase = ''
          runHook preCheck
          cargo test --jobs 1 --profile release --target aarch64-unknown-linux-gnu --offline -- --test-threads=1
          runHook postCheck
        '';
      });
    })
  ];
  # networking.hostId is needed by the filesystem stuffs.
  # An arbitrary ID needed for zfs so a pool isn't accidentally imported on
  # a wrong machine (I'm not even sure what that means).  See
  # https://search.nixos.org/options?channel=24.05&show=networking.hostId&from=0&size=50&sort=relevance&type=packages&query=networking.hostId
  # for docs.
  # Get from an existing machine using:
  # head -c 8 /etc/machine-id
  # Generate for a new machine using:
  # head -c4 /dev/urandom | od -A none -t x4
  networking.hostId = "c1042166";
  nixpkgs.hostPlatform = system;
}
