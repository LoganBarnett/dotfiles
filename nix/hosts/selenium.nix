################################################################################
# Trivia: Selenium is used primarily for glass production and also makes for a
# good lead and sulfur replacement in many applications.  It is required for
# many forms of life in very trace amounts and very small amounts can lead to
# toxicity.
#
# Selenium provides an OctoPrint server for the Prusia 3D FFF/FDM printer.
{ config, flake-inputs, host-id, system, ... }: {
  imports = [
    ../nixos-modules/raspberry-pi-5.nix
    ../nixos-modules/nix-builder-provide.nix
    ../nixos-modules/server-host.nix
    ../nixos-configs/octoprint-prusa-xl.nix
  ];
  services.https.fqdns."selenium.proton" = {
    enable = true;
    internalPort = config.services.octoprint.port;
  };
  # networking.hostId is needed by the filesystem stuffs.  An arbitrary ID
  # needed for zfs so a pool isn't accidentally imported on a wrong machine (I'm
  # not even sure what that means).  See
  # https://search.nixos.org/options?channel=24.05&show=networking.hostId&from=0&size=50&sort=relevance&type=packages&query=networking.hostId
  # for docs.
  # Get from an existing machine using:
  # head -c 8 /etc/machine-id
  # Generate for a new machine using:
  # head -c4 /dev/urandom | od -A none -t x4
  networking.hostId = "16474474";
  nixpkgs.hostPlatform = system;
}
