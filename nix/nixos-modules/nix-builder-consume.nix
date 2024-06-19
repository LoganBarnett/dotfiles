################################################################################
# Make this host consume our various builders.
################################################################################
{ config, pkgs, ... }: let
  # sshKey = config.age.secrets.builder-key.path;
  sshKey = "/etc/nix/builder_ed25519";
  sshUser = "nixremote";
  toBase64 = (pkgs.callPackage ../base64.nix {}).toBase64;
in {
  nix.buildMachines = [
    {
      inherit sshKey sshUser;
      hostName = "nickel.proton";
      systems = [
        "aarch64-linux"
        "armv6l-linux"
        "armv7l-linux"
      ];
      protocol = "ssh-ng";
      # Keep this host from being bogged down by builds.
      maxJobs = 1;
      # What's this for?
      speedFactor = 2;
      supportedFeatures = [ "benchmark" ];
      mandatoryFeatures = [];
      publicHostKey = toBase64
        (builtins.readFile ../secrets/builder-key.pub)
      ;
    }
    {
      inherit sshKey sshUser;
      hostName = "lithium.proton";
      # Or use systems with a list.
      system = "x86_64-linux";
      protocol = "ssh-ng";
      # Keep this host from being bogged down by builds.
      maxJobs = 1;
      # What's this for?
      speedFactor = 2;
      supportedFeatures = [ "big-parallel" "benchmark" "kvm" ];
      mandatoryFeatures = [];
      publicHostKey = toBase64
        (builtins.readFile /etc/nix/builder_ed25519.pub)
      ;
    }
  ];
  nix.distributedBuilds = true;
  # Optional.  Useful when the builder has a faster internet connection than
  # yours.
  # nix.extraOptions = ''
  #   builders-use-substitutes = true
  # '';
}
