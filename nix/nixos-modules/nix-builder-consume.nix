################################################################################
# Make this host consume our various builders.
################################################################################
{ config, ... }: let
  sshKey = config.age.secrets.builder-key.path;
  sshUser = "nixremote";
in {
  nix.buildMachines = [
    {
      inherit sshKey sshUser;
      hostName = "nickel";
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
      publicHostKey = builtins.readFile ../secrets/nickel-pub-key.pub;
    }
    {
      inherit sshKey sshUser;
      hostName = "lithium";
      # Or use systems with a list.
      system = "x86_64-linux";
      protocol = "ssh-ng";
      # Keep this host from being bogged down by builds.
      maxJobs = 1;
      # What's this for?
      speedFactor = 2;
      supportedFeatures = [ "benchmark" ];
      mandatoryFeatures = [];
      publicHostKey = builtins.readFile ../secrets/lithium-pub-key.pub;
    }
  ];
  nix.distributedBuilds = true;
  # Optional.  Useful when the builder has a faster internet connection than
  # yours.
  # nix.extraOptions = ''
  #   builders-use-substitutes = true
  # '';
}
