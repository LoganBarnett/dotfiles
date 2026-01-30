################################################################################
# Make this host consume our various builders.
################################################################################
{ config, lib, pkgs, ... }: let
  # sshKey = ../secrets/builder-key.pub;
  # Use the agenix-managed secret path directly.  This avoids conflicts with
  # nix-darwin's environment.etc management.
  sshKey = config.age.secrets.builder-key.path;
  # This may be causing conflicts with the darwin linux-builder, since this is
  # its path.
  # sshKey = "/etc/nix/builder_ed25519";
  # Switched on 2025-08-14 for a Raspberry Pi host.  Unsure how this could
  # impact the linux-builder host (per my notes above), but something I want to
  # keep an eye out for.
  # sshKey = "/etc/nix/remote-builder_ed25519";
  sshUser = "builder";
  toBase64 = (pkgs.callPackage ../base64.nix {}).toBase64;
  rpi-systems = [
    "aarch64-linux"
    "armv6l-linux"
    "armv7l-linux"
    "arm-linux"
  ];
  rpi-build = {
    inherit sshKey;
    inherit sshUser;
    hostName = "rpi-build.proton";
    systems = rpi-systems;
    protocol = "ssh-ng";
    # Keep this host from being bogged down by builds.
    # Beware setting this to 1, as it can mean no jobs are available ever
    # (possibly due to a bug?).
    maxJobs = 2;
    # What's this for?
    speedFactor = 2;
    # Note: "kvm" means "Kernel-based Virtual Machine":
    # https://en.m.wikipedia.org/wiki/Kernel-based_Virtual_Machine
    # This is something I got from TLATER here:
    # https://discourse.nixos.org/t/kvm-is-required-error-building-a-docker-image-using-runasroot/22923/2
    # I seem to be unable to build other Raspberry Pi images without it.
    supportedFeatures = [ "benchmark" "big-parallel" "kvm" ];
    mandatoryFeatures = [];
  };
  silicon = {
    inherit sshKey;
    inherit sshUser;
    hostName = "silicon.proton";
    systems = [ "x86_64-linux" ];
    protocol = "ssh-ng";
    # Keep this host from being bogged down by builds.
    # Beware setting this to 1, as it can mean no jobs are available ever
    # (possibly due to a bug?).
    maxJobs = 2;
    # What's this for?
    speedFactor = 2;
    supportedFeatures = [ "benchmark" "big-parallel" "kvm" ];
    mandatoryFeatures = [];
  };
in {
  nix.buildMachines = [
    rpi-build
    silicon
  ];
  nix.distributedBuilds = true;
  # Optional.  Useful when the builder has a faster internet connection than
  # yours.
  # nix.extraOptions = ''
  #   builders-use-substitutes = true
  # '';

  programs.ssh.knownHosts = {
    "rpi-build.proton" = {
      # Include all hostname variations since rpi-build.proton is a CNAME for
      # cobalt.proton, and SSH may check the key against any of these names.
      hostNames = [ "rpi-build.proton" "cobalt.proton" ];
      publicKeyFile = ../secrets/cobalt-pub-key.pub;
    };
    "silicon.proton" = {
      hostNames = [ "silicon.proton" ];
      publicKeyFile = ../secrets/silicon-pub-key.pub;
    };
  };
  #   lithium = {
  #     hostNames = [ "lithium.proton" ];
  #     publicKeyFile = /etc/nix/builder_ed25519.pub;
  #   };
  # };

  # This demonstrates that Nix doesn't actually use the data found in
  # /etc/nix/machines but instead relies upon SSH configuration.  Documentation
  # should be addressed.
  # environment.etc."ssh/ssh_config.d/101-lithium.conf".text = ''
  #   Host lithium.proton
  #     HostName lithium.proton
  #     User builder
  #     IdentityFile ${config.age.secrets.builder-key.path}
  # '';

  environment.etc."nix/builder_ed25519.pub".text =
    builtins.readFile ../secrets/builder-key.pub;
}
