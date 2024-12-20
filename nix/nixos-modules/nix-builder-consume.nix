################################################################################
# Make this host consume our various builders.
################################################################################
{ config, lib, pkgs, ... }: let
  # sshKey = config.age.secrets.builder-key.path;
  # This may be causing conflicts with the darwin linux-builder, since this is
  # its path.
  sshKey = "/etc/nix/builder_ed25519";
  # sshKey = "/etc/nix/remote-builder_ed25519";
  sshUser = "builder";
  toBase64 = (pkgs.callPackage ../base64.nix {}).toBase64;
  rpi-systems = [
    "aarch64-linux"
    "armv6l-linux"
    "armv7l-linux"
    "arm-linux"
  ];
in {
  nix.buildMachines = [
    {
      inherit sshKey;
      inherit sshUser;
      hostName = "gallium.proton";
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
    }
    # {
    #   inherit sshKey;
    #   inherit sshUser;
    #   # sshKey = config.age.secrets.builder-key.path;
    #   hostName = "nickel.proton";
    #   systems = [
    #     "aarch64-linux"
    #     "armv6l-linux"
    #     "armv7l-linux"
    #     "arm-linux"
    #   ];
    #   protocol = "ssh-ng";
    #   # Keep this host from being bogged down by builds.
    #   # Beware setting this to 1, as it can mean no jobs are available ever
    #   # (possibly due to a bug?).
    #   maxJobs = 2;
    #   # What's this for?
    #   speedFactor = 2;
    #   # Note: "kvm" means "Kernel-based Virtual Machine":
    #   # https://en.m.wikipedia.org/wiki/Kernel-based_Virtual_Machine
    #   # This is something I got from TLATER here:
    #   # https://discourse.nixos.org/t/kvm-is-required-error-building-a-docker-image-using-runasroot/22923/2
    #   # I seem to be unable to build other Raspberry Pi images without it.
    #   supportedFeatures = [ "benchmark" "big-parallel" "kvm" ];
    #   mandatoryFeatures = [];
    #   # publicHostKey = toBase64
    #   #   (builtins.readFile ../secrets/builder-key.pub)
    #   # ;
    # }
    {
      inherit sshKey sshUser;
      hostName = "lithium.proton";
      # Or use systems with a list.
      systems = [
        "x86_64-linux"
        # Allows building 32-bit binaries which are sometimes needed (like
        # Steam).
        "i686-linux"
      ];
      protocol = "ssh-ng";
      # Keep this host from being bogged down by builds.
      # Beware setting this to 1, as it can mean no jobs are available ever
      # (possibly due to a bug?).
      maxJobs = 2;
      # What's this for?
      speedFactor = 2;
      supportedFeatures = [ "big-parallel" "benchmark" "kvm" ];
      mandatoryFeatures = [];
      # publicHostKey = toBase64
      #   (builtins.readFile ../secrets/lithium-pub-key.pub)
      # ;
    }
  ];
  nix.distributedBuilds = true;
  # Optional.  Useful when the builder has a faster internet connection than
  # yours.
  # nix.extraOptions = ''
  #   builders-use-substitutes = true
  # '';

  # This seems to not do anything useful.
  # programs.ssh.knownHosts = {
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

  # environment.etc."nix/remote-builder-key".text =
  #   builtins.readFile ../secrets/builder-key.age;
}
