################################################################################
# Make this host consume our various builders.
################################################################################
{
  config,
  host-id,
  lib,
  pkgs,
  ...
}:
let
  # sshKey = ../secrets/builder-key-blue.pub;
  # Use the agenix-managed secret path directly.  This avoids conflicts with
  # nix-darwin's environment.etc management.
  sshKey = config.age.secrets.builder-key-blue.path;
  sshUser = "builder";
  toBase64 = (pkgs.callPackage ../base64.nix { }).toBase64;
  facts = import ../nixos-modules/facts.nix;

  # Check if a builder's hostname matches this host or any of its aliases.
  # This prevents self-loop deadlocks where a host tries to build on itself.
  isCurrentHost =
    builderHostName:
    let
      # Get all names this host is known by (hostname + aliases).
      hostAliases = [ host-id ] ++ (facts.network.hosts.${host-id}.aliases or [ ]);
      # Build FQDNs for all aliases.
      hostFqdns = map (alias: "${alias}.${facts.network.domain}") hostAliases;
    in
    lib.elem builderHostName hostFqdns;

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
    supportedFeatures = [
      "benchmark"
      "big-parallel"
      "kvm"
    ];
    mandatoryFeatures = [ ];
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
    supportedFeatures = [
      "benchmark"
      "big-parallel"
      "kvm"
    ];
    mandatoryFeatures = [ ];
  };
in
{
  # Exclude builders from the list if they refer to the current host.  This
  # prevents self-loop deadlocks where a host tries to build on itself via SSH,
  # creating circular dependencies that hang builds indefinitely.
  # Check both the hostname and any aliases defined in facts.nix.
  nix.buildMachines = lib.filter (builder: !(isCurrentHost builder.hostName)) [
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
      hostNames = [
        "rpi-build.proton"
        "cobalt.proton"
      ];
      publicKeyFile = ../secrets/cobalt-pub-key.pub;
    };
    "silicon.proton" = {
      hostNames = [ "silicon.proton" ];
      publicKeyFile = ../secrets/silicon-pub-key.pub;
    };
  };

  # This demonstrates that Nix doesn't actually use the data found in
  # /etc/nix/machines but instead relies upon SSH configuration.  Documentation
  # should be addressed.
  # environment.etc."ssh/ssh_config.d/101-lithium.conf".text = ''
  #   Host lithium.proton
  #     HostName lithium.proton
  #     User builder
  #     IdentityFile ${config.age.secrets.builder-key-blue.path}
  # '';

  environment.etc."nix/builder_ed25519.pub".text =
    builtins.readFile ../secrets/builder-key-blue.pub;
}
