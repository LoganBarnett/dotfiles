{ config, flake-inputs, lib, nixpkgs, pkgs, ... }: {
  # nix.buildMachines = lib.mkForce [
  #   (config.nix.buildMachines[0] // {
  #     publicHostKey = builtins.readFile ./secrets/builder-key.pub;
  #   })
  # ];
  # Does not need to be set because linux-builder sets this itself and will be
  # less error prone.
  # buildMachines = [
  #   {
  #   hostName = "linux-builder";
  #   systems = [
  #     "aarch64-linux"
  #     "x86_64-linux"
  #   ];
  # }
  # ];
  nix.distributedBuilds = true;
  nix.linux-builder = {
    # Set this to true and re-run nix-darwin-switch if you need a setting from
    # virtualisation (such as diskSize) to stick.  Be warned that this will wipe
    # the filesystem and even repartition it.  The build speed for this can be
    # pretty quick.  Afterwards, put this back to false.  State will accumulate
    # rapidly in the store if left on, potentially.
    ephemeral = false;
    # It is not enough to simply flip this on.  See instructions in
    # ./darwin-linux-builder-vm.nix for how to properly get this working.  This
    # can appear to be an issue if it's run on a fresh system, or the system
    # recently got garbage collected.
    # Sometimes toggling this value isn't enough.  This is when one should
    # disable the VM here at this setting, and switch to apply it, then:
    # `sudo rm -f # /var/lib/darwin-builder/nixos.qcow2`
    # `nix run nixpkgs#darwin.linux-builder`
    # `shutdown now` (inside the VM)
    # and enable this once again.
    # These steps are also helpful when the builder is lodged somehow (such as
    # not responding over SSH, despite holding the port open and negotiating a
    # TCP connection).
    # Per this comment, additional troubleshooting could be done via this
    # configuration, which I have yet to try:
    # https://github.com/LnL7/nix-darwin/issues/913#issuecomment-2081538333
    # ```
    # launchd.daemons.linux-builder = {
    #   serviceConfig = {
    #     StandardOutPath = "/var/log/darwin-builder.log";
    #     StandardErrorPath = "/var/log/darwin-builder.log";
    #   };
    # };
    # ```
    enable = true;
    # callPackage might seem a better use here, but I run into this error when
    # using it:
    #  error: The option `override' does not exist. Definition values:
    #  - In `/nix/store/dzns6ry4dmfa3x9fawg7308vyzngk4ab-source/nix/darwin-linux-builder-module.nix, via option nix.linux-builder.config': <function, args: {flake-inputs, lib, nixpkgs, pkgs}>
    config = (import ./darwin-linux-builder-vm.nix {
      inherit flake-inputs;
      inherit lib;
      inherit nixpkgs;
      inherit pkgs;
    });
    package = pkgs.darwin.linux-builder;
    protocol = "ssh-ng";
    systems = [
      # "i686-linux"
      # "x86_64-linux"
      "aarch64-linux"
      "armv7l-linux"
      "armv6l-linux"
    ];
  };
}
