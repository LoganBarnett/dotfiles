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
    # It is not enough to simply flip this on.  See instructions in
    # ./darwin-linux-builder-vm.nix for how to properly get this working.  This
    # can appear to be an issue if it's run on a fresh system, or the system
    # recently got garbage collected.
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
