{ lib, nixpkgs, pkgs, ... }: {
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
    enable = true;
    config = (import ./darwin-linux-builder-vm.nix {
      inherit lib;
      inherit nixpkgs;
    });
    protocol = "ssh-ng";
    systems = [
      "i686-linux"
      "x86_64-linux"
      "aarch64-linux"
    ];
  };
}
