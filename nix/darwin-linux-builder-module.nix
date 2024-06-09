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
    # It is not enough to simply flip this on.  See instructions in
    # ./darwin-linux-builder-vm.nix for how to properly get this working.  This
    # can appear to be an issue if it's run on a fresh system, or the system
    # recently got garbage collected.
    enable = true;
    config = (import ./darwin-linux-builder-vm.nix {
      inherit lib;
      inherit nixpkgs;
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
