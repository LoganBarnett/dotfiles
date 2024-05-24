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
    #
    # This might be outdated:
    # I am seeing the same problem shown here:
    # https://github.com/NixOS/nixpkgs/issues/257686
    # I have confirmed that my nixpkgs branch includes this, but perhaps is a
    # problem because I'm not actually on nixpkgs#master where this would have
    # been built by the build system (hydra?).
    enable = false;
    config = (import ./darwin-linux-builder-vm.nix {
      inherit lib;
      inherit nixpkgs;
    });
    protocol = "ssh-ng";
    systems = [
      # "i686-linux"
      # "x86_64-linux"
      "aarch64-linux"
    ];
  };
}
