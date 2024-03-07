{ config, lib, pkgs, ... }: {
  imports = [
    ./emacs.nix
    ({ pkgs, ... }: {
      environment.systemPackages =
        pkgs.callPackage ./general-packages.nix {};
    })
  ];
}
