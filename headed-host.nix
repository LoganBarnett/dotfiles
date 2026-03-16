{ config, flake-inputs, lib, pkgs, ... }: {
  imports = [
    flake-inputs.emacs-config.darwinModules.default
    ({ pkgs, ... }: {
      environment.systemPackages =
        pkgs.callPackage ./general-packages.nix {};
    })
  ];
}
