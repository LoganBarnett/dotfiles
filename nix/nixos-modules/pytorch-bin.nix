{ lib, pkgs, ... }: {
  imports = [
    ../nixos-modules/unfree-predicates.nix
  ];
  nixpkgs.overlays = [
    (final: prev: {
      pythonPackagesExtensions = [(py-final: py-prev: {
        torch = py-final.pytorch-bin;
      })];
    })
  ];
  allowUnfreePackagePredicates = [
    (pkg: builtins.elem (lib.getName pkg) [
      "torch"
      "triton"
    ])
  ];
  # This might need to be applied first in some scenarios.  Look to this if it's
  # not seeming to use pytorch-bin.
  nix.settings = {
    substituters = [
      "https://nix-community.cachix.org"
    ];
    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}
