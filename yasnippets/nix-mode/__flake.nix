# -*- mode: snippet; require-file-newline: nil -*-
# name: flake.nix
# key: flake.nix
# condition: "flake.nix"
# group: yatemplate
# Emit a Nix Flake with much of the boilerplate done.  Could use some examples
# of other entities (like nixosConfigurations, packages, darwinConfigurations,
# etc.). or an intelligent lookup of the platform name.
# --
{
  description = "";
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  };

  outputs = { self, nixpkgs }@inputs: {

    devShells.aarch64-darwin.default = let
      system = "aarch64-darwin";
      pkgs = import nixpkgs {
        inherit system;
      };
    in pkgs.mkShell {
      buildInputs = [];
    };

  };
}
