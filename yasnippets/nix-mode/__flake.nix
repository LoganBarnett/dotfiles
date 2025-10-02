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

  outputs = { self, nixpkgs }@inputs: let
    systems = [
      "aarch64-darwin"
      "aarch64-linux"
      "x86_64-darwin"
      "x86_64-linux"
    ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems f;
    overlays = [
      (import rust-overlay)
    ];
    pkgsFor = system: import nixpkgs { inherit overlays system; };
    packages = (pkgs: let

    in [

    ]);
  in {

    devShells = forAllSystems (system: {
      default = (pkgsFor system).mkShell {
        buildInputs = (packages (pkgsFor system));
      };
    });

  };
}
