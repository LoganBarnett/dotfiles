{ lib, pkgs, ... }:
{
  environment.systemPackages =
    (lib.optionals pkgs.stdenv.isLinux [
      # OpenGL ES 2.0 benchmark; generates GPU activity for diagnostics and for
      # exercising pressure detection (e.g. proc-siding).
      pkgs.glmark2
    ])
    ++ (lib.optionals pkgs.stdenv.isDarwin [
      # Metal GPU stress test via MLX matrix multiplication.
      (pkgs.callPackage ../derivations/mlx-metal-test/default.nix { })
    ]);
}
