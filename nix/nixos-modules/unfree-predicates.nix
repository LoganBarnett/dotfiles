##
# NixOS does not merge nixpkgs.config.allowUnfreePredicate as one might think -
# indeed, it is only a single function the merged function is just a function,
# not a list of functions to invoke.  Perhaps it should be a list.  But also,
# Nix isn't flagging it as a collision of values.  See
# https://github.com/NixOS/nixpkgs/issues/197325 for the bug report about this.
#
# This provides a configuration option in which one can specify a list of
# functions, which is adapted from the ticket (which just took regular
# expressions.
##
{ config, lib, ... }: {
  options = {
    allowUnfreePackagePredicates = lib.mkOption {
      type = lib.types.listOf (lib.types.functionTo lib.types.bool);
      default = [];
      example = lib.literalExpression ''
        [
          (pkg: builtins.elem (lib.getName pkg) [ "steam" "steam-original" ] )
        ];
      '';
    };
  };

  config = {
    nixpkgs.config.allowUnfreePredicate = lib.mkForce (
      pkg: (builtins.any (p: p pkg) config.allowUnfreePackagePredicates)
    );
  };
}
