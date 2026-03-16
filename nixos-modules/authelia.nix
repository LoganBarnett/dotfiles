################################################################################
# Authelia acts as a login portal / proxy.  We can easily integrate our existing
# SSO solutions for applications that do not support SSO, or do not support the
# types we need.
#
# This configuration assumes you're already using the ../nixos-modules/https.nix
# module.
################################################################################
{ config, lib, ... }: {
  options.authelia = {
    apps = lib.mkOption (let
      autheliaAppSubmodule = lib.types.submodule {
        options = {

        };
      };

    in {
      type = lib.attrsOf autheliaAppSubmodule;
      default = {};
    });
  };
  config = {

  };
}
