################################################################################
# Populate a systemd environment file with a secret.
#
# At the moment only supports one field.
################################################################################
{ config, lib, ... }: {
  age.generators.environment-file = {
    decrypt,
    deps,
    file,
    name,
    pkgs,
    secret,
    ...
  }:
    "echo '${
      lib.escapeShellArg secret.settings.field
    }=$(cat ${
      lib.escapeShellArg (builtins.elemAt deps 0).file
    })'"
  ;
}
