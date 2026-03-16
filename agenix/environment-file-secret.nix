################################################################################
# Populate a systemd environment file with a secret.
#
# At the moment only supports one field.
################################################################################
{ lib, ... }: {
  # Give us a single entry for the "EnvironmentFile" form of credentials.  The
  # environment file expects a `key=value` set of lines.  I've tested this and
  # cannot query the systemd metadata to get this information - all that is
  # stored is the path to the environment file, so it should be safe to use even
  # if it is less preferable than LoadCredential.  Some services just want this
  # in environment variable form and nothing else.
  age.generators.environment-variable = {
    decrypt,
    deps,
    file,
    name,
    pkgs,
    secret,
    ...
  }: ''
    echo "${
      lib.escapeShellArg secret.settings.field
    }=$(${decrypt} ${lib.escapeShellArg (builtins.elemAt deps 0).file})"
  '';
  # Give us an aggregate of the single-entry environment-file generators.
  age.generators.environment-file = {
    decrypt,
    deps,
    file,
    name,
    pkgs,
    secret,
    ...
  }:
    lib.strings.concatStringsSep "; echo ; " (
      builtins.map (dep:
        "${decrypt} ${lib.escapeShellArg dep.file}"
      ) deps
    )
  ;
}
