{ pkgs, lib, ... }: {
  age.generators.htpasswd = {
    decrypt,
    deps,
    file,
    name,
    pkgs,
    secret,
    ...
  }:
    lib.flip lib.concatMapStrings deps ({ file, ... }: ''
      ${decrypt} ${lib.escapeShellArg file} \
        | ${pkgs.apacheHttpd}/bin/htpasswd -niBC 10 ${lib.escapeShellArg secret.settings.username}
    '');
}
