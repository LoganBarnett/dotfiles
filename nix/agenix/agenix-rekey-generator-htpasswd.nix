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
    lib.strings.concatLines
      (builtins.map
        (dep: let
          username = lib.escapeShellArg (dep.settings.username or dep.name);
          password = "$(${decrypt} ${lib.escapeShellArg dep.file})";
        in ''
          echo -n "${username}:" && ${pkgs.apacheHttpd}/bin/htpasswd -nbB "" "${password}" | cut -d: -f2
        '')
        deps
      );
}
