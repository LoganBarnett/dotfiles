{ lib, pkgs, ... }:
{
  age.generators.wireguard-priv =
    {
      pkgs,
      file,
      gitAdd,
      ...
    }:
    let
      pub-file = lib.escapeShellArg (lib.removeSuffix ".age" file + ".pub");
    in
    ''
      priv=$(${pkgs.wireguard-tools}/bin/wg genkey)
      ${pkgs.wireguard-tools}/bin/wg pubkey <<< "$priv" > ${pub-file}
      ${gitAdd} ${pub-file}
      echo -n "$priv"
    '';
}
