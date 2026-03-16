{ lib, pkgs, ... }: {
  age.generators.wireguard-priv = {pkgs, file, ...}: ''
    priv=$(${pkgs.wireguard-tools}/bin/wg genkey)
    ${pkgs.wireguard-tools}/bin/wg pubkey <<< "$priv" \
      > ${lib.escapeShellArg (lib.removeSuffix ".age" file + ".pub")}
    echo -n "$priv"
  '';
}
