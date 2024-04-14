##
# Silicon runs NextCloud.
{ config, ... }: let
  hostname = "silicon";
  system = "x86_64-linux";
in {
  imports = [
    (import ../nixos-modules/nextcloud.nix { inherit hostname; })
  ];
  age.secrets.nextcloud-logan-password = {
    rekeyFile = ../secrets/nextcloud-logan-pass.age;
    # See secrets.nix for where this is declared.
    generator = "long-passphrase";
  };
  services.nextcloud = {
    ensureUsers = {
      logan = {
        email = "logan@silicon.proton";
        passwordFile = config.age.secrets.nextcloud-logan-password.path;
      };
    };
  };
}
