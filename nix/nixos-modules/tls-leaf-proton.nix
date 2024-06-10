################################################################################
# Ensure this host has a TLS certificate available which is tied to the internal
# CA.
################################################################################
{ host-id }: { config, pkgs, ... }: {

  age.secrets."tls-${host-id}.key" = {
    generator = {
      dependencies = [
        config.age.secrets.proton-ca
      ];
      script = "tls-signed-certificate";
    };
    group = "tls-leaf";
    mode = "0440";
    settings = {
      root-certificate = config.age.secrets.proton-ca;
      fqdn = "${host-id}.proton";
    };
    rekeyFile = ../secrets/tls-${host-id}.key.age;
  };

  users.groups = {
    tls-leaf = {};
  };

}
