################################################################################
# Ensure this host has a TLS certificate available which is tied to the internal
# CA.
################################################################################
{ host-id }: { config, ... }: {

  age.secrets."tls-${host-id}" = {
    generator = {
      dependencies = [
        config.age.secrets.proton-ca
      ];
      script = "tls-signed-certificate";
    };
    settings = {
      root-certificate = config.age.secrets.proton-ca;
      fqdn = "${host-id}.proton";
    };
    rekeyFile = ../secrets/tls-${host-id}.age;
  };

}
