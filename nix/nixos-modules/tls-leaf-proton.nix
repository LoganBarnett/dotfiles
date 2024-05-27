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
    # This will probably need to be revisited.  Conceivably more than one
    # service might need to use this key, and so we should probably have a group
    # that indicates access just to this one file, and then add those services
    # to that group.  For now though, just give it to nginx as it _should_ be
    # the only thing to need it.
    group = "nginx";
    mode = "0440";
    settings = {
      root-certificate = config.age.secrets.proton-ca;
      fqdn = "${host-id}.proton";
    };
    rekeyFile = ../secrets/tls-${host-id}.key.age;
  };

}
