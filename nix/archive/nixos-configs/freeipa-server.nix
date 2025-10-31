{ host-id }: { ... }: {
  age.secrets.proton-ipa-certificate = {
    rekeyFile = ../secrets/proton-ipa-certificate.age;
    # This will appear identical to the HTTPS certificate for the domain, but we
    # want a separate one for IPA's needs.
    settings = {
      tls = {
        domain = "proton";
        subject = {
          country = "US";
          state = "Oregon";
          location = "Portland";
          organization = "Barnett family";
          organizational-unit = "IT Department";
        };
        validity = 365 * 5;
      };
    };
    generator.script = "tls-ca-root";
  };
  security.ipa = {
    server.enable = true;
    ipaHostname = host-id;
    certificate = ""; # TODO: Issue secret.
    realm = "PROTON";
    basedn = "dc=proton,dc=org";
  };
}
