################################################################################
# DNS Smart Block admin password secrets.
################################################################################
{ config, ... }: {
  age.secrets = {
    dns-smart-block-admin-password = {
      generator.script = "long-passphrase";
      rekeyFile = ../secrets/dns-smart-block-admin-password.age;
      settings = {
        username = "admin";
      };
    };
    dns-smart-block-admin-htpasswd = {
      generator = {
        script = "htpasswd";
        dependencies = [
          config.age.secrets.dns-smart-block-admin-password
        ];
      };
      rekeyFile = ../secrets/dns-smart-block-admin-htpasswd.age;
      mode = "0440";
      owner = "nginx";
      group = "nginx";
    };
  };
}
