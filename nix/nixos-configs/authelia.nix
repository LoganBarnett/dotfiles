{ config, host-id, lib, pkgs, ... }: {
  services.authelia.instances.auth = {
    enable = true;
    # I don't recall why I pinned this to a more current version but let's leave
    # it for now.
    package = pkgs.authelia.overrideAttrs (old: let
      version = "4.39.4";
    in {
      inherit version;
      src = pkgs.fetchFromGitHub {
        owner = "authelia";
        repo = "authelia";
        rev = "v${version}";
        hash = "sha256-OIf7Q84uWk2q+lTBQNHHO11QEl7FBGv2uNx+g2GNHE0=";
      };
      vendorHash = "sha256-Vndkts5e3NSdtTk3rVZSjfuGuafQ3eswoSLLFspXTIw=";
      pnpmDepsHash = "sha256-hA9STLJbFw5pFHx2Wi3X6JFsTvHzCMFVS3HEJApQ9zM=";
    });
    # We're not really going manual.  We're using LoadCredential so we don't
    # need to manage permissions to the secret files.  It also keeps things
    # consistent with the rest of the secrets we're using.
    secrets.manual = true;
    # secrets = {
    #   storageEncryptionKeyFile = config
    #     .age
    #     .secrets
    #     ."${host-id}-authelia-storage-key"
    #     .path;
    #   jwtSecretFile = config
    #     .age
    #     .secrets
    #     ."${host-id}-authelia-jwt-secret"
    #     .path;
    #   sessionSecretFile = config
    #     .age
    #     .secrets
    #     ."${host-id}-authelia-session-secret"
    #     .path;
    # };
    # See https://github.com/authelia/authelia/blob/master/config.template.yml
    # for how this configuration should look.
    settings = {
      theme = "dark";
      authentication_backend = {
        ldap = {
          # We have to keep using nickel.proton because currently that's the
          # ceriticate it offers.
          address = "ldaps://nickel.proton";
          timeout = "20 seconds";
          start_tls = false;
          base_dn = "dc=proton,dc=org";
          additional_users_dn = "ou=users";
          additional_groups_dn = "ou=groups";
          users_filter = "(&(|({username_attribute}={input})({mail_attribute}={input})))";
          # Not needed - see access_control section in this file instead.
          # groups_filter = lib.strings.concatStrings [
          #   "(&(|(objectclass=groupOfNames))"
          #   "(|(cn=home-assistant-admins)(cn=home-assistant-users)))"
          # ];
          attributes = {
            username = "uid";
            mail = "mail";
            display_name = "cn";
            group_name = "cn";
            member_of = "memberOf";
          };
          user = "uid=${host-id}-authelia-service,ou=users,dc=proton,dc=org";
        };
        password_reset.disable = true;
      };
      access_control = {
        default_policy = "deny";
        rules = [
          {
            domain = "home-assistant.proton";
            subject = [
              "group:home-assistant-admins"
              "group:home-assistant-users"
            ];
            policy = "one_factor";
          }
          {
            domain = "gitea.proton";
            subject = [
              "group:gitea-admins"
              "group:gitea-users"
            ];
            policy = "one_factor";
          }
        ];
      };
      notifier = {
        # The docs warn not to do this, but I don't see a need for this and it
        # significantly complicates my configuration.
        disable_startup_check = true;
        filesystem = {
          # Hur hur this will never exist but the configuration validation
          # insists we have it anyways.
          filename = "/var/lib/authelia/notification.txt";
        };
      };
      server = {
        address = "tcp://127.0.0.1:9091/login";
      };
      session = {
        cookies = [{
          authelia_url = "https://auth.proton/login/";
          # This shouldn't ever be needed, but just in case.
          default_redirection_url = "https://nextcloud.proton";
          domain = "auth.proton";
        }];
      };
      storage = {
        local = {
          path = ":memory:";
        };
      };
    };
  };
}
