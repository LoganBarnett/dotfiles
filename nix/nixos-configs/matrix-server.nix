{ config, host-id, pkgs, ... }: let
  fqdn = "matrix.proton";
in {
  imports = [
    (import ../nixos-modules/https.nix {
      inherit fqdn host-id;
      server-port = 8008;
    })
  ];
  age.secrets = config.lib.ldap.ldap-password
    "matrix"
    "${host-id}-matrix-service"
  ;
  users.groups.matrix = {};
  # age.secrets.matrix-synapse-postgres-password = {
  #   chmod = "0440";
  #   group = "matrix-synapse";
  #   generator.script = "long-passphrase";
  #   rekeyFile = ../secrets/matrix-synapse-postgres-password.age;
  # };
  # Alas, Conduit doesn't support LDAP authentication yet, so no go there.
  services.matrix-synapse = {
    enable = true;
    plugins = with config.services.matrix-synapse.package.plugins; [
      matrix-synapse-ldap3
    ];
    settings = {
      server_name = "matrix.proton";
      # Uncomment to disable the damned burst settings - it's so sensitive!  I
      # can't debug anything like this.
      # rc_login = {
      #   address.per_second = 1000;
      #   address.burst_count = 1000;
      #   account.per_second = 1000;
      #   account.burst_count = 1000;
      # };
      database = {
        # name = "psycopg2"; # Requires services.postgresql.enable = true;
        name = "sqlite3";
      };
      modules = [
        {
          module = "ldap_auth_provider.LdapAuthProviderModule";
          config = {
            enable = true;
            uri = "ldaps://nickel.proton";
            start_tls = false;
            base = "dc=proton,dc=org";
            bind_dn = "uid=${host-id}-matrix-service,ou=users,dc=proton,dc=org";
            bind_password_file = config
              .age
              .secrets
              ."${host-id}-matrix-service-ldap-password"
              .path
            ;
            attributes = {
              uid = "uid";
              mail = "mail";
              name = "cn";
            };
            # Only allow users who are members of matrix-users
            filter = "(&(objectClass=person)(memberOf=cn=matrix-users,ou=groups,dc=proton,dc=org))";
            # Optional: fallback for local DB auth if needed
            allowLocalPasswords = false;
            # Define who gets admin privileges
            adminFilter = "(&(objectClass=person)(memberOf=cn=matrix-admins,ou=groups,dc=proton,dc=org))";
          };
        }
      ];
      listeners = [
        {
          port = 8008;
          bind_addresses = [ "127.0.0.1" ];
          type = "http";
          tls = false;
          resources = [
            { names = [ "client" "federation" ]; compress = false; }
          ];
        }
      ];
    };
  };
  # services.postgresql = {
  #   enable = true;
  #   ensureDatabases = [ "matrix" ];
  #   ensureUsers = [
  #     {
  #       name = "matrix";
  #       ensureDBOwnership = true;
  #     }
  #   ];
  # };
  users.users.matrix-synapse.extraGroups = [ "matrix" ];
  # Goss health checks for Matrix Synapse.
  services.goss.checks = {
    # Check that the HTTPS endpoint is responding.
    http."https://${fqdn}/" = {
      status = 200;
      timeout = 5000;
    };
    # Check that the Matrix server version endpoint works.  This is part of
    # the Matrix spec and returns server version information.
    http."https://${fqdn}/_matrix/federation/v1/version" = {
      status = 200;
      timeout = 3000;
    };
    # Check that the internal matrix-synapse port is listening.  Matrix
    # Synapse listens on loopback only.
    port."tcp:8008" = {
      listening = true;
      ip = [ "127.0.0.1" ];
    };
    # Check that HTTPS port is listening (handled by reverse proxy).
    port."tcp:443" = {
      listening = true;
      ip = [ "0.0.0.0" ];
    };
    # Check that the matrix-synapse service is running.
    service.matrix-synapse = {
      enabled = true;
      running = true;
    };
  };
}
