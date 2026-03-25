{
  config,
  facts,
  host-id,
  pkgs,
  ...
}:
let
  fqdn = "matrix.${facts.network.domain}";
in
{
  services.https.fqdns."${fqdn}" = {
    serviceNameForSocket = "matrix-synapse";
  };
  auth.ldap.users."${host-id}-matrix-service" = {
    email = "${host-id}-matrix-service@${facts.network.domain}";
    fullName = "${host-id}-matrix-service";
    description = "Matrix service account on ${host-id}.";
    group = "matrix";
  };
  auth.ldap.groups."matrix-users" = {
    description = "People who can use Matrix.";
  };
  auth.ldap.groups."matrix-admins" = {
    description = "People who can administer Matrix.";
  };
  users.groups.matrix = { };
  # Alas, Conduit doesn't support LDAP authentication yet, so no go there.
  services.matrix-synapse = {
    enable = true;
    enableRegistrationScript = false;
    plugins = with config.services.matrix-synapse.package.plugins; [
      matrix-synapse-ldap3
    ];
    settings = {
      server_name = "matrix.${facts.network.domain}";
      # Uncomment to disable the damned burst settings - it's so sensitive!  I
      # can't debug anything like this.
      # rc_login = {
      #   address.per_second = 1000;
      #   address.burst_count = 1000;
      #   account.per_second = 1000;
      #   account.burst_count = 1000;
      # };
      # Peer auth: the service runs as the unix user "matrix-synapse", which
      # matches the PostgreSQL role, so no password is required.
      database = {
        name = "psycopg2";
        args.database = "matrix-synapse";
      };
      modules = [
        {
          module = "ldap_auth_provider.LdapAuthProviderModule";
          config = {
            enable = true;
            uri = "ldaps://ldap.${facts.network.domain}";
            start_tls = false;
            base = "dc=proton,dc=org";
            bind_dn = "uid=${host-id}-matrix-service,ou=users,dc=proton,dc=org";
            bind_password_file = "/run/credentials/matrix-synapse.service/ldap-password";
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
          path = "/run/matrix-synapse/matrix-synapse.sock";
          type = "http";
          resources = [
            {
              names = [
                "client"
                "federation"
              ];
              compress = false;
            }
          ];
        }
      ];
    };
  };
  systemd.services.matrix-synapse.serviceConfig.LoadCredential = [
    "ldap-password:${
      config.age.secrets."${host-id}-matrix-service-ldap-password".path
    }"
  ];

  services.postgresql = {
    enable = true;
    # ensureDatabases does not support setting collation; database creation is
    # handled by matrix-synapse-db-init below.  See that service for the full
    # explanation of why this matters.
    ensureUsers = [
      {
        # Ownership is granted by matrix-synapse-db-init via createdb --owner.
        name = "matrix-synapse";
      }
    ];
  };

  # Why this service exists — please read before simplifying it away.
  #
  # Synapse refuses to start against a PostgreSQL database whose LC_COLLATE is
  # not 'C'.  This is not mere preference: PostgreSQL B-tree indexes store
  # values in collation-sorted order.  When the collation rules change (i.e.
  # when glibc ships an update that alters sort weights for any Unicode
  # character), the sort order the index was built with no longer matches what
  # PostgreSQL computes when searching it.  A binary search through a
  # mis-ordered index silently skips records that are physically present in the
  # table.  This is not a wrong-ordering cosmetic issue; it is invisible record
  # loss from the perspective of any query that uses that index, including
  # equality lookups on primary keys.
  #
  # The C locale uses raw byte (memcmp) ordering, which is defined by the
  # encoding and never changes regardless of what glibc does.  It is therefore
  # immune to this class of corruption.
  #
  # The typical production mitigation when running a non-C locale is to REINDEX
  # after every glibc upgrade, or to restore from dump into a freshly-initdb'd
  # cluster after a major PostgreSQL version upgrade.  Both require operator
  # awareness and manual action that is easy to forget.
  #
  # This is an active, known problem.  As of late 2025 the PostgreSQL hackers
  # list contains an open (unmerged) proposal to change initdb's default away
  # from the libc provider:
  #
  #   https://www.mail-archive.com/pgsql-hackers@lists.postgresql.org/msg209178.html
  #
  # NixOS has received multiple bug reports of PostgreSQL breaking after a
  # NixOS release upgrade due to glibc collation version bumps, and has closed
  # them without a systemic fix, leaving users to run ALTER DATABASE ... REFRESH
  # COLLATION VERSION manually:
  #
  #   https://github.com/NixOS/nixpkgs/issues/318777
  #   https://github.com/NixOS/nixpkgs/issues/361481
  #
  # ensureDatabases runs a bare CREATE DATABASE with no locale arguments, which
  # inherits the cluster's locale (en_US.UTF-8 on this host).  It provides no
  # way to override collation.  This service therefore creates the database
  # explicitly using template0 (the only template that permits a locale
  # different from the cluster default) with LC_COLLATE and LC_CTYPE both set
  # to C.  It is idempotent: it checks for existence before creating.
  systemd.services.matrix-synapse-db-init = {
    description = "Initialize matrix-synapse PostgreSQL database with C locale";
    wantedBy = [ "matrix-synapse.service" ];
    before = [ "matrix-synapse.service" ];
    after = [
      "postgresql.service"
      "matrix-synapse-postgresql-ensure-users.service"
    ];
    requires = [ "postgresql.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = "postgres";
    };
    script = ''
      if ! ${pkgs.postgresql}/bin/psql -lqt | grep -qw matrix-synapse; then
        ${pkgs.postgresql}/bin/createdb \
          --owner=matrix-synapse \
          --encoding=UTF8 \
          --lc-collate=C \
          --lc-ctype=C \
          --template=template0 \
          matrix-synapse
      fi
    '';
  };

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
    # Check that HTTPS port is listening (handled by reverse proxy).
    port."tcp:443" = {
      listening = true;
    };
    # Confirm the reverse proxy is reachable from all interfaces, not
    # only on one specific address.
    command."tcp:443-wildcard-binding" = pkgs.lib.custom.gossWildcardPortCheck 443;
    # Check that the matrix-synapse service is running.
    service.matrix-synapse = {
      enabled = true;
      running = true;
    };
  };
}
