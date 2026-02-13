################################################################################
# Gitea (pronounced "git-tee") is a git collaboration server.  Much like GitHub,
# it offers pull requests, diff viewing, build triggers, and merge checks.
#
# Gitea is favorable to me over GitLab because it is lighter weight and more
# dedicated to the singular role of being a git collaboration server, whereas
# GitLab requires many more resources and revolves around a kind of DevOps
# management landscape.
################################################################################
{ config, host-id, lib, pkgs, ... }: let
  service = "gitea";
  domain = "${service}.proton";
  dataDir = "/mnt/gitea";
  ldap-enabled = true;
  ldapServiceUser = "${host-id}-${service}-service";
in {
  imports = [
    ../nixos-modules/bindfs.nix
    ../nixos-modules/https-module.nix
    ../nixos-modules/nfs-mount-consumer.nix
  ];
  age.secrets = {
    # Other secrets go in here.
  } // (
    if ldap-enabled
    then config.lib.ldap.ldap-password
      service
      ldapServiceUser
    else {}
  );
  nfsConsumerFacts = {
    enable = true;
    provider = {
      remoteHost = "silicon.proton";
      vpnHost = "silicon-nas.proton";
      providerHostId = "silicon";
      wgPort = 51821;
    };
  };
  services.https.fqdns."gitea.proton" = {
    enable = true;
    internalPort = config.services.gitea.settings.server.HTTP_PORT;
  };
  services.gitea = {
    enable = true;
    database = {
      type = "postgres";
    };
    settings = {
      server = {
        # Use Gitea's built-in SSH server on port 2222 since the host
        # already uses port 22 for system SSH.
        SSH_PORT = 2222;
        START_SSH_SERVER = true;
        DISABLE_SSH = false;
        # Use standard 'git' username for SSH operations instead of 'gitea'.
        # BUILTIN_SSH_SERVER_USER controls what username the SSH server
        # accepts, while SSH_USER only controls what's displayed in clone
        # URLs.
        BUILTIN_SSH_SERVER_USER = "git";
      };
      service = {
        DISABLE_REGISTRATION = true;
        DOMAIN = domain;
        # This defaults to using HTTP.  Probably because the defaults don't
        # expect an easy HTTPS reverse proxy configuration.
        ROOT_URL = "https://${domain}/";
      };
      # oauth2_client = {
      #   ENABLE_AUTO_REGISTRATION = true;
      # };
      # "openid authelia" = {
      #   ENABLE = true;
      #   NAME = "Authelia";
      #   PROVIDER = "openidConnect";
      #   CLIENT_ID = "gitea";
      #   CLIENT_SECRET = "your-client-secret";
      #   OPENID_CONNECT_AUTO_DISCOVER_URL = "https://auth.proton/.well-known/openid-configuration";
      #   SCOPES = "openid profile email";
      # };
      "auth.ldap" =
        if ldap-enabled
        then
          let
            base-dn = "dc=proton,dc=org";
          in {
            ENABLED = true;
            NAME = "LDAP";
            HOST = "nickel.proton";
            PORT = 636;
            USE_SSL = true;
            START_TLS = false;
            BIND_DN = "uid=${host-id}-${service}-service,ou=users,${base-dn}";
            BIND_PASSWORD = "@${
              config
                .age
                .secrets
                ."${ldapServiceUser}-ldap-password"
                .path
            }";
            USER_BASE = "ou=users,${base-dn}";
            USER_FILTER = "(&(memberOf=cn=gitea-users,ou=groups,${base-dn})(objectClass=person)(uid=%s))";
            ADMIN_FILTER = "(memberOf=cn=gitea-admins,ou=groups,${base-dn})";
            ATTRIBUTE_USERNAME = "uid";
            ATTRIBUTE_NAME = "cn";
            ATTRIBUTE_SURNAME = "sn";
            ATTRIBUTE_MAIL = "mail";
            IS_ACTIVE = true;
            ALLOW_DEACTIVATION = false;
            SKIP_TLS_VERIFY = false;
          }
        else
          {}
      ;
    };
    stateDir = dataDir;
  };
  # Goss health checks for Gitea.
  services.goss.checks = {
    # Check that the HTTPS endpoint is responding.
    http."https://${domain}" = {
      status = 200;
      timeout = 5000;
      body = [ "Gitea" ];
    };
    # Check that the API endpoint works.
    http."https://${domain}/api/v1/version" = {
      status = 200;
      timeout = 3000;
      headers = [ "Content-Type: application/json" ];
    };
    # Check that SSH port is listening.  Gitea binds to :: (IPv6 any)
    # which also accepts IPv4 connections via IPv4-mapped IPv6 addresses.
    port."tcp6:2222" = {
      listening = true;
    };
    # Check that HTTPS port is listening (handled by reverse proxy).
    port."tcp:443" = {
      listening = true;
      ip = [ "0.0.0.0" ];
    };
    # Check that the gitea service is running.
    service.gitea = {
      enabled = true;
      running = true;
    };
    # Check that PostgreSQL is running.  We don't check "enabled" because
    # PostgreSQL is started as a dependency of gitea and shows as "linked"
    # rather than "enabled" in systemd.
    service.postgresql = {
      running = true;
    };
  };
  systemd.services.gitea = let
    after = [
      "postgresql.service"
      "run-agenix.d.mount"
    ];
  in {
    inherit after;
    requires = after;
    serviceConfig = {
      # LoadCredential = [];
      # Environment = [];
    };
  };
  ##############################################################################
  # Gitea lacks the ability to have a purely configuration driven authentication
  # solution.  There have to be modifications made to the database.  This
  # service handles this for us.  We could perhaps consolidate some of these
  # settings with the actual on-disk configuration above, but for now it seems
  # to work so let's leave it be.
  #
  # Note that if the credentials change, this might need some manual work to
  # repair.  In addition, this could mean a manual restart is needed of gitea
  # itself.  Though I feel like if this is connected to the other, it should
  # restart automatically.  But I don't know.
  ##############################################################################
  systemd.services.gitea-ldap-setup = lib.mkIf ldap-enabled {
    description = "Configure Gitea LDAP authentication source";
    after = [ "gitea.service" ];
    wants = [ "gitea.service" ];
    wantedBy = [ "multi-user.target" ];
    # Provide awk and grep for the script.
    path = [ pkgs.gawk pkgs.gnugrep ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = "gitea";
      ExecStartPost = "!${pkgs.systemd}/bin/systemctl restart gitea.service";
    };
    script = let
      base-dn = "dc=proton,dc=org";
      giteaBin = "${config.services.gitea.package}/bin/gitea";
      appIni = "/mnt/gitea/custom/conf/app.ini";
    in ''
      # Check if LDAP auth source already exists.
      AUTH_ID=$(${giteaBin} --config ${appIni} admin auth list | grep "LDAP" | awk '{print $1}')

      if [ -n "$AUTH_ID" ]; then
        COMMAND="update-ldap"
        ID_ARG="--id $AUTH_ID"
        ACTION="updated"
      else
        COMMAND="add-ldap"
        ID_ARG=""
        ACTION="created"
      fi

      echo "LDAP authentication source being ''${ACTION}..."
      ${giteaBin} --config ${appIni} admin auth $COMMAND \
        $ID_ARG \
        --name "LDAP" \
        --security-protocol ldaps \
        --host nickel.proton \
        --port 636 \
        --bind-dn "uid=${ldapServiceUser},ou=users,${base-dn}" \
        --bind-password "$(cat ${config.age.secrets."${ldapServiceUser}-ldap-password".path})" \
        --user-search-base "ou=users,${base-dn}" \
        --user-filter "(&(memberOf=cn=gitea-users,ou=groups,${base-dn})(objectClass=person)(uid=%s))" \
        --admin-filter "(memberOf=cn=gitea-admins,ou=groups,${base-dn})" \
        --username-attribute uid \
        --firstname-attribute cn \
        --surname-attribute sn \
        --email-attribute mail \
        --skip-tls-verify=false

      echo "LDAP authentication source ''${ACTION} successfully."
    '';
  };
  services.postgresql = {
    enable = true;
    # We have to specify a version yet we don't actually care.  This host is
    # shared with NextCloud that also needs PostgreSQL.  The NixOS options sees
    # declaring the same package twice as "differing".  For now just leave this
    # commented out until I can figure out a sane way to manage this setting.
    # package = pkgs.postgresql_16;
    ensureDatabases = [ "gitea" ];
    ensureUsers = [
      {
        # This uses peer authentication by default, which means only users of
        # matching Unix user names can connect as this user.
        name = config.services.gitea.database.user;
        ensureDBOwnership = true;
      }
    ];
  };
  # Open firewall for Gitea's SSH server.
  networking.firewall.allowedTCPPorts = [ 2222 ];
}
