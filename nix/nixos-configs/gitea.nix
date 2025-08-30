################################################################################
# Gitea (pronounced "git-tee") is a git collaboration server.  Much like GitHub,
# it offers pull requests, diff viewing, build triggers, and merge checks.
#
# Gitea is favorable to me over GitLab because it is lighter weight and more
# dedicated to the singular role of being a git collaboration server, whereas
# GitLab requires many more resources and revolves around a kind of DevOps
# management landscape.
################################################################################
{ config, host-id, pkgs, ... }: let
  service = "gitea";
  domain = "${service}.proton";
  dataDir = "/mnt/gitea";
in {
  imports = [
    ../nixos-modules/nfs-mount-consumer.nix
  ];
  age.secrets = {
    gitea-nfs-wireguard-key = {
      generator.script = "wireguard-priv";
      group = "gitea";
      mode = "0440";
      rekeyFile = ../secrets/gitea-nfs-wireguard-key.age;
    };
    # Other secrets go in here.
  } // (
    config.lib.ldap.ldap-password
      "openldap-${host-id}-${service}-service"
      "${host-id}-${service}-service"
  );
  services.nfs-mount.mounts.gitea-data = {
    enable = true;
    bindToService = "gitea";
    remoteHost = "silicon.proton";
    vpnHost = "silicon-nas.proton";
    share = "/tank/data/gitea";
    mountPoint = dataDir;
    testFile = "/mnt/gitea/nfs-share-working";
    wgPrivateKeyFile = config.age.secrets.gitea-nfs-wireguard-key.path;
    wgIP = "10.100.0.4/24";
    wgPeerPublicKeyFile = ../secrets/silicon-nfs-wireguard-key.pub;
    wgPeerIP = "10.100.0.1/32";
    # Optional bindfs mapping if Gitea runs under its own user/group.
    bindfs = {
      uid = "994";
      gid = "998";
    };
  };
  services.gitea = {
    enable = true;
    database = {
      type = "postgres";
    };
    settings = {
      stateDir = dataDir;
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
      "auth.ldap" = let
        base-dn = "dc=proton,dc=org";
      in {
        ENABLED = true;
        NAME = "LDAP";
        HOST = "nickel.proton";
        PORT = 636;
        USE_SSL = true;
        START_TLS = false;
        BIND_DN = "cn=admin,${base-dn}";
        BIND_PASSWORD = "@${config.age.secrets."${host-id}-gitea-service".path}";
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
      };
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
}
