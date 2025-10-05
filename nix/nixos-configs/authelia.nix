{ config, facts, host-id, lib, pkgs, ... }: let
  inherit (lib) filter pipe;
  inherit (lib.attrsets) attrNames attrValues filterAttrs mapAttrs'
    mapAttrsToList nameValuePair
  ;
  inherit (lib.strings) concatLines replaceStrings;
  inherit (pkgs.callPackage ../lib/yaml.nix {}) indentLines;

  services = facts.network.services or {};
  users    = facts.network.users or {};

  credName = name: "${name}-oidc-client-secret";

  # mkRedirects = svc: svc.redirectUris or [
  #   "https://${svc.fqdn}/oauth2/callback"
  #   "https://${svc.fqdn}/auth/callback"
  # ];

  # mkClient = name: svc:
  #   let
  #     oidcUserName = name;
  #     hasConfidential = (users ? ${lib.escapeNixIdentifier oidcUserName})
  #                       && (users.${oidcUserName}.type or "") == "oidc-client";
  #   in
  #     {
  #       client_id = if hasConfidential then oidcUserName else name;
  #       client_name = svc.description or name;
  #       redirect_uris = mkRedirects svc;
  #       scopes = [ "openid" "profile" "email" "groups" ];
  #       authorization_policy = "one_factor"; # matches your access stance
  #       public = !hasConfidential;
  #       require_pkce = !hasConfidential;
  #     } // (if hasConfidential then {
  #       # Authelia expects a digest OR a raw secret via *_FILE env; we’ll inject
  #       # the secret with
  #       # AUTHELIA_IDENTITY_PROVIDERS_OIDC_CLIENTS_#_CLIENT_SECRET_FILE via
  #       # systemd.credentials stanza below.
  #       # Sentinel we’ll translate to env below.
  #       client_secret = "@file:${credName name}@";
  #       token_endpoint_auth_method = "client_secret_basic";
  #     } else {});

  clientYaml = ''
    identity_providers:
      oidc:
        clients:
    ''
  + (pipe oidcServices [
    (mapAttrsToList (name: svc:
    let
      oidcUserName = name;
      hasConfidential = (users ? ${oidcUserName})
                        && (users.${oidcUserName}.type or "") == "oidc-client";
    in ''
    - client_id: ${if hasConfidential then oidcUserName else name}
      client_name: ${svc.description or name}
      redirect_uris:
        - https://${svc.fqdn}/oauth2/callback
        - https://${svc.fqdn}/auth/callback
      scopes:
        - openid
        - profile
        - email
        - groups
      authorization_policy: one_factor
      public: ${toString (!hasConfidential)}
      require_pkce: ${toString (!hasConfidential)}
    '' + (
        if hasConfidential
        then ''
          client_secret: %${credName name}%
          token_endpoint_auth_method: client_secret_basic
          ''
        else ""
      )
    ))
    (builtins.map (indentLines 6))
    concatLines
  ]);

  oidcServices = filterAttrs
    (name: value: (services.${name}.authentication or null) == "oidc")
    services
  ;

  # clients = map (name: mkClient name services.${name}) (attrNames oidcServices);

in {

  age.secrets =
    (mapAttrs' (name: value: {
      name = credName name;
      value = {
        generator.script = "hex";
        settings.length = 64;
      };
    }) oidcServices)
    // {
      "${host-id}-authelia-storage-key" = {
        generator.script = "hex";
        settings.length = 64;
      };
      "${host-id}-authelia-jwt-secret" = {
        generator.script = "hex";
        settings.length = 64;
      };
      "${host-id}-authelia-session-secret" = {
        generator.script = "hex";
        settings.length = 64;
      };
      "${host-id}-authelia-oidc-secrets-config.yaml" = {
        generator = {
          script = "template-file";
          dependencies = mapAttrsToList (name: _:
            config.age.secrets."${credName name}"
          ) oidcServices;
        };
        settings.template = clientYaml;
      };
    };

  services.authelia.instances.authelia = {
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
    # secrets.manual = true;
    secrets = {
      storageEncryptionKeyFile = "/run/credentials/authelia/storage-encryption-key";
      jwtSecretFile = "/run/credentials/authelia/jwt-secret";
      sessionSecretFile = "/run/credentials/authelia/session-secret";
    };
    # See https://github.com/authelia/authelia/blob/master/config.template.yml
    # for how this configuration should look.

    settingsFiles = [
      "/run/credentials/authelia/oidc-secrets"
    ];
    settings = {
      theme = "dark";
      # authentication_backend = {
      #   ldap = {
      #     # We have to keep using nickel.proton because currently that's the
      #     # ceriticate it offers.
      #     address = "ldaps://nickel.proton";
      #     timeout = "20 seconds";
      #     start_tls = false;
      #     base_dn = "dc=proton,dc=org";
      #     additional_users_dn = "ou=users";
      #     additional_groups_dn = "ou=groups";
      #     users_filter = "(&(|({username_attribute}={input})({mail_attribute}={input})))";
      #     # Not needed - see access_control section in this file instead.
      #     # groups_filter = lib.strings.concatStrings [
      #     #   "(&(|(objectclass=groupOfNames))"
      #     #   "(|(cn=home-assistant-admins)(cn=home-assistant-users)))"
      #     # ];
      #     attributes = {
      #       username = "uid";
      #       mail = "mail";
      #       display_name = "cn";
      #       group_name = "cn";
      #       member_of = "memberOf";
      #     };
      #     user = "uid=${host-id}-authelia-service,ou=users,dc=proton,dc=org";
      #   };
      #   password_reset.disable = true;
      # };
      access_control = {
        default_policy = "deny";
        rules = mapAttrsToList (name: data: {
          domain = data.fqdn;
          subject = builtins.map (g: "group:${g}") (data.groups or []);
          policy = "one_factor";
        }) facts.network.services;
        # rules = [
        #   {
        #     domain = "home-assistant.proton";
        #     subject = [
        #       "group:home-assistant-admins"
        #       "group:home-assistant-users"
        #     ];
        #     policy = "one_factor";
        #   }
        #   {
        #     domain = "gitea.proton";
        #     subject = [
        #       "group:gitea-admins"
        #       "group:gitea-users"
        #     ];
        #     policy = "one_factor";
        #   }
        # ];
      };
      # identity_providers.oidc.clients = clients;
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
        cookies = [
          {
            authelia_url = "https://authelia.proton/login/";
            default_redirection_url = "https://authelia.proton";
            domain = "proton";
          }
          {
            authelia_url = "https://authelia.proton/login/";
            default_redirection_url = "https://authelia.proton";
            domain = "auth.proton";
          }
        ];
      };
      storage = {
        local = {
          path = ":memory:";
        };
      };
    };
  };

  # services.environment-file-secrets.services.authelia = {
  #   enable = true;
  #   secrets = (mapAttrs' (name: value: {
  #     name = credName name;
  #     value.environmentVariable = "";
  #   }) oidcServices);
  # };

  systemd.services.authelia = let
    after = [ "run-agenix.d.mount" ];
  in {
    inherit after;
    requires = after;
    serviceConfig = {
      LoadCredential = (
        map (name:
          "(${credName name}):${
            config.age.secrets."${credName name}".path
          }"
        ) (attrNames oidcServices)
      ) ++ [
        "storage-encryption-key:${
          config
            .age
            .secrets
            ."${host-id}-authelia-storage-key"
            .path
        }"
        "jwt-secret:${
          config
            .age
            .secrets
            ."${host-id}-authelia-jwt-secret"
            .path
        }"
        "session-secret:${
          config
            .age
            .secrets
            ."${host-id}-authelia-session-secret"
            .path
        }"
      ];
      # Environment = pipe oidcServices [
      #   (mapAttrs' (name: value: {
      #     name = "AUTHELIA_IDENTITY_PROVIDERS_OIDC_CLIENTS_${
      #       replaceStrings ["-"] [ "_"] name
      #     }_CLIENT_SECRET";
      #     value = "%d/${credName name}";
      #   }))
      # ];
    };
  };
}
