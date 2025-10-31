################################################################################
# Home Assistant coordinates a variety of IoT (Internet of Things) devices.
################################################################################
{ config, host-id, lib, pkgs, ... }: let
  # You'd think it would be in the settings, but it's encoded in the
  # settings.address field.  Impossible to retrieve dynamically.
  authelia-port = 9091;
  # anonymous-user-id = "35646913-ae33-4de2-8918-366f2078097d";
  # I pulled this value from the example here so it will probably work:
  # https://www.home-assistant.io/docs/authentication/providers/
  anonymous-user-id = "acbbff56461748718f3650fb914b88c9";
in {
  disabledModules = [ "services/home-automation/home-assistant.nix" ];
  age.secrets = {
    "${host-id}-home-assistant-elevation" = {
      rekeyFile = ../secrets/${host-id}-home-assistant-elevation.age;
      settings = {
        fieldName = "home-assistant-elevation";
      };
    };
    "${host-id}-home-assistant-latitude" = {
      rekeyFile = ../secrets/${host-id}-home-assistant-latitude.age;
      settings = {
        fieldName = "home-assistant-latitude";
      };
    };
    "${host-id}-home-assistant-longitude" = {
      rekeyFile = ../secrets/${host-id}-home-assistant-longitude.age;
      settings = {
        fieldName = "home-assistant-longitude";
      };
    };
    home-assistant-secrets = {
      generator = {
        script = "yaml-secret-file";
        dependencies = [
          config.age.secrets.home-assistant-client-secret
          config.age.secrets."${host-id}-home-assistant-elevation"
          config.age.secrets."${host-id}-home-assistant-latitude"
          config.age.secrets."${host-id}-home-assistant-longitude"
          config.age.secrets.mosquitto-home-assistant-password
        ];
      };
      rekeyFile = ../secrets/home-assistant-secrets.age;
    };
    # "${host-id}-authelia-home-assistant-ldap-password-environment-file" = {
    #   generator = {
    #     dependencies = [
    #       config.age.secrets."${host-id}-authelia-home-assistant-matrix-service-ldap-password"
    #     ];
    #     script = "environment-variable";
    #   };
    #   settings.field = "LDAP_BIND_PASSWORD";
    #   rekeyFile = ../secrets/${host-id}-authelia-home-assistant-service-ldap-password-environment-file.age;
    # };
    # "${host-id}-authelia-home-assistant-ldap-password-environment-file-aggregate" = {
    #   generator = {
    #     dependencies = [
    #       config.age.secrets."${host-id}-authelia-home-assistant-matrix-service-ldap-password-environment-file"
    #     ];
    #     script = "environment-variable";
    #   };
    #   rekeyFile = ../secrets/${host-id}-authelia-home-assistant-service-environment-file-aggregate.age;
    # };
    "${host-id}-authelia-home-assistant-session-secret" = {
      generator.script = "long-passphrase";
      rekeyFile = ../secrets/${host-id}-authelia-home-assistant-session-secret.age;
    };
    "${host-id}-authelia-home-assistant-jwt-secret" = {
      generator.script = "long-passphrase";
      rekeyFile = ../secrets/${host-id}-authelia-home-assistant-jwt-secret.age;
    };
    "${host-id}-authelia-home-assistant-storage-key" = {
      generator.script = "long-passphrase";
      rekeyFile = ../secrets/${host-id}-authelia-home-assistant-storage-key.age;
    };
  }
  // config.lib.ldap.ldap-password
    "authelia-home-assistant"
    "${host-id}-authelia-home-assistant-service"
  ;
  imports = [
    ./mosquitto-secrets.nix
    ../nixos-modules/home-assistant.nix
    ./home-assistant-secret-file.nix
    (import ../nixos-modules/https.nix {
      # The auth_request setting prevents this from going straight to
      # home-assistant if there's no valid auth header in place.
      server-port = config.services.home-assistant.config.http.server_port;
      # Actually just redirect to Authelia for authentication.
      # server-port = authelia-port;
      inherit host-id;
      fqdn = "home-assistant.proton";
      # redirect = false;
    })
  ];
  systemd.services.home-assistant = {
    serviceConfig = {
      LoadCredential = [
        "home-assistant-secrets:${config.age.secrets.home-assistant-secrets.path}"
      ];
    };
  };
  systemd.services.authelia-home-assistant = {
    serviceConfig = let
      credentials = [
        # See
        # https://www.authelia.com/configuration/methods/secrets/#environment-variables
        # for how the patterning works for these environment variables.
        # Ideally, we'd just do this via the instance's secrets, but it's a bit
        # of a fixed structure right now.
        "AUTHELIA_AUTHENTICATION_BACKEND_LDAP_PASSWORD_FILE:${
          config
            .age
            .secrets
            ."${host-id}-authelia-home-assistant-service-ldap-password"
            .path
        }"
        "AUTHELIA_STORAGE_ENCRYPTION_KEY_FILE:${
          config
            .age
            .secrets
            ."${host-id}-authelia-home-assistant-storage-key"
            .path
        }"
        "AUTHELIA_IDENTITY_VALIDATION_RESET_PASSWORD_JWT_SECRET_FILE:${
          config
            .age
            .secrets
            ."${host-id}-authelia-home-assistant-jwt-secret"
            .path
        }"
        "AUTHELIA_SESSION_SECRET_FILE:${
          config
            .age
            .secrets
            ."${host-id}-authelia-home-assistant-session-secret"
            .path
        }"
      ];
    in {
      # EnvironmentFile = [
      #   config.age.secrets."${host-id}-authelia-home-assistant-service-environment-file"
      # ];
      Environment = builtins.map
        (cred-pair: let
          creds = lib.strings.splitString ":" cred-pair;
          var = builtins.elemAt creds 0;
        in
          "${var}=/run/credentials/%n/${var}"
        )
        credentials
        ;
      LoadCredential = credentials;
    };
  };
  services.authelia.instances.home-assistant = {
    enable = true;
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
    #     ."${host-id}-authelia-home-assistant-storage-key"
    #     .path;
    #   jwtSecretFile = config
    #     .age
    #     .secrets
    #     ."${host-id}-authelia-home-assistant-jwt-secret"
    #     .path;
    #   sessionSecretFile = config
    #     .age
    #     .secrets
    #     ."${host-id}-authelia-home-assistant-session-secret"
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
          groups_filter = lib.strings.concatStrings [
            "(&(|(objectclass=groupOfNames))"
            "(|(cn=home-assistant-admins)(cn=home-assistant-users)))"
          ];
          attributes = {
            username = "uid";
            mail = "mail";
            display_name = "cn";
            group_name = "cn";
            member_of = "memberOf";
          };
          user = "uid=${host-id}-authelia-home-assistant-service,ou=users,dc=proton,dc=org";
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
          authelia_url = "https://auth.home-assistant.proton/login/";
          default_redirection_url = "https://home-assistant.proton";
          domain = "home-assistant.proton";
        }];
      };
      storage = {
        local = {
          path = ":memory:";
        };
      };
    };
  };
  services.nginx = {
    logError = "stderr debug";
    virtualHosts."home-assistant.proton" = {
      # It's hard to find good examples.  This one inspired much of the config
      # below: https://gist.github.com/userdocs/7634b8a57e803e378b09c18225edd446
      locations."/" = {
        # proxyPass = "http://127.0.0.1:${toString config.services.home-assistant.port}";
        extraConfig = ''
          # Send a subsequent request to Authelia to verify if the user is
          # authenticated and has the right permissions to access the resource.
          auth_request /login/api/verify;
          # Set the $(target_url) variable based on the request. It will be used
          # to build the portal URL with the correct redirection parameter.
          auth_request_set $target_url $scheme://$http_host$request_uri;
          # If Authelia returns 401, then nginx redirects the user to the login
          # portal.  If it returns 200, then the request pass through to the
          # backend.  For other type of errors, nginx will handle them as usual.
          error_page 401 =302 https://$http_host/login/?rd=$target_url;
          auth_request_set $user $upstream_http_remote_user;
          auth_request_set $groups $upstream_http_remote_groups;
          auth_request_set $name $upstream_http_remote_name;
          auth_request_set $email $upstream_http_remote_email;
          proxy_set_header Remote-User $user;
          proxy_set_header Remote-Groups $groups;
          proxy_set_header Remote-Name $name;
          proxy_set_header Remote-Email $email;
          proxy_set_header X-Forwarded-User $remote_user;
          proxy_set_header X-Real-IP $remote_addr;
          # proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Preferred-Username $remote_user;
          # proxy_set_header X-Forwarded-Proto $scheme;
        '';
      };
      locations."/login" = {
        proxyPass = "http://127.0.0.1:${toString authelia-port}/";
        extraConfig = ''
          internal;
          proxy_set_header Content-Length "";
          proxy_set_header X-Original-URL $scheme://$http_host$request_uri;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-Preferred-Username $remote_user;
          proxy_set_header X-Forwarded-Proto $scheme;

          client_body_buffer_size 128k;
          #Timeout if the real server is dead
          proxy_next_upstream error timeout invalid_header http_500 http_502 http_503;
          # Advanced Proxy Config
          send_timeout 5m;
          proxy_read_timeout 360;
          proxy_send_timeout 360;
          proxy_connect_timeout 360;
          # Basic Proxy Config
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
          proxy_set_header X-Forwarded-Host $http_host;
          # proxy_set_header X-Forwarded-Uri $request_uri;
          proxy_set_header X-Forwarded-Uri /login/;
          proxy_set_header X-Forwarded-Prefix /login;
          proxy_set_header X-Forwarded-Ssl on;
          proxy_redirect  http://  $scheme://;
          proxy_http_version 1.1;
          proxy_set_header Connection "";
          proxy_cache_bypass $cookie_session;
          proxy_no_cache $cookie_session;
          proxy_buffers 64 256k;
          # If behind reverse proxy, forwards the correct IP
          set_real_ip_from 10.0.0.0/8;
          set_real_ip_from 172.16.0.0/12;
          set_real_ip_from 192.168.0.0/16;
          set_real_ip_from fc00::/7;
          real_ip_header X-Forwarded-For;
          real_ip_recursive on;
        '';
      };
      locations."/login/" = {
        proxyPass = "http://127.0.0.1:${toString authelia-port}";
      };
      locations."/login/api/verify" = {
        proxyPass = "http://127.0.0.1:9091/api/verify";
        extraConfig = ''
          internal;
          proxy_pass_request_body off;
          proxy_set_header Content-Length "";
          # [REQUIRED] Needed by Authelia to check authorizations of the resource.
          # Provide either X-Original-URL and X-Forwarded-Proto or
          # X-Forwarded-Proto, X-Forwarded-Host and X-Forwarded-Uri or both.
          # Those headers will be used by Authelia to deduce the target url of the user.
          #
          # Basic Proxy Config
          proxy_set_header X-Original-URL $scheme://$http_host$request_uri;
          proxy_set_header X-Forwarded-Method $request_method;

          client_body_buffer_size 128k;
          #Timeout if the real server is dead
          proxy_next_upstream error timeout invalid_header http_500 http_502 http_503;
          # Advanced Proxy Config
          send_timeout 5m;
          proxy_read_timeout 360;
          proxy_send_timeout 360;
          proxy_connect_timeout 360;
          # Basic Proxy Config
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
          proxy_set_header X-Forwarded-Host $http_host;
          proxy_set_header X-Forwarded-Uri $request_uri;
          proxy_set_header X-Forwarded-Ssl on;
          proxy_redirect  http://  $scheme://;
          proxy_http_version 1.1;
          proxy_set_header Connection "";
          proxy_cache_bypass $cookie_session;
          proxy_no_cache $cookie_session;
          proxy_buffers 64 256k;
          # If behind reverse proxy, forwards the correct IP
          set_real_ip_from 10.0.0.0/8;
          set_real_ip_from 172.16.0.0/12;
          set_real_ip_from 192.168.0.0/16;
          set_real_ip_from fc00::/7;
          real_ip_header X-Forwarded-For;
          real_ip_recursive on;
        '';
      };
    };
  };
  services.home-assistant = {
    enable = true;
    extraComponents = [
      "auth"
      "frontend"
      "http"
      "isal"
      "mqtt"
    ];
    extraPackages = ps: [
      ps.roombapy
    ];
    # This is a dead end.  I can't build hefty Rust applications on bogged down
    # Raspberry Pis, essentially.
    # extraPackages = ps: [
    #   (ps.buildPythonPackage (let
    #     version = "v1.12";
    #   in {
    #     pname = "hass-auth-header";
    #     inherit version;
    #     format = "poetry";
    #     src = pkgs.fetchFromGitHub {
    #       owner = "BeryJu";
    #       repo = "hass-auth-header";
    #       rev = version;
    #       hash = "sha256-BPG/G6IM95g9ip2OsPmcAebi2ZvKHUpFzV4oquOFLPM=";
    #     };
    #     nativeBuildInputs = [
    #       pkgs.black
    #       pkgs.isort
    #       (pkgs.ruff.overrideAttrs (old: let
    #         version = "v0.6.2";
    #       in {
    #         inherit version;
    #         src = old.src // {
    #           rev = version;
    #           hash = "";
    #         };
    #       }))
    #     ];
    #     # propagatedBuildInputs = [
    #     #   pkgs.isort
    #     # ];
    #   }))
    # ];
    onboardingComplete = true;
    # Derp - it's just hardcoded at the moment in the module.  Gotta fix that!
    staticUsers = {};
    secretsFile = "/run/credentials/home-assistant.service/home-assistant-secrets";
    config = {
      homeassistant = {
        # This section is required for bypassing the onboarding mode.
        name = "${host-id}-home-assistant";
        elevation = "!secret ${host-id}-home-assistant-elevation";
        latitude = "!secret ${host-id}-home-assistant-latitude";
        longitude = "!secret ${host-id}-home-assistant-longitude";
        unit_system = "metric";
        temperature_unit = "C";
        time_zone = "America/Los_Angeles";
      } // {
        auth_providers = let
          trusted_networks = [
            "0.0.0.0/0"
            "::/0"
          ];
        in [
          # Authelia provides the authentication, and this basically disables
          # users in Home Assistant itself.  See what you made me do?
          {
            allow_bypass_login = true;
            type = "trusted_networks";
            inherit trusted_networks;
            # I can't get this to validate even using the user ID in their
            # example, which admittedly might be contrived.  It won't accept an
            # arbitrary string either, nor a valid UUID.  Something with the
            # structure might be off, but near as I can tell this is correct.
            # Instead, the system allows me to omit this if there is one and
            # only one user.
            trusted_users =
              builtins.listToAttrs
                (
                  # Also surround with double quotes, because we're not actually
                  # emitting YAML for the configuration so much as just building
                  # a big YAML string.  So everything must be escaped manually.
                  builtins.map
                    (x: {
                      name = x;
                      value = anonymous-user-id;
                    })
                    trusted_networks
                )
            ;
          }
          {
            type = "homeassistant";
          }
        ];
        # auth_providers = [
        #   {
        #     type = "header";
        #     header = "X-Forwarded-Preferred-Username";
        #   }
          # {
          #   type = "auth_header";
          # }
          # I guess this only exists in forks?
          # {
          #   type = "oidc";
          #   id = "oidc";
          #   client_id = "home-assistant";
          #   client_secret = "!secret home-assistant-client-secret";
          #   callback_url = "https://home-assistant.proton/auth/external/callback";
          #   scopes = [
          #     "openid"
          #     "email"
          #     "profile"
          #   ];
          # }
        # ];
      };
      auth_header = {};
      default_config = {};
      http = {
        trusted_proxies = [
          "127.0.0.1"
          "::1"
          "0.0.0.0"
          "::/0"
        ];
        use_x_forwarded_for = true;
      };
      mqtt = [
        {
        broker = "mosquitto.proton";
        username = "home-assistant";
        certificate = "auto";
        password = "!secret mosquitto-home-assistant-password";
        # This is sort of what "activates" the integration in the UI.
        discovery = true;
        }
      ];
      # Yep, just leave it empty.  I found out this is Because No Reasons.
      zwave_js = {
        # I can't find a way to make this work.
        # LOL just leave this out I guess?
        # url = "ws://localhost:${toString config.services.zwave-js.port}";
      };
    };
  };
}
