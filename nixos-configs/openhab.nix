################################################################################
# OpenHab handles controlling and automating small household devices (IoT).  It
# is a competitor to Home Assistant, and the largest pull for me is that OpenHab
# uses configuration files it wishes to keep documented, as opposed to Home
# Assistant's recent declaration of abandoning a file based configuration in
# favor of click-ops.
################################################################################
{
  config,
  facts,
  flake-inputs,
  pkgs,
  system,
  ...
}:
let
  domain = facts.network.domain;
  # Extract the host portion from the oauth2-proxy httpAddress URL so that
  # trustedNetworks stays consistent with wherever nginx proxies from.
  proxyHost = builtins.head (
    builtins.match ".*://([^:]+):.*" config.services.oauth2-proxy.httpAddress
  );
in
{
  imports = [
    flake-inputs.openhab-flake.nixosModules.openhab
  ];
  networking.dnsAliases = [ "openhab" ];
  nixpkgs.overlays = [ flake-inputs.openhab-flake.overlays.default ];

  age.secrets = {
    # Cookie signing secret for oauth2-proxy sessions on openhab.${domain}.
    # 16 bytes → 32 hex chars = 32 raw ASCII bytes, satisfying oauth2-proxy's
    # requirement of exactly 16, 24, or 32 bytes for AES cookie encryption.
    openhab-oauth2-proxy-cookie-secret = {
      generator.script = "hex";
      settings.length = 16;
    };
    # Combined environment file for oauth2-proxy sensitive options.  Substitutes
    # the OIDC client secret (declared in authelia.nix) and the cookie signing
    # secret above into a systemd EnvironmentFile.
    openhab-oauth2-proxy-env = {
      generator = {
        script = "template-file";
        dependencies = [
          config.age.secrets.openhab-oidc-client-secret
          config.age.secrets.openhab-oauth2-proxy-cookie-secret
        ];
      };
      settings.template = ''
        OAUTH2_PROXY_CLIENT_SECRET=%openhab-oidc-client-secret%
        OAUTH2_PROXY_COOKIE_SECRET=%openhab-oauth2-proxy-cookie-secret%
      '';
    };
    # Plaintext admin password used to construct the Basic auth header nginx
    # injects into every proxied request.  OpenHAB validates the header against
    # the PBKDF2 hash stored in users.json, granting administrator role so the
    # MainUI never shows the "sign in to grant admin access" prompt.
    openhab-admin-password = {
      generator.script = "hex";
      settings.length = 16;
    };
    # PBKDF2WithHmacSHA512 hash of openhab-admin-password, formatted as the
    # users.json entry for the admin user.  Written to
    # /var/lib/openhab/jsondb/users.json by openhab-admin-auth.service after
    # openhab-setup.service runs so the correct hash is always in place.
    openhab-admin-users-override = {
      generator = {
        script = "openhab-pbkdf2";
        dependencies = [ config.age.secrets.openhab-admin-password ];
      };
    };
  };

  # Ensure oauth2-proxy starts after Authelia is *healthy* (not just forked).
  # authelia-authelia-ready.service polls /api/health and only completes once Authelia
  # can serve OIDC discovery.  blocky.service is needed because OIDC discovery
  # resolves authelia.<domain> through local DNS.
  systemd.services.oauth2-proxy = {
    after = [
      "run-agenix.d.mount"
      "network-online.target"
      "blocky.service"
      "nginx.service"
      "authelia-authelia-ready.service"
    ];
    requires = [ "run-agenix.d.mount" ];
    wants = [
      "network-online.target"
      "blocky.service"
      "nginx.service"
      "authelia-authelia-ready.service"
    ];
    serviceConfig = {
      # If OIDC discovery still fails despite the gate (e.g. transient DNS
      # hiccup), space out restarts so we don't hit the default start-limit
      # (5 failures in 10 s → permanently failed).
      RestartSec = lib.mkDefault 3;
      StartLimitIntervalSec = lib.mkDefault 120;
      StartLimitBurst = lib.mkDefault 20;
    };
  };

  # Merge the real admin password hash into users.json after openhab-setup
  # writes the Nix-generated placeholder.  LoadCredential lets the DynamicUser
  # service read the agenix secret without an ownership change on the age file.
  systemd.services.openhab-admin-auth = {
    description = "OpenHAB admin user hash setup";
    after = [
      "run-agenix.d.mount"
      "openhab-setup.service"
    ];
    requires = [ "run-agenix.d.mount" ];
    before = [ "openhab.service" ];
    restartTriggers = [ config.age.secrets.openhab-admin-users-override.file ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      DynamicUser = true;
      User = "openhab";
      StateDirectory = "openhab";
      LoadCredential = [
        "users-override:${config.age.secrets.openhab-admin-users-override.path}"
      ];
      ExecStart = pkgs.writeShellScript "openhab-admin-auth" ''
        set -euo pipefail
        jsondb=/var/lib/openhab/userdata/jsondb
        override=/run/credentials/openhab-admin-auth.service/users-override
        mkdir -p "$jsondb"
        cp "$override" "$jsondb/users.json"
        # Pre-create the overview page so the MainUI setup wizard does not
        # redirect to /auth on first load.  The wizard fires exactly when
        # this page is absent from the JSONDB; writing it here means
        # OpenHAB is fully usable after first boot with no manual login.
        if [ ! -e "$jsondb/uicomponents_ui_page.json" ]; then
          install -Dm644 ${
            pkgs.writeText "uicomponents_ui_page.json" (
              builtins.toJSON {
                overview = {
                  class = "org.openhab.core.ui.components.RootUIComponent";
                  value = {
                    uid = "overview";
                    component = "oh-layout-page";
                    config = {
                      label = "Overview";
                    };
                    slots = {
                      default = [ ];
                      masonry = null;
                    };
                  };
                };
              }
            )
          } "$jsondb/uicomponents_ui_page.json"
        fi
      '';
    };
  };

  # openhab must start after admin auth so users.json has the real hash.
  # The restartTrigger ensures openhab restarts whenever the users-override
  # secret is rekeyed, so it never runs with a stale in-memory hash.
  systemd.services.openhab = {
    wants = [ "openhab-admin-auth.service" ];
    after = [ "openhab-admin-auth.service" ];
    restartTriggers = [ config.age.secrets.openhab-admin-users-override.file ];
  };

  # Write the nginx auth snippet from the admin password at activation time so
  # the file exists before nginx starts.  The snippet injects Basic auth
  # credentials into every proxied request to OpenHAB, granting admin role.
  system.activationScripts.openhabNginxAuth = {
    text = ''
      mkdir -p /run/nginx-openhab
      password=$(cat ${config.age.secrets.openhab-admin-password.path})
      auth=$(printf '%s' "admin:$password" \
        | ${pkgs.coreutils}/bin/base64 -w 0)
      printf 'proxy_set_header Authorization "Basic %s";\n' "$auth" \
        > /run/nginx-openhab/auth.conf
      chmod 644 /run/nginx-openhab/auth.conf
    '';
    deps = [ "agenixInstall" ];
  };

  # Reload nginx when the admin password rotates so it picks up the new header.
  systemd.services.nginx.restartTriggers = [
    config.age.secrets.openhab-admin-password.file
  ];

  # oauth2-proxy authenticates users via Authelia OIDC and sets a session
  # cookie scoped to openhab.${domain}.  The nginx auth_request integration
  # (via services.oauth2-proxy.nginx) gates every request to openhab.${domain}
  # before forwarding to OpenHAB.  Because nginx proxies from 127.0.0.1,
  # trustedNetworks (below) grants implicit user role to those requests.
  services.oauth2-proxy = {
    enable = true;
    provider = "oidc";
    oidcIssuerUrl = "https://authelia.${domain}";
    clientID = "openhab";
    # Sensitive values are supplied via keyFile; the module's mkDefault null
    # sets clientSecret and cookie.secret to null when keyFile is present.
    keyFile = config.age.secrets.openhab-oauth2-proxy-env.path;
    redirectURL = "https://openhab.${domain}/oauth2/callback";
    reverseProxy = true;
    setXauthrequest = true;
    scope = "openid profile email groups";
    email.domains = [ "*" ];
    # In nginx auth_request mode, oauth2-proxy does not proxy requests; nginx
    # handles the upstream connection.  static://202 satisfies the upstream
    # requirement while responding 202 to authenticated auth checks.
    upstream = "static://202";
    passBasicAuth = false;
  };

  # Gate openhab.${domain} with oauth2-proxy auth_request, restricted to the
  # LDAP groups declared in facts.network.services.openhab.groups.
  services.oauth2-proxy.nginx = {
    domain = "openhab.${domain}";
    virtualHosts."openhab.${domain}" = {
      allowed_groups = facts.network.services.openhab.groups;
    };
  };

  services.nginx.virtualHosts."openhab.${domain}" = {
    # Inject the Basic auth header into every proxied request so OpenHAB
    # grants admin role without prompting for credentials in the MainUI.
    locations."/".extraConfig = ''
      include /run/nginx-openhab/auth.conf;
    '';

    # Strip the "auth" link from REST API JSON responses so the MainUI sets
    # noAuth = true, hiding the login button.  Access control is handled by
    # oauth2-proxy; OpenHAB's session auth layer is redundant.  Basic auth
    # (above) still grants admin role for each individual REST request.
    #
    # Cache-Control: no-store prevents the browser and service worker from
    # serving a stale pre-filter response that still contains the auth link.
    #
    # Accept-Encoding is cleared so nginx receives uncompressed JSON and
    # sub_filter can rewrite it before forwarding to the browser.
    locations."/rest/" = {
      proxyPass = "http://127.0.0.1:${toString config.services.openhab.ports.http}";
      extraConfig = ''
        include /run/nginx-openhab/auth.conf;
        proxy_set_header Accept-Encoding "";
        sub_filter_types application/json;
        sub_filter_once on;
        sub_filter '{"type":"auth","url"' '{"type":"x-auth","url"';
        add_header Cache-Control "no-store" always;
      '';
    };

    # Intercept OpenHAB's PKCE authorization redirect and immediately return
    # the callback URL with a synthetic code.  The $arg_state variable
    # preserves the state token that authorize() stored in sessionStorage, so
    # the SPA's CSRF check passes.  The /rest/auth/token location (below)
    # then exchanges that code for a synthetic user object without involving
    # OpenHAB's authorization server, which would require interactive login.
    locations."/auth" = {
      extraConfig = ''
        return 302 /?code=openhab_noauth&state=$arg_state;
      '';
    };

    # Return a synthetic token response so the SPA sets store.state.user to
    # an administrator object.  enforceAdminForRoute() in auth.js checks
    # store.getters.user (not the noAuth flag), so a real user object with
    # the administrator role is required before admin routes resolve.
    # Actual API calls authenticate via the nginx-injected Basic auth header;
    # the fake access_token is never presented to OpenHAB.  expires_in is
    # set to one year so the scheduled token-refresh timer never fires during
    # normal usage.
    locations."= /rest/auth/token" = {
      extraConfig = ''
        default_type application/json;
        add_header Cache-Control "no-store" always;
        return 200 '{"access_token":"openhab_noauth","token_type":"bearer","expires_in":31536000,"refresh_token":"openhab_noauth","user":{"name":"admin","roles":["administrator"],"apiTokens":[],"sessions":[]}}';
      '';
    };
  };

  services.openhab = {
    enable = true;
    # Avoid conflict with the goss prometheusContentTypeFixProxy, which
    # occupies port 8080 on all linux hosts via linux-host.nix.
    ports.http = 8085;
    # Restart openhab whenever the Nix-generated config derivation changes.
    # Without this, nixos-rebuild switch updates config files on disk but
    # openhab keeps running with stale in-memory state (JSONDB, Felix
    # ConfigAdmin, etc.).  The module omits cfgDrv from restartTriggers by
    # default for OH 3+ because Felix can hot-reload most settings, but
    # JSONDB files are only read at startup.
    workarounds.restart.onDeploy = true;
    # Allow OpenHAB to download addons from the marketplace at startup.
    # The Z-Wave binding communicates directly with the Aeotec Z-Stick 7
    # over its serial port; once the binding is installed the controller
    # thing can be added via the UI and device discovery begins.
    initialAddons = {
      remote = true;
      binding = [ "zwave" ];
    };
    # Serial port locking (via /run/lock) is required by the Z-Wave
    # binding to claim exclusive access to the USB controller.  This adds
    # the uucp group so the openhab service can write lock files.
    workarounds.lockDir = true;
    workarounds.nativeLibs = true;
    # Grant implicit user role to requests from the oauth2-proxy host.  All
    # external traffic reaches OpenHAB through nginx, which is gated by
    # oauth2-proxy and Authelia SSO.
    conf.services.runtime."org.openhab.restauth:trustedNetworks" =
      "${proxyHost}/32";
    conf.services.runtime."org.openhab.restauth:implicitUserRole" = "true";
    # Accept Basic auth so nginx can inject admin credentials on every request.
    conf.services.runtime."org.openhab.restauth:allowBasicAuth" = "true";
  };

  services.https.fqdns."openhab.${domain}" = {
    enable = true;
    internalPort = config.services.openhab.ports.http;
  };
}
