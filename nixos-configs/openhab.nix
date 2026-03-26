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
    flake-inputs.openhab-flake.nixosModules.${system}.openhab
  ];
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
  };

  # Ensure oauth2-proxy starts after agenix secrets are decrypted.
  systemd.services.oauth2-proxy = {
    after = [ "run-agenix.d.mount" ];
    requires = [ "run-agenix.d.mount" ];
  };

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

  services.openhab = {
    enable = true;
    # Avoid conflict with the goss prometheusContentTypeFixProxy, which
    # occupies port 8080 on all linux hosts via linux-host.nix.
    ports.http = 8085;
    users = {
      enable = true;
      users.admin = {
        # All-zero PBKDF2WithHmacSHA512 hash: base64(zeroes(64)).  This will
        # never match any real password derivation, so local password auth is
        # permanently disabled.  Auth is handled by Authelia OIDC via
        # oauth2-proxy; trustedNetworks grants implicit user role to nginx's
        # proxy requests once the user is authenticated.
        passwordHash = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA==";
        passwordSalt = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA==";
        roles = [ "administrator" ];
      };
    };
    # Grant implicit user role to requests from the oauth2-proxy host.  All
    # external traffic reaches OpenHAB through nginx, which is gated by
    # oauth2-proxy and Authelia SSO.
    conf.services.runtime."org.openhab.restauth:trustedNetworks" =
      "${proxyHost}/32";
  };

  services.https.fqdns."openhab.${domain}" = {
    enable = true;
    internalPort = config.services.openhab.ports.http;
  };
}
