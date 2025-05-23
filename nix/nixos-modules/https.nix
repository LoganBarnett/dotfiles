################################################################################
# Setup an HTTPS reverse proxy.
################################################################################
{
  listen-port ? 443,
  # Typically we want this on, but we want this off for PHP apps where, in
  # essence, PHP is running on nginx already.
  redirect ? true,
  server-port ? null,
  host-id,
  fqdn ? "${host-id}.proton"
}: { config, ... }: {
  age.secrets."tls-${fqdn}.key" = {
    generator = {
      dependencies = [
        config.age.secrets.proton-ca
      ];
      script = "tls-signed-certificate";
    };
    group = "tls-leaf";
    mode = "0440";
    settings = {
      root-certificate = config.age.secrets.proton-ca;
      inherit fqdn;
    };
    rekeyFile = ../secrets/tls-${fqdn}.key.age;
  };

  users.groups = {
    nginx = {};
    tls-leaf = {};
  };

  users.users.nginx = {
    extraGroups = [ "tls-leaf" ];
    group = "nginx";
    isSystemUser = true;
  };
  networking.firewall.allowedTCPPorts = [ listen-port ];
  networking.firewall.allowedUDPPorts = [ listen-port ];
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    # This should be added on a per-service basis, since all services are not
    # resilient for this.  The one encountered specifically for me is comfyui.
    # appendHttpConfig = ''
    #   # Add HSTS header with preloading to HTTPS requests.
    #   # Adding this header to HTTP requests is discouraged
    #   map $scheme $hsts_header {
    #       https   "max-age=31536000; includeSubdomains; preload";
    #   }
    #   add_header Strict-Transport-Security $hsts_header;

    #   # Enable CSP for your services.
    #   # add_header Content-Security-Policy "script-src 'self'; object-src 'none'; base-uri 'none';" always;

    #   # Minimize information leaked to other domains
    #   add_header 'Referrer-Policy' 'origin-when-cross-origin';

    #   # Disable embedding as a frame
    #   # add_header X-Frame-Options DENY;

    #   # Prevent injection of code in other mime types (XSS Attacks)
    #   add_header X-Content-Type-Options nosniff;

    #   # This might create errors
    #   proxy_cookie_path / "/; secure; HttpOnly; SameSite=strict";
    # '';

    virtualHosts.${fqdn} = {
      forceSSL = true;
      locations."/" = if redirect then {
        extraConfig =
          # required when the target is also TLS server with multiple hosts
          "proxy_ssl_server_name on;" +
          # required when the server wants to use HTTP Authentication
          "proxy_pass_header Authorization;"
          ;
        proxyPass = "http://127.0.0.1:${toString server-port}";
        proxyWebsockets = true; # needed if you need to use WebSocket
      } else {};
      sslCertificateKey = config.age.secrets."tls-${fqdn}.key".path;
      sslCertificate = ../secrets/tls-${fqdn}.crt;
    };
  };
}
