################################################################################
# This is the replacement for ./https.nix that uses Nix options.
################################################################################
{ config, host-id, lib, ... }: let
  cfg = config.services.https;
  inherit (lib) mkEnableOption mkIf mkMerge mkOption types;
in {
  imports = [ ./tls-leaf.nix ];
  options = {
    services.https = {
      enable = mkEnableOption "Short-hand nginx settings." // {
        default = true;
      };
      fqdns = mkOption (let
        # Note: The destructuring form in Nix is not just syntax sugar.  If I
        # treated this as a single variable (e.g. `submodule`) and referenced
        # the name field (`submodule.name`), this would create infinite
        # recursion.
        fqdn-type = types.submodule ({ name, ... }: {
          options = {
            enable = mkEnableOption "Reverse proxies for FQDNs." // {
              default = true;
            };
            fqdn = mkOption {
              type = types.str;
              readOnly = true;
              internal = true;
              default = name;
            };
            proxy = mkOption {
              type = types.bool;
              default = true;
              description = ''
              Setup a reverse proxy for TLS.  Typically we want this on, but PHP
              generally want this off since they are running nginx already.
            '';
            };
            internalPort = mkOption {
              type = types.port;
            };
            externalPort = mkOption {
              type = types.port;
              default = 443;
            };
          };
        });
      in {
        type = types.attrsOf fqdn-type;
        default = {};
      });
    };
  };
  config = mkIf config.services.https.enable (let
    fqdns = lib.filter
      (fqdn-cfg: fqdn-cfg.enable)
      (lib.attrsets.attrValues cfg.fqdns)
    ;
  in {
    users.users.nginx = {
      extraGroups = [ "tls-leaf" ];
      group = "nginx";
      isSystemUser = true;
    };
    networking.firewall.allowedTCPPorts = mkMerge (
      map (fqdn-config: [ fqdn-config.externalPort ]) fqdns
    );
    networking.firewall.allowedUDPPorts = mkMerge (
      map (fqdn-config: [ fqdn-config.externalPort ]) fqdns
    );
    tls.tls-leafs = mkMerge (
      map
        (fqdn-cfg: {
          "${fqdn-cfg.fqdn}" = {
            inherit (fqdn-cfg) fqdn;
            ca = config.age.secrets.proton-ca;
          };
        })
        fqdns
    );
    services.nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      # This should be added on a per-service basis, since all services
      # are not resilient for this.  The one encountered specifically for
      # me is
      # comfyui.
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

      virtualHosts = mkMerge (
        map
          (fqdn-cfg: {
            "${fqdn-cfg.fqdn}" = {
              forceSSL = true;
              locations."/" = if fqdn-cfg.proxy then {
                extraConfig = lib.strings.concatLines [
                  # Required when the target is also TLS server with multiple
                  # hosts.
                  "proxy_ssl_server_name on;"
                  # Required when the server wants to use HTTP Authentication.
                  "proxy_pass_header Authorization;"
                  # Sometimes you'll get advice to add the following headers.
                  # Do not do that.  It's already included in a separate conf
                  # file that the NixOS nginx module includes automatically.
                  # "proxy_set_header Host $host;"
                  # "proxy_set_header X-Real-IP $remote_addr;"
                  # "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;"
                  # "proxy_set_header X-Forwarded-Proto $scheme;"
                ];
                proxyPass = "http://127.0.0.1:${toString fqdn-cfg.internalPort}";
                # Needed if you need to use WebSocket.
                proxyWebsockets = true;
              } else {};
              sslCertificateKey = config
                .age
                .secrets
                ."tls-${fqdn-cfg.fqdn}.key"
                .path
              ;
              sslCertificate = ../secrets/tls-${fqdn-cfg.fqdn}.crt;
            };
          })
          fqdns
      );
    };
  });
}
