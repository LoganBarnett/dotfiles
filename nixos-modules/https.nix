################################################################################
# Provides HTTPS reverse proxying via nginx using Nix options.
################################################################################
{
  config,
  host-id,
  lib,
  ...
}:
let
  cfg = config.services.https;
  inherit (lib)
    mkEnableOption
    mkIf
    mkMerge
    mkOption
    types
    ;
in
{
  imports = [ ./tls-leaf.nix ];
  options = {
    services.https = {
      enable = mkEnableOption "Short-hand nginx settings." // {
        default = true;
      };
      fqdns = mkOption (
        let
          # Note: The destructuring form in Nix is not just syntax sugar.  If I
          # treated this as a single variable (e.g. `submodule`) and referenced
          # the name field (`submodule.name`), this would create infinite
          # recursion.
          fqdn-type = types.submodule (
            { name, ... }:
            {
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
                  type = types.nullOr types.port;
                  default = null;
                  description = "Internal TCP port for the upstream service.";
                };
                socket = mkOption {
                  type = types.nullOr types.path;
                  default = null;
                  description = ''
                    Unix domain socket path for the upstream service.  The service is
                    responsible for creating the socket with permissions that allow
                    the nginx-upstream group to connect.
                  '';
                };
                serviceNameForSocket = mkOption {
                  type = types.nullOr types.str;
                  default = null;
                  description = ''
                    Name of the upstream service that creates its own socket at
                    /run/<name>/<name>.sock.  https sets UMask = 0007 and
                    RuntimeDirectoryMode = 0750 on the service so the socket is
                    group-writable and the directory is traversable.  nginx is
                    added to the service's primary group (by convention the group
                    name matches the service name).
                  '';
                };
                externalPort = mkOption {
                  type = types.port;
                  default = 443;
                };
              };
            }
          );
        in
        {
          type = types.attrsOf fqdn-type;
          default = { };
        }
      );
    };
  };
  config = mkIf config.services.https.enable (
    let
      fqdns = lib.filter (fqdn-cfg: fqdn-cfg.enable) (
        lib.attrsets.attrValues cfg.fqdns
      );

      # Fqdns that proxy to a Unix domain socket.
      socketFqdns = lib.filter (
        fqdn-cfg:
        fqdn-cfg.proxy
        && (fqdn-cfg.socket != null || fqdn-cfg.serviceNameForSocket != null)
      ) fqdns;
      anySocketFqdns = socketFqdns != [ ];

      # Fqdns using the serviceNameForSocket convention.
      serviceSocketFqdns = lib.filter (
        fqdn-cfg: fqdn-cfg.serviceNameForSocket != null
      ) fqdns;

      # Compute the nginx upstream URL for a given fqdn config.
      upstreamFor =
        fqdn-cfg:
        if fqdn-cfg.internalPort != null then
          "http://127.0.0.1:${toString fqdn-cfg.internalPort}"
        else if fqdn-cfg.socket != null then
          "http://unix:${fqdn-cfg.socket}:"
        else
          "http://unix:/run/${fqdn-cfg.serviceNameForSocket}/${fqdn-cfg.serviceNameForSocket}.sock:";
    in
    {
      assertions = map (fqdn-cfg: {
        assertion =
          !fqdn-cfg.proxy
          || fqdn-cfg.internalPort != null
          || fqdn-cfg.socket != null
          || fqdn-cfg.serviceNameForSocket != null;
        message = "https: ${fqdn-cfg.fqdn} has proxy = true but no upstream is configured; set internalPort, socket, or serviceNameForSocket";
      }) fqdns;

      # Declare the shared group that grants nginx read access to explicit
      # socket upstreams (used by the socket option).
      users.groups.nginx-upstream = mkIf anySocketFqdns { };

      users.users.nginx = {
        extraGroups = [
          "tls-leaf"
        ]
        ++ lib.optionals anySocketFqdns [ "nginx-upstream" ]
        # For serviceNameForSocket, nginx joins the upstream service's primary
        # group (by convention the group name matches the service name) so it
        # can connect to the app-created socket.
        ++ map (fqdn-cfg: fqdn-cfg.serviceNameForSocket) serviceSocketFqdns;
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
        map (fqdn-cfg: {
          "${fqdn-cfg.fqdn}" = {
            inherit (fqdn-cfg) fqdn;
            ca = config.age.secrets.proton-ca;
          };
        }) fqdns
      );

      # For serviceNameForSocket fqdns, configure the upstream service so the
      # socket is created with group-writable permissions:
      # - UMask 0007 → socket mode 0770 (group can connect)
      # - RuntimeDirectoryMode 0750 → nginx can traverse /run/<name>/
      systemd.services = mkMerge (
        map (fqdn-cfg: {
          "${fqdn-cfg.serviceNameForSocket}" = {
            serviceConfig = {
              RuntimeDirectoryMode = lib.mkDefault "0750";
              UMask = lib.mkDefault "0007";
            };
          };
        }) serviceSocketFqdns
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
          map (fqdn-cfg: {
            "${fqdn-cfg.fqdn}" = {
              forceSSL = true;
              locations."/" =
                if fqdn-cfg.proxy then
                  {
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
                    proxyPass = upstreamFor fqdn-cfg;
                    # Needed if you need to use WebSocket.
                    proxyWebsockets = true;
                  }
                else
                  { };
              sslCertificateKey = config.age.secrets."tls-${fqdn-cfg.fqdn}.key".path;
              sslCertificate = ../secrets/tls-${fqdn-cfg.fqdn}.crt;
            };
          }) fqdns
        );
      };
    }
  );
}
