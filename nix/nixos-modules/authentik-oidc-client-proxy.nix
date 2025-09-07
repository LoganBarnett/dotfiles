################################################################################
# Setup an OIDC client proxy using oauth2-proxy.  There may be skew towards
# supporting Authentik and other authentication systems support is not known.
################################################################################
{ lib, pkgs, config, ... }:
let
  inherit (lib) mkOption types mkMerge;
  cfg = config.services.oidc-proxy;
in
{
  options.services.oidc-proxy = {
    fqdns = mkOption {
      description = "Map of FQDN → OIDC proxy config.";
      type = types.attrsOf (types.submodule ({ name, ... }: {
        options = {
          issuerUrl = mkOption {
            type = types.str;
            example = "https://auth.example.com/application/o/myapp/";
            description = "OIDC issuer URL from Authentik for this client.";
          };
          clientId = mkOption {
            type = types.str;
            default = "${name}-proxy";
            description = "OIDC client_id registered in Authentik.";
          };
          # Name of the age secret that holds the client_secret
          clientSecretName = mkOption {
            type = types.str;
            description = "age.secrets NAME containing the OIDC client_secret.";
          };
          internalPort = mkOption {
            type = types.port;
            description = "Backend service port on localhost.";
          };
          proxyPort = mkOption {
            type = types.port;
            default = 4180;
            description = "Local port oauth2-proxy listens on for this FQDN.";
          };
        };
      }));
      default = {};
    };
  };

  config = let
    fqdns = config.services.oidc-proxy.fqdns;
  in {
    # age.secrets
    age.secrets = lib.mkMerge (lib.mapAttrsToList (fqdn: inst:
      let
        svc = "oidc-proxy-${lib.replaceStrings ["."] ["-"] fqdn}";
        cookie = "${svc}-cookie-secret";
      in {
        # 24 will give us 32 bytes, which is what oauth2-proxy expects.
        ${cookie} = { generator.script = "base64url"; settings.length = 32; };
      }
    ) fqdns);

    # env-file-secrets
    services.environment-file-secrets.services =
      (lib.attrsets.mapAttrs' (fqdn: inst: let
        svc = "oidc-proxy-${lib.replaceStrings ["."] ["-"] fqdn}";
        cookie = "${svc}-cookie-secret";
      in {
        name = svc;
        value = {
          secrets = {
          ${inst.clientSecretName} = {
            environmentVariable = "OAUTH2_PROXY_CLIENT_SECRET";
            secretName = "oauth2-proxy-${svc}-client-secret-environment-variable";
          };
          "${cookie}".environmentVariable = "OAUTH2_PROXY_COOKIE_SECRET";
        };
        };
      }) fqdns);

    # systemd
    systemd.services = lib.mkMerge (lib.mapAttrsToList (fqdn: inst: let
      svc = "oidc-proxy-${lib.replaceStrings ["."] ["-"] fqdn}";
    in {
      ${svc} = {
        description = "oauth2-proxy for ${fqdn}";
        wantedBy = [ "multi-user.target" ];
        after    = [ "network.target" ];
        serviceConfig = {
          DynamicUser = true;
          Environment = [
            # TODO: Set groups here via this:
            # "OAUTH2_PROXY_ALLOWED_GROUPS=openhab-users,admins"
            # But, you know, configurable.
            "OAUTH2_PROXY_EMAIL_DOMAINS=*"
            "OAUTH2_PROXY_PROVIDER=oidc"
            "OAUTH2_PROXY_REVERSE_PROXY=true"
            "OAUTH2_PROXY_OIDC_ISSUER_URL=${inst.issuerUrl}"
            "OAUTH2_PROXY_CLIENT_ID=${inst.clientId}"
            "OAUTH2_PROXY_HTTP_ADDRESS=127.0.0.1:${toString inst.proxyPort}"
            "OAUTH2_PROXY_REDIRECT_URL=https://${fqdn}/oauth2/callback"
            "OAUTH2_PROXY_SET_XAUTHREQUEST=true"
            "OAUTH2_PROXY_PASS_ACCESS_TOKEN=true"
            "OAUTH2_PROXY_COOKIE_REFRESH=1h"
            "OAUTH2_PROXY_COOKIE_EXPIRE=168h"
          ];
          ExecStart = "${pkgs.oauth2-proxy}/bin/oauth2-proxy";
          Restart   = "on-failure";
        };
      };
    }
    ) fqdns);

    # nginx
    services.nginx.virtualHosts =
      lib.mkMerge (lib.mapAttrsToList (fqdn: inst: {
        ${fqdn}.extraConfig = ''
          location /oauth2/ {
            proxy_pass http://127.0.0.1:${toString inst.proxyPort};
            include proxy_params;
          }
          location = /oauth2/auth {
            proxy_pass http://127.0.0.1:${toString inst.proxyPort};
            include proxy_params;
                      }
          location / {
            auth_request /oauth2/auth;
            error_page 401 =302 /oauth2/start;
            error_page 403 =302 /oauth2/start;
            proxy_set_header X-Forwarded-User  "";
            proxy_set_header X-Forwarded-Email "";
            proxy_set_header X-Forwarded-User  $upstream_http_x_auth_request_user;
            proxy_set_header X-Forwarded-Email $upstream_http_x_auth_request_email;
            proxy_set_header Host $host;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection $connection_upgrade;
            proxy_buffering off;
            proxy_read_timeout 3600s;
            proxy_pass http://127.0.0.1:${toString inst.internalPort};
          }
        '';
      }) fqdns);
  };
}
