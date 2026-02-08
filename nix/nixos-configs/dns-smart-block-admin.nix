################################################################################
# DNS Smart Block - Admin Interface with HTTP Basic Auth.
#
# Exposes admin endpoints (/classifications, /reprojection) via nginx with
# htpasswd authentication using agenix-rekey.
################################################################################
{ config, ... }: {
  # Expose admin interface via HTTPS with basic auth.
  services.https.fqdns."dns-admin.proton" = {
    locations."/" = {
      proxyPass = "http://127.0.0.1:8080";
      basicAuth = "DNS Smart Block Admin";
      basicAuthFile = config.age.secrets.dns-smart-block-admin-htpasswd.path;
    };
  };
}
