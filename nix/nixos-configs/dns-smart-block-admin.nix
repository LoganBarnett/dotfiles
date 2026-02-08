################################################################################
# DNS Smart Block - Admin Interface with HTTP Basic Auth.
#
# Exposes admin endpoints (/classifications, /reprojection) via nginx with
# htpasswd authentication using agenix-rekey.
################################################################################
{ config, ... }: {
  # Generate htpasswd file for admin access using agenix-rekey's built-in
  # htpasswd generator.
  age.generators.htpasswd.dns-smart-block-admin = {
    users = ["admin"];
  };

  age.secrets.dns-smart-block-admin-htpasswd = {
    generator = config.age.generators.htpasswd.dns-smart-block-admin;
    mode = "0440";
    owner = "nginx";
    group = "nginx";
  };

  # Expose admin interface via HTTPS with basic auth.
  services.https.fqdns."dns-admin.proton" = {
    locations."/" = {
      proxyPass = "http://127.0.0.1:8080";
      basicAuth = "DNS Smart Block Admin";
      basicAuthFile = config.age.secrets.dns-smart-block-admin-htpasswd.path;
    };
  };
}
