################################################################################
# Non-email Porkbun DNS records.  Contributes to the same
# `services.nix-hapi-porkbun.scopes` attrset that porkbun-dns-email.nix
# populates; per-key merging means each file owns its own records without
# stomping on the other's config.
#
# Currently:
#   logustus.com / CNAME/blog  →  loganbarnett.github.io
#     (points blog.logustus.com at the GitHub Pages site)
################################################################################
{ ... }:
{
  services.nix-hapi-porkbun.scopes."logustus.com".records."CNAME/blog" = {
    content = "loganbarnett.github.io";
  };
}
