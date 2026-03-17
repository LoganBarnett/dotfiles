################################################################################
# Track groceries, recipes, pantry/refrigerator inventory.
#
# Imagine the savings.
#
# I chose Grocy because I've heard it's got great support for bar-code scanning,
# which is essential for tracking refrigerator.
################################################################################
{ host-id, ... }:
{
  imports = [
    ./https-module.nix
  ];
  # Grocy is a PHP app served directly by nginx, not proxied.
  services.https.fqdns."${host-id}.proton" = {
    proxy = false;
  };
  services.grocy = {
    enable = true;
    # Documentation says this is specifically for ACME and Let's Encrypt,
    # neither of which we are using.  Leave it off until we find out if it's
    # needed.
    # enableSSL = true;
  };
}
