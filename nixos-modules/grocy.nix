################################################################################
# Track groceries, recipes, pantry/refrigerator inventory.
#
# Imagine the savings.
#
# I chose Grocy because I've heard it's got great support for bar-code scanning,
# which is essential for tracking refrigerator.
################################################################################
{ host-id, ... }: {
  imports = [
    (import ./https.nix {
      inherit host-id;
      server-port = 80;
      # Because this is a PHP app, we have to do special things.  Nginx will not
      # serve as a reverse proxy but as the application server itself.
      redirect = false;
    })
  ];
  services.grocy = {
    enable = true;
    # Documentation says this is specifically for ACME and Let's Encrypt,
    # neither of which we are using.  Leave it off until we find out if it's
    # needed.
    # enableSSL = true;
  };
}
