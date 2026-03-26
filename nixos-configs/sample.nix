################################################################################
# A sample configuration.  Don't actually include this anywhere, because there's
# no real sample service.
################################################################################
{ config, facts, ... }:
{
  # Note we don't need to import anything.  This is because the base host file
  # imports all capabilities, and we just activate them with these "config"
  # files.
  # imports = [];

  age.secrets.sample-foo-secret = {
    # Find generators in ../agenix/.  "hex" in particular has been overridden to
    # be configurable.
    generator.script = "hex";
    # Our deviation from agenix-rekey:  You can have settings!
    settings.length = 16;
  };

  # This enables HTTPS support.  Make sure to update the aliases attribute on
  # the host that will be running this.
  services.https.fqdns."sample.${facts.network.domain}" = {
    # Most of the times you will run into a port based program and this is how
    # you would use it if it's not a PHP app.  See below for how this works for
    # sockets.
    internalPort = config.services.sample.port;
    serviceNameForSocket = "sample";
  };

  services.sample = {
    enable = true;
  };
  systemd.services.sample = {
    # Make sure our secrets are available before we try to start.
    after = [ "run-agenix.d.mount" ];
    requires = [ "run-agenix.d.mount" ];
    LoadCredential = [
      # This makes it so we needn't manage groups shared with agenix, and we
      # don't have to manage file permissions for the secret.  These get stuffed
      # under /run/credentials/%C (%C is the name of the service).  Even root
      # cannot see them - fancy systemd tricks!
      "foo-secret:${config.age.secrets.sample-foo-secret.path}"
    ];
  };
}
