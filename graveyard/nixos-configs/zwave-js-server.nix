################################################################################
# TOMBSTONE — zwave-js-server is no longer hosted.
#
# Configures zwave-js-server (aka node-zwave-server) for managing a Z-Wave
# controller.  It produces very verbose logs but has fallen out of active
# maintenance, and is more strict about pairing and interviewing in ways that
# are unhelpful.
#
# Z-Wave JS tooling is designed as a companion to Home Assistant.  We are no
# longer running Home Assistant.  Hardware secrets are retained in
# hardware/aeotec-z-stick-7.nix.  This file is kept for historical reference.
################################################################################
{ ... }:
{
  services.zwave-js = {
    enable = true;
    secretsConfigFile = "/run/credentials/zwave-js.service/zwave-js-secret";
    settings = {
      # As of [2025-07-10] the only documentation for configuration is sitting
      # here: https://github.com/zwave-js/zwave-js-server/blob/master/example_config.js
      # Do not follow the readme for true configuration structure, but it might
      # give some hints.
      logConfig = {
        enable = true;
        # Actually make this print 99.999999% of the logs when running as a
        # service?  Cool.
        forceConsole = true;
        # level = "debug";
        # Should be debug.  6 is "silly".
        # level = 5;
        level = "debug";
      };
      serial = {
        baudRate = 115200;
        # baudRate = 9600;
      };
    };
  };
}
