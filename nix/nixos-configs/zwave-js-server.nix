################################################################################
# Config a zwave-js-server for managing a Z-Wave controller.
#
# See the hardware directory for including a controller.
#
# zwave-js-server, aka node-zwave-server, seems to have fallen out of
# maintenance.  It does have very verbose logs that can be useful.  That said,
# zwave-js-ui might produce similar logs if the log level is set to "silly", but
# I haven't tried it yet.  Compared to zwave-js-ui, zwave-js-server tends to be
# more strict about pairing and interviewing in a way that is unhelpful.
################################################################################
{ ... }: {
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
