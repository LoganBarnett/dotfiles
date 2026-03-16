################################################################################
# Enable this NixOS host to be an Airplay client and play audio to Airplay
# devices.
#
# See https://wiki.nixos.org/wiki/PipeWire#AirPlay/RAOP_configuration for
# inspiration.
#
# Untested.
################################################################################
{
  services.pipewire = {
    # opens UDP ports 6001-6002
    raopOpenFirewall = true;
    extraConfig.pipewire = {
      "10-airplay" = {
        "context.modules" = [
          {
            name = "libpipewire-module-raop-discover";
            # increase the buffer size if you get dropouts/glitches
            # args = {
            #   "raop.latency.ms" = 500;
            # };
          }
        ];
      };
    };
  };
}
