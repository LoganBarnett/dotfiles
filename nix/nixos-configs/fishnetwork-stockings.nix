################################################################################
# Connect to fishnetwork-stockings.
################################################################################
{ config, lib, pkgs, ... }: let
  psk-name = "wifi-psk";
  # This doesn't work, because a sub-unit is setup for the specific interface
  # that wpa_supplicant finds.  Unless we want to lock into specific interfaces
  # (we don't, lest this module become non-reusable), we need to rely on the
  # fact that this service must run as root to operate, and simply point it at
  # the agenix-located secret directly.
  # psk-path = "$CREDENTIALS_DIRECTORY/${psk-name}";
  psk-path = config.age.secrets.fishnetwork-stockings-5g-password.path;
  psk-script = pkgs.writeShellApplication {
    name = "psk-script";
    text = ''
      ${pkgs.coreutils}/bin/cat ${psk-path}
    '';
  };
in {
  age.secrets.fishnetwork-stockings-5g-password = {
    rekeyFile = ../secrets/fishnetwork-stockings-5g-password.age;
  };
  age.secrets.fishnetwork-stockings-5g-password-environment-variable = {
    generator = {
      script = "environment-variable";
      dependencies = [
        config.age.secrets.fishnetwork-stockings-5g-password
      ];
    };
    settings.field = "psk_fishnetwork_stockings";
  };
  networking.wireless = {
    enable = true;
    # If we need more than one later, we'll need to combine them into a proper
    # "environment file".
    secretsFile = config
      .age
      .secrets
      .fishnetwork-stockings-5g-password-environment-variable
      .path
    ;
    networks = {
      fishnetwork-stockings-5g = {
        priority = 20;
        # extraConfig = ''
        #   psk=FILE:${psk-path}
        # '';
        pskRaw = "ext:psk_fishnetwork_stockings";
      };
      fishnetwork-stockings = {
        priority = 5;
        # extraConfig = ''
        #   psk=FILE:${psk-path}
        # '';
        # pskRaw = "ext:${psk-script}/bin/psk-script";
        pskRaw = "ext:psk_fishnetwork_stockings";
      };
    };
  };
  # wpa_supplicant spawns off a sub-service of sorts.  Using the @ notation
  # below, we can indicate these settings apply to any of the sub-services (and
  # perhaps the primary service itself, but I have not verified this).
  systemd.services."wpa_supplicant@" = let
    after = [ "run-agenix.d.mount" ];
  in {
    inherit after;
    requires = after;
    # This doesn't work, because a sub-unit is setup for the specific interface
    # that wpa_supplicant finds.  Unless we want to lock into specific
    # interfaces (we don't, lest this module become non-reusable), we need to
    # rely on the fact that this service must run as root to operate, and simply
    # point it at the agenix-located secret directly.
    serviceConfig.LoadCredential = [
      "${psk-name}:${config.age.secrets.fishnetwork-stockings-5g-password.path}"
    ];
  };
}
