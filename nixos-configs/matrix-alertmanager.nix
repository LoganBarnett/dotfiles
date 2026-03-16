{ config, host-id, lib, pkgs, ... }: let
  token-file = "/etc/nixos/secrets/matrix-alertmanager.token";
in {
  systemd.services.alertmanager = {
    serviceConfig = {
      LoadCredential = [
        # If we do it this way, we don't run into permission issues accessing
        # the secret.  This is basically required because the NixOS module for
        # alertmanager configures the systemd service to use DynamicUser.
        "matrix-alertmanager-secret:${
          config
            .age
            .secrets
            .matrix-alertmanager-secret
            .path
        }"
      ];
    };
  };
  services.matrix-alertmanager = {
    enable = true;
    # For some reason this defaults to the same port as alertmanager.  This also
    # means the webhook URL in alertmanager-proper needs to follow too.
    port = 3001;
    homeserverUrl = "https://matrix.proton";
    matrixUser = "${host-id}-alertmanager-service";
    matrixRooms = [
      {
        # Alerts room.
        # Be mindful that this is a private room, and so invites must be sent
        # out.  I might just want to make it public.
        roomId = "!qCtJEBShlKlBhZWqLd:matrix.proton";
        receivers = [ "team-admins" ];
      }
    ];
    mention = true;
    tokenFile = token-file;
    secretFile = config
      .age
      .secrets
      .matrix-alertmanager-secret
      .path;
  };
  # Activate the timer on a switch/rebuild.
  # system.activationScripts.matrix-alertmanager-token-refresh-activate = let
  #   service-name = "matrix-alertmanager-token-refresh";
  #   systemctl = "${pkgs.systemd}/bin/systemctl";
  # in {
  #   text = ''
  #     if ${systemctl} is-enabled --quiet ${service-name}.service; then
  #       echo "Triggering ${service-name}.service after rebuild."
  #       ${systemctl} start ${service-name}.service
  #     fi
  #   '';
  # };
  systemd.services.matrix-alertmanager = {
    unitConfig = {
      StartLimitIntervalSec = 30;
      StartLimitBurst = 3;
    };
    wants = [
      "run-agenix.d.mount"
      "matrix-alertmanager-token-refresh.service"
    ];
  };
  systemd.services.matrix-alertmanager-token-refresh = {
    enable = true;
    description = "Refresh the Matrix access token.";
    serviceConfig = {
      Type = "oneshot";
      # Synapse can aggressively give 429s.
      Restart = "on-failure";
      RestartSec = "30min";
      ExecStart = let
        # Unfortunately the documentation generators out there are incomplete
        # for this function.  See
        # https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/trivial-builders/default.nix#L175
        # for all of the parameters availble.
        script = pkgs.writeShellApplication {
          name = "matrix-token-refresh";
          excludeShellChecks = [
            "SC2154"
          ];
          runtimeInputs = [
            pkgs.curl
            pkgs.jq
          ];
          text = builtins.readFile ../scripts/matrix-token-refresh.sh;
        };
        password-file = config
          .age
          .secrets
          ."${host-id}-alertmanager-service-ldap-password"
          .path
        ;
      in
        lib.strings.concatStringsSep " " [
          "${script}/bin/matrix-token-refresh"
          "--password-file '${password-file}'"
          "--token-file '${token-file}'"
          ''--username "${host-id}-alertmanager-service"''
        ];
      User = "root";
    };
    environment = {
      password_file = config
        .age
        .secrets
        ."${host-id}-alertmanager-service-ldap-password"
        .path
      ;
      homeserver_url = "https://matrix.proton";
      username = "${host-id}-alertmanager-service";
    };
    wants = [ "run-agenix.d.mount" ];
  };
  systemd.timers.matrix-alertmanager-token-refresh = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "daily";
      Persistent = true;
    };
  };
}
