################################################################################
# This stands up alertmanager and connects it to Prometheus.
################################################################################
{ config, host-id, lib, pkgs, ... }: let
  token-file = "/etc/nixos/secrets/matrix-alertmanager.token";
in {
  imports = [
    (import ../nixos-modules/https.nix {
      server-port = 9093;
      inherit host-id;
      fqdn = "alertmanager.proton";
    })
  ];
  age.secrets.matrix-alertmanager-secret = {
    generator.script = "base64";
    rekeyFile = ../secrets/matrix-alertmanager-secret.age;
  };
  environment.systemPackages = [
    # Give us a test program to make sure this works.
    (pkgs.writeShellApplication {
      name = "alert-test";
      text = builtins.readFile ../scripts/alert-test.sh;
      runtimeInputs = [
        pkgs.curl
      ];
    })
  ];
  services.prometheus = {
    alertmanagers = [
      {
        scheme = "http";
        static_configs = [{ targets = [ "localhost:9093" ]; }];
      }
    ];
    rules = [
      (builtins.toJSON {
        groups = [
          {
            name = "node_alerts";
            rules = [
              {
                alert = "node_down";
                expr = ''up{job="node"} == 0'';
                for = "5m";
                labels = {
                  severity = "page";
                };
                annotations = {
                  summary = "{{ $labels.alias }}: Node is down.";
                  description = ''
                    {{ $labels.alias }} has been down for more than 5 minutes.
                  '';
                };
              }
              {
                alert = "systemd_unit_down";
                expr = ''systemd_unit_state{state="failed"} == 1'';
                for = "5m";
                labels = {
                  severity = "page";
                };
                annotations = {
                  summary = "{{ $labels.alias }}: Systemd unit is down.";
                  description = ''
                    {{ $labels.alias }} has been down for more than 5 minutes.
                  '';
                };
              }
            ];
          }
        ];
      })
    ];
    alertmanager = {
      enable = true;
      openFirewall = true;
      configuration = {
        receivers = [
          {
            name = "team-admins";
            webhook_configs = [
              {
                # Default webhook path is /alerts per:
                # https://github.com/jaywink/matrix-alertmanager
                url = "http://${host-id}.proton:3001/alerts";
                send_resolved = true;
                http_config = {
                  basic_auth = {
                    # This seems to be hardcoded into matrix-alertmanager.
                    username = "alertmanager";
                    password_file = "/run/credentials/alertmanager.service/matrix-alertmanager-secret";
                  };
                };
              }
            ];
          }
        ];
        route = {
          group_by = [ "alertname" "alias" ];
          group_wait = "30s";
          group_interval = "2m";
          repeat_interval = "4h";
          receiver = "team-admins";
        };
      };
    };
  };
  # This is what the old promql stuff looked like, I think.
  # ''
  #   ALERT node_down
  #   IF up == 0
  #   FOR 5m
  #   LABELS {
  #     severity="page"
  #   }
  #   ANNOTATIONS {
  #     summary = "{{$labels.alias}}: Node is down.",
  #     description = "{{$labels.alias}} has been down for more than 5 minutes."
  #   }
  # ''
  systemd.services.alertmanager = {
    serviceConfig = {
      LoadCredential = [
        # If we do it this way, we don't run into permission issues accessing
        # the secret.  This is basically required because the NixOS module for
        # alertmanager configures the systemd service to use DynamicUser.
        "matrix-alertmanager-secret:${config.age.secrets.matrix-alertmanager-secret.path}"
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
  systemd.timers.matrix-alertmanager-token-refresh = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "daily";
      Persistent = true;
    };
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
}
