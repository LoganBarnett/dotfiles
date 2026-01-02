################################################################################
# This stands up alertmanager and connects it to Prometheus.
################################################################################
{ config, host-id, lib, pkgs, ... }: {
  imports = [
    (import ../nixos-modules/https.nix {
      server-port = 9093;
      inherit host-id;
      fqdn = "alertmanager.proton";
    })
    # ./matrix-alertmanager.nix
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
}
