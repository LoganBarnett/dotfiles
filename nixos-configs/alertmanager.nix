################################################################################
# This stands up alertmanager and connects it to Prometheus.
################################################################################
{
  config,
  facts,
  host-id,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    # ./matrix-alertmanager.nix
  ];
  networking.dns.aliases = [ "alertmanager" ];
  services.https.fqdns."alertmanager.${facts.network.domain}" = {
    internalPort = config.services.prometheus.alertmanager.port;
  };
  auth.ldap.users."${host-id}-alertmanager-service" = {
    email = "${host-id}-alertmanager-service@${facts.network.domain}";
    fullName = "${host-id}-alertmanager-service";
    description = "AlertManager service account on ${host-id}.  Primarily for posting alerts.";
    group = "root";
  };
  # Alertmanager posts to Matrix via matrix-alertmanager, so its LDAP identity
  # must be a member of matrix-users for the Matrix server to allow it.
  auth.ldap.groups."matrix-users".members = [ "${host-id}-alertmanager-service" ];
  age.secrets.matrix-alertmanager-secret = {
    generator.script = "base64";
    rekeyFile = ../secrets/matrix-alertmanager-secret.age;
  };
  # Shared with darwin-configs/ollama.nix — that host generates the value;
  # this host needs to read it to authenticate with the webhook endpoint.
  age.secrets.ollama-webhook-token = {
    rekeyFile = ../secrets/generated/ollama-webhook-token.age;
  };
  # Bind agenix secrets into alertmanager's credential namespace so the
  # service reads them from /run/credentials/alertmanager.service/<name>.
  # This avoids ownership management and works correctly with DynamicUser.
  systemd.services.alertmanager.serviceConfig.LoadCredential = [
    "matrix-alertmanager-secret:${config.age.secrets.matrix-alertmanager-secret.path}"
    "ollama-webhook-token:${config.age.secrets.ollama-webhook-token.path}"
  ];
  environment.systemPackages = [
    # Give us a test program to make sure this works.
    (pkgs.writeShellApplication {
      name = "alert-test";
      text = builtins.readFile ../scripts/alert-test;
      runtimeInputs = [
        pkgs.curl
      ];
    })
  ];
  services.prometheus = {
    alertmanagers = [
      {
        scheme = "http";
        static_configs = [ { targets = [ "localhost:9093" ]; } ];
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
              {
                # The Nix store must be mounted read-only at all times.  A
                # writable /nix/store allows any root process to corrupt store
                # paths silently.  This alert fires when the goss mount check
                # introduced in nixos-modules/goss-exporter.nix detects that
                # the ro option is absent.
                alert = "nix_store_writable";
                expr = ''goss_tests_outcomes_total{outcome="fail",resource_type="Mount",resource_id="/nix/store"} > 0'';
                for = "5m";
                labels = {
                  severity = "page";
                };
                annotations = {
                  summary = "{{ $labels.instance }}: /nix/store is writable.";
                  description = ''
                    The /nix/store mount on {{ $labels.instance }} is missing
                    the ro option.  A writable store allows arbitrary root
                    processes to corrupt store paths.  Do not use
                    nix-store-dislodge or any other tool as a pretext to leave
                    the store writable — terminate any stuck lock holders and
                    then restore the ro mount.  See docs/nix-store-locks.org.
                  '';
                };
              }
            ];
          }
          {
            name = "ollama_alerts";
            rules = [
              {
                # GPU baseline: ~900 classifications/hr.  CPU fallback: ~2/hr.
                # A threshold of 50/hr sits well clear of both — any sustained
                # drop below it signals Metal acceleration has been lost.
                alert = "ollama_gpu_degraded";
                expr = ''deriv(dns_smart_block_classifications_total[1h]) * 3600 < 50'';
                for = "30m";
                labels = {
                  severity = "page";
                };
                annotations = {
                  summary = "Ollama GPU acceleration degraded on {{ $labels.instance }}.";
                  description = ''
                    DNS classification throughput has been below 50/hr for more
                    than 30 minutes, indicating Ollama has fallen back to CPU
                    inference.  Current rate: {{ $value | humanize }}/hr.
                  '';
                };
              }
              {
                # Fires immediately so alertmanager can attempt remediation
                # without delay.  The goss check samples via metalps: if the
                # Ollama process has no accumulated gpu_time_ns (Metal GPU
                # context never acquired or lost after eviction), the check
                # fails and this alert fires.
                alert = "ollama_metal_gpu_evicted";
                expr = ''increase(goss_tests_outcomes_total{outcome="fail",resource_id="ollama-metal-acceleration"}[5m]) > 0'';
                for = "0m";
                labels = {
                  severity = "page";
                };
                annotations = {
                  summary = "Ollama Metal GPU evicted on {{ $labels.instance }}.";
                  description = ''
                    The ollama-metal-acceleration goss check has detected that
                    Ollama is running on CPU.  Alertmanager will POST to the
                    webhook endpoint to restart the launchd agent and
                    re-acquire the Metal GPU context.
                  '';
                };
              }
              {
                # Fires when GPU eviction has persisted for 20 minutes despite
                # repeated automated restart attempts.  Routes only to
                # team-admins (not ollama-remediation) to avoid a restart loop.
                alert = "ollama_metal_gpu_remediation_failed";
                expr = ''increase(goss_tests_outcomes_total{outcome="fail",resource_id="ollama-metal-acceleration"}[5m]) > 0'';
                for = "20m";
                labels = {
                  severity = "page";
                };
                annotations = {
                  summary = "Ollama Metal GPU remediation failed on {{ $labels.instance }}.";
                  description = ''
                    GPU eviction has persisted for more than 20 minutes despite
                    automated restart attempts.  Manual intervention is
                    required.
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
                url = "http://${host-id}.${facts.network.domain}:3001/alerts";
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
          {
            name = "ollama-remediation";
            webhook_configs = [
              {
                url = "http://M-CL64PK702X.${facts.network.domain}:9000/hooks/ollama-restart";
                # No resolved notification needed — we only care about kicking
                # the restart, not about the all-clear.
                send_resolved = false;
                http_config = {
                  authorization = {
                    type = "Bearer";
                    credentials_file = "/run/credentials/alertmanager.service/ollama-webhook-token";
                  };
                };
              }
            ];
          }
        ];
        route = {
          group_by = [
            "alertname"
            "alias"
          ];
          group_wait = "30s";
          group_interval = "2m";
          repeat_interval = "4h";
          receiver = "team-admins";
          routes = [
            {
              # continue = true sends to ollama-remediation AND falls through
              # to the parent receiver (team-admins) so humans are still
              # notified even while remediation is in progress.
              matchers = [ ''alertname="ollama_metal_gpu_evicted"'' ];
              receiver = "ollama-remediation";
              continue = true;
              # Fire immediately — no batching delay for remediation.
              group_wait = "0s";
              # Re-POST every 10 minutes while the alert remains active.
              repeat_interval = "10m";
            }
          ];
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
  # Goss health checks for Alertmanager.
  services.goss.checks = {
    # Check that the HTTPS endpoint is responding.
    http."https://alertmanager.${facts.network.domain}/" = {
      status = 200;
      timeout = 5000;
    };
    # Check that the Alertmanager API is responding.
    http."https://alertmanager.${facts.network.domain}/api/v2/status" = {
      status = 200;
      timeout = 3000;
    };
    # Check that the internal alertmanager port is listening.  Alertmanager
    # binds to :: (IPv6 any) which also accepts IPv4 connections via
    # IPv4-mapped IPv6 addresses.
    port."tcp6:${toString config.services.prometheus.alertmanager.port}" = {
      listening = true;
    };
    # Check that HTTPS port is listening (handled by reverse proxy).
    port."tcp:443" = {
      listening = true;
    };
    # Confirm the reverse proxy is reachable from all interfaces, not
    # only on one specific address.
    command."tcp:443-wildcard-binding" = pkgs.lib.custom.gossWildcardPortCheck 443;
    # Check that the alertmanager service is running.
    service.alertmanager = {
      enabled = true;
      running = true;
    };
  };
}
