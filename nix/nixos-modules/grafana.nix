################################################################################
# Provides a Grafana server.
#
# Grafana reads from a specialized metrics data source (such as Prometheus), and
# presents it in (hopefully) interesting and informative ways.  Typically this
# is in the form of some sort of graph - hence the name sounding like "graph".
#
# See https://wiki.nixos.org/wiki/Grafana for a writeup on how to get started
# with Grafana in NixOS.
#
#
################################################################################
{ config, facts, host-id, lib, pkgs, ... }: let
  service-user-prefix = host-id;
  # How these get wired up is tricky.  We need to be able to delete them as
  # Grafana starts up, or it won't pick up changes.  So we need to safely emit
  # them somewhere where they can be "deleted" (removed symlinks).  We also want
  # changes to dashboards to issue a restart.  This is really hard to do in Nix,
  # since the settings are not desired to be in any of the provisioning
  # dashboard settings.  The evaluation means a content change isn't a name
  # change and thus it's the same as before - no service restart.  So instead we
  # use some systemd trickery to pull it off.
  #
  # Names must be valid POSIX file names.
  dashboards = let
    without-socket-port = query:
      ''label_replace(${query}, "instance", "$1", "instance", "^(.*):[0-9]+$")''
    ;
    height = 9;
    # This is half a row.
    width = 12;
    # In all honesty, I vibe coded these.  I believe there is a mix of Grafana
    # versions in here.  It's probably worth learning the structure more
    # tightly, and making sure all of this confirms to latest.
  in {
    nvidia-gpu = let
      # This assumes a single GPU on the host.  Perhaps should use a chopped up
      # version of the PCI ID to help tell them apart?
      mkGpuPanel = {
        title,
          expr,
          unit ? "percent",
          decimals ? 2,
          y ? 0,
          h ? 8
      }: {
        title = title;
        type = "timeseries";
        datasource = "Prometheus";
        targets = [{
          expr = without-socket-port expr;
          legendFormat = "{{instance}}";
          format = "time_series";
        }];
        gridPos = { x = 0; y = y; w = 24; h = h; };
        fieldConfig.defaults.unit = unit;
        fieldConfig.defaults.decimals = decimals;
      };
    in
      {
        title = "NVIDIA GPU Metrics";
        uid = "nvidia-gpu-dashboard";
        schemaVersion = 37;
        version = 1;
        refresh = "10s";
        panels = [
          (mkGpuPanel {
            title = "GPU Utilization";
            expr = "nvidia_smi_utilization_gpu_ratio";
            unit = "percent";
            y = 0;
          })
          (mkGpuPanel {
            title = "Memory Utilization";
            expr = "nvidia_smi_memory_free_bytes / nvidia_smi_memory_total_bytes";
            unit = "percent";
            y = 9;
          })
          (mkGpuPanel {
            title = "Memory Used";
            expr = "nvidia_smi_memory_used_bytes";
            unit = "bytes";
            y = 18;
          })
          (mkGpuPanel {
            title = "Temperature";
            expr = "nvidia_smi_temperature_gpu";
            unit = "celsius";
            y = 27;
          })
          (mkGpuPanel {
            title = "Power Draw (Watts)";
            expr = "nvidia_smi_power_draw_watts";
            unit = "watts";
            y = 36;
          })
        ];
      };
    system-monitoring = {
      id = null;
      uid = "system-monitoring";
      schemaVersion = 16;
      title = "System Monitoring";
      panels = [
        {
          title = "CPU Usage %";
          type = "timeseries";
          datasource = "Prometheus";
          targets = [
            {
              expr =
                ''
                  (1 -
                    avg by (instance) (${
                     without-socket-port
                       ''(rate(node_cpu_seconds_total{mode="idle"}[5m]))''
                   })
                  ) * 100
                '';
              legendFormat = "{{instance}}";
              format = "time_series";
            }
          ];
          gridPos = {
            h = 1 * height;
            w = 1 * width;
            x = 0 * width;
            y = 0 * height;
          };
        }
        {
          title = "Memory Usage %";
          type = "timeseries";
          datasource = "Prometheus";
          targets = [
            {
              expr = without-socket-port ''
               (1 -
                 (node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes)
               ) * 100
              '';
              legendFormat = "{{instance}}";
              format = "time_series";
            }
          ];
          gridPos = {
            h = 1 * height;
            w = 1 * width;
            x = 1 * width;
            y = 0 * height;
          };
        }
        {
          title = "Disk Usage %";
          type = "timeseries";
          datasource = "Prometheus";
          targets = [
            {
              expr = without-socket-port ''
                (1 - (
                  node_filesystem_avail_bytes {
                    fstype!="tmpfs",fstype!="devtmpfs"
                  } /
                    node_filesystem_size_bytes {
                      fstype!="tmpfs",fstype!="devtmpfs"
                    }
                )) * 100'';
              legendFormat = "{{instance}} - {{mountpoint}}";
              format = "time_series";
            }
          ];
          gridPos = {
            h = 1 * height;
            w = 1 * width;
            x = 1 * width;
            y = 1 * height;
          };
        }
        {
          title = "Network Traffic (TX + RX)";
          type = "timeseries";
          datasource = "Prometheus";
          targets = [
            {
              expr = without-socket-port ''
                sum by (instance) (
                  rate(node_network_receive_bytes_total {
                    device!~"lo|docker.*|veth.*"
                  }[5m])
                )
              '';
              legendFormat = "{{instance}} RX";
              format = "time_series";
            }
            {
              expr = without-socket-port ''
                sum by (instance) (
                  rate(node_network_transmit_bytes_total {
                    device!~"lo|docker.*|veth.*"
                  }[5m])
                )
              '';
              legendFormat = "{{instance}} TX";
              format = "time_series";
            }
          ];
          gridPos = {
            h = 1 * height;
            w = 1 * width;
            x = 0 * width;
            y = 2 * height;
          };
          fieldConfig = {
            defaults = {
              # This tells Grafana to auto-format bytes/sec.
              unit = "Bps";
              # This gives us some rounding.  Allows the auto formatting to
              # actually do its job.
              decimals = 3;
            };
            overrides = [];
          };
        }
        {
          title = "Load Average (1 min)";
          type = "timeseries";
          datasource = "Prometheus";
          targets = [
            {
              expr = without-socket-port "node_load1";
              legendFormat = "{{instance}}";
              format = "time_series";
            }
          ];
          gridPos = {
            h = 1 * height;
            w = 2 * width;
            x = 0 * width;
            y = 3 * height;
          };
        }
        {
          type = "stat";
          title = "Host Uptime";
          datasource = "Prometheus";
          targets = [
            {
              expr = without-socket-port ''up{job="node"}'';
              format = "time_series";
              legendFormat = "{{instance}}";
            }
          ];
          gridPos = {
            h = 1 * height;
            w = 1 * width;
            x = 1 * width;
            y = 2 * height;
          };
          options = {
            legend = {
              show = true;
              displayMode = "table";
              # displayMode = "list";
            };
          };
        }
      ];
    };
    # Both the stat and timeseries versions of this are helpful because the
    # former gives us an glanceable dashboard to tell us what hosts are down or
    # up.  The latter shows us history.
    uptime-timeseries = {
      id = null;
      uid = "uptime-timeseries";
      schemaVersion = 16;
      title = "Uptime (Timeseries)";
      panels = [
        {
          type = "stat";
          title = "Host Uptime";
          datasource = "Prometheus";
          targets = [
            {
              expr = without-socket-port ''up{job="node"}'';
              format = "time_series";
              legendFormat = "{{instance}}";
            }
          ];
          gridPos = {
            h = 9;
            w = 24;
            x = 0;
            y = 0;
          };
          options = {
            legend = {
              show = true;
              displayMode = "table";
              # displayMode = "list";
            };
          };
        }
      ];
    };
    uptime-stat = {
      id = null;
      uid = "uptime-stat";
      schemaVersion = 38;
      title = "Uptime (Stat)";
      panels = [
        {
          type = "stat";
          title = "Host Uptime";
          datasource = "Prometheus";
          targets = [
            {
              expr = without-socket-port ''up{job="node"}'';
              # expr = "up";
              refId = "A";
            }
          ];
          fieldConfig = {
            defaults = {
              thresholds = {
                mode = "absolute";
                steps = [
                  { color = "red"; value = null; }
                  { color = "green"; value = 1; }
                ];
              };
              mappings = {
                type = "value";
                options = [
                  { value = 0; text = "DOWN"; }
                  { value = 1; text = "UP"; }
                ];
              };
            };
            overrides = [];
          };
          gridPos = {
            h = 4;
            w = 4;
            x = 0;
            y = 0;
          };
          options = {
            reduceOptions = {
              values = false;
              calcs = [ "last" ];
              fields = "";
            };
            orientation = "auto";
            textMode = "value";
            colorMode = "value";
          };
        }
      ];
    };
    wireguard = {
      uid = "wireguard";
      title = "WireGuard Status, Bandwidth, etc.";
      tags = [ "wireguard" ];
      timezone = "browser";
      # editable = true;
      refresh = "10s";
      schemaVersion = 36;
      # version = 1;
      panels = let
        # We just get back public_key as a display name by default.  The
        # exporter does support a friendly name mapping, but it requires a
        # comment in the Wireguard configuration file, which the exporter
        # parses.  Since we emit the configuration file via a Nix
        # expression, we can't include comments.  But that's okay, because
        # our facts definition has all of the information we need.  Here we
        # convert that into a static "mapping" via overrides.
        #
        # Each value comes in as "$public_key $type" (e.g. "foobarkey RX").
        # One for RX and one for TX.  We have to match the exact strings of
        # both to provide overrides.
        peer-overrides-with-transmission = lib.pipe facts.network.users [
          (lib.attrsets.mapAttrsToList (name: user: user.devices))
          lib.lists.flatten
          (lib.lists.filter (d: d.vpn))
          (lib.lists.map (d: d // {
            public-key = lib.strings.trim (builtins.readFile
              ../secrets/${d.host-id}-wireguard-client.pub
            );
          }))
          (lib.lists.map (d:
            lib.lists.map (x: {
              matcher = {
                id = "byName";
                options = "${d.public-key} ${x}";
              };
              properties = [
                { id = "displayName"; value = "${d.host-id} ${x}"; }
              ];
            }) [ "RX" "TX"]
          ))
          lib.lists.flatten
        ];
        # Ugh this could use a lot of de-duplication.
        peer-overrides = lib.pipe facts.network.users [
          (lib.attrsets.mapAttrsToList (name: user: user.devices))
          lib.lists.flatten
          (lib.lists.filter (d: d.vpn))
          (lib.lists.map (d: d // {
            public-key = lib.strings.trim (builtins.readFile
              ../secrets/${d.host-id}-wireguard-client.pub
            );
          }))
          (lib.lists.map (d: {
            matcher = {
              id = "byName";
              options = "${d.public-key}";
            };
            properties = [
              { id = "displayName"; value = "${d.host-id}"; }
            ];
          }))
          lib.lists.flatten
        ];
      in [
        {
          title = "WireGuard Bandwidth per Peer";
          type = "timeseries";
          datasource = "Prometheus";
          targets = [
            {
              expr = "rate(wireguard_received_bytes_total[5m])";
              legendFormat = "{{public_key}} RX";
              refId = "A";
            }
            {
              expr = "rate(wireguard_sent_bytes_total[5m])";
              legendFormat = "{{public_key}} TX";
              refId = "B";
            }
          ];
          fieldConfig = {
            defaults = {
              unit = "Bps";
              decimals = 2;
            };
            overrides = peer-overrides-with-transmission;
          };
          gridPos = { x = 0; y = 0; w = 12; h = 9; };
        }
        {
          title = "WireGuard Bandwidth per Interface";
          type = "timeseries";
          datasource = "Prometheus";
          targets = [
            {
              expr = "sum by (interface) (rate(wireguard_received_bytes_total[5m]))";
              legendFormat = "{{interface}} RX";
              refId = "A";
            }
            {
              expr = "sum by (interface) (rate(wireguard_sent_bytes_total[5m]))";
              legendFormat = "{{interface}} TX";
              refId = "B";
            }
          ];
          fieldConfig = {
            defaults = {
              unit = "bps";
              decimals = 2;
            };
          };
          gridPos = { x = 12; y = 0; w = 12; h = 9; };
        }
        {
          title = "Peer Last Handshake (Seconds Ago)";
          type = "bargauge";
          datasource = "Prometheus";
          targets = [
            {
              expr = "time() - wireguard_latest_handshake_seconds";
              legendFormat = "{{public_key}}";
              refId = "A";
            }
          ];
          fieldConfig = {
            defaults = {
              unit = "s";
              min = 0;
              max = 600;
              thresholds = {
                mode = "absolute";
                steps = [
                  { color = "green"; value = null; }
                  { color = "yellow"; value = 30; }
                  { color = "red"; value = 300; }
                ];
              };
            };
            overrides = peer-overrides;
          };
          options = {
            displayMode = "gradient"; # Or "lcd" or "basic"
            orientation = "horizontal"; # Optional
            showUnfilled = true;
          };
          gridPos = {
            h = 9;
            w = 24;
            x = 0;
            y = 9;
          };
        }
      ];
    };
  };
in {
  imports = [
    (import ../nixos-modules/https.nix {
      server-port = 3000;
      inherit host-id;
      fqdn = "grafana.proton";
    })
  ];
  environment.systemPackages = [
    # Include sqlite because it's what Grafana uses in the default NixOS setup.
    # This can allow us to chase down database issues if the API isn't
    # forthcoming to us.
    pkgs.sqlite
  ];
  environment.etc = lib.attrsets.mapAttrs' (name: value: {
    name = "grafana/dashboards/provisioned/${name}.json";
    value = { text = builtins.toJSON value; };
  }) dashboards;
  networking.firewall.allowedTCPPorts = [ 3000 ];
  networking.firewall.allowedUDPPorts = [ 3000 ];
  services.grafana = {
    enable = true;
    declarativePlugins = with pkgs.grafanaPlugins; [];
    provision = {
      enable = true;
      # A provider in this context is a dashboard provider.  It's not quite a
      # "folder" but it is close to that.
      dashboards.settings.providers = [
        {
          name = "provisioned-dashboards";
          options.path = "/etc/grafana/dashboards/provisioned";
          folder = "Provisioned";
          disableDeletion = false;
          updateIntervalSeconds = 60;
        }
      ];
      datasources.settings.datasources = [
        {
          name = "Prometheus";
          type = "prometheus";
          url = "https://prometheus.proton";
          access = "proxy";
          isDefault = true;
        }
      ];
    };
    settings = {
      # Enable anonymous access to the dashboards.  Everything is exposed
      # publicly via Prometheus exporters anyways.
      "auth.anonymous" = {
        enabled = true;
        # org_name = "Main Org.";
        org_role = "Viewer";
        hide_version = false;
        # device_limit = 1;
      };
      "auth.ldap" = {
        enabled = true;
        config_file = (toString ((pkgs.formats.toml {}).generate "ldap.toml" {
          servers = [
            {
              host = "nickel.proton";
              port = 636;
              use_ssl = true;
              start_tls = false;
              tls_ciphers = [];
              min_tls_version = "";
              ssl_skip_verify = false;
              # This might not be needed, since it should probably use the
              # system trust.
              root_ca_cert = ../secrets/proton-ca.crt;
              bind_dn =
                "uid=${service-user-prefix}-grafana-service,ou=users,dc=proton,dc=org"
              ;
              bind_password = "$__file{${
                config
                  .age
                  .secrets
                  ."${service-user-prefix}-grafana-service-ldap-password"
                  .path
              }}";
              timeout = 10;
              search_filter = "(uid=%s)";
              search_base_dns = ["dc=proton,dc=org"];
              # Deliberately left out to disable.
              # group_search_filter = "";
              # Deliberately left out to disable.
              # group_search_filter_user_attribute = "";
              group_search_filter_base_dns = [
                "ou=groups,dc=proton,dc=org"
              ];
              attributes = {
                email = "mail";
                member_of = "memberOf";
                username = "uid";
              };
              group_mappings = [
                {
                  group_dn = "cn=grafana-admins,ou=groups,dc=proton,dc=org";
                  org_role = "Admin";
                  grafana_admin = true;
                }
                {
                  group_dn = "cn=grafana-viewers,ou=groups,dc=proton,dc=org";
                  org_role = "Viewer";
                }
              ];
            }
          ];
          # Undocumented?  But talked about here:
          # https://stackoverflow.com/a/47594344
          # The documented approach of setting `filters = ldap:debug` only adds
          # a single, unhelpful statement for authentication issues.
          # It doesn't change anything though.  I believe this is from an older
          # version.
          # verbose_logging = true;
        }))
        ;
        # Allow sign-up should be `true` (default) to allow Grafana to create
        # users on successful LDAP authentication.  If set to `false` only
        # already existing Grafana users will be able to login.
        allow_sign_up = true;
        # Prevent synchronizing ldap users organization roles.
        # skip_org_role_sync = true;
        skip_org_role_sync = false;
      };
      log = {
        filters = "ldap:debug";
      };
    };
  };
  systemd.services.grafana = {
    wants = [ "run-agenix.d.mount" ];
    # Use a handy trick seen in
    # https://blog.withsam.org/blog/nixos-path-restart-trigger/ to promote
    # restarts.  Spoiler alert:  `restartTriggers` doesn't work seamlessly, but
    # if we serialize and hash our stuff, it's cool.
    restartTriggers =  [
      # Look at what you made me do.
      (builtins.hashString "sha256" (builtins.toJSON dashboards))
    ];
  };
  # The dashboards you see above are not truly declarative - they are purely
  # additive.  Some changes will be affected automatically, but others will not.
  # Surprise on which ones!  Nah, just delete them all and rebuild them.  Nix is
  # your master now.
  #
  # TODO: Test this out now that we've fixed the issue with "duplicate
  # dashboards".
  systemd.services.clean-grafana-dashboards = {
    description = "Clean out Grafana dashboards before startup.";
    before = [ "grafana.service" ];
    wantedBy = [ "grafana.service" ];
    requiredBy = [ "grafana.service" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = let
        # I feel like this should be the value, but I'm doing things a little
        # too manually.  Let's see if we can use this later.
        # dashboardsDir = config.services.grafana.settings.paths.provisioning;
        dashboardsDir = "/etc/grafana/dashboards";
      in [
        # Safely wipe only .json files.
        # TODO: Let's emit the JSON files with a prefix such as
        # "__nix-provisioned-${name}".  Then we can find `__nix-provisioned-*`
        # and only delete those, and leave dynamically created ones intact.
        # This might be a worthy submission to add.
        "/run/current-system/sw/bin/find ${dashboardsDir} -type f -name '*.json' -delete"
      ];
      User = "grafana";
      Group = "grafana";
    };
  };
  users.users.grafana.extraGroups = [ "openldap-${host-id}-grafana-service" ];
  users.groups."openldap-${host-id}-grafana-service" = {};
}
