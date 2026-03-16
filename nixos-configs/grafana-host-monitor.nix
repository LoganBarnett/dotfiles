################################################################################
# Declares a dashboard for monitoring various health metrics of hosts.
# This includes:
# 1. CPU utilization.
# 2. Disk utilization (by partition).
# 3. Memory utilization.
# 4. Network utilization.
# 5. System uptime.
# 6. Service health (systemd status and goss health checks).
################################################################################
{
  without-socket-port,
  height,
  width,
  ...
}:
let
  # Ephemeral, virtual, and network filesystems excluded from disk
  # usage accounting.
  excludedFstypes = "tmpfs|proc|sysfs|devtmpfs|overlay|squashfs|nfs|nfs4";
in
{
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
          expr = ''
            # clamp_min prevents negative values caused by rate() spanning a
            # node exporter restart: the counter reset within the look-back
            # window produces a momentarily inflated idle rate, driving the
            # utilisation expression below zero.
            clamp_min(
              (1 -
                avg by (instance) (${without-socket-port ''(rate(node_cpu_seconds_total{mode="idle"}[5m]))''})
              ) * 100,
              0
            )
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
    # NOTE: Disk I/O panel commented out - it is impossible to make practical
    # sense of such a graph with the current visualization approach. The
    # logarithmic scale and per-device breakdown creates visual noise that
    # obscures meaningful patterns. This should be revisited with a better
    # approach to disk activity monitoring.
    # {
    #   title = "Disk I/O - Read/Write Bytes/sec";
    #   type = "timeseries";
    #   datasource = "Prometheus";
    #   targets = let
    #     # Exclude device which are non-physical (LVM volume mappers, shows as
    #     # "dm") and optical drives (shows as "sr").
    #     exclude-devices = ''{device!~"^sr.*|^dm-.*"}'';
    #   in [
    #     {
    #       expr = without-socket-port
    #         "rate(node_disk_read_bytes_total${exclude-devices}[1m])";
    #       legendFormat = "{{instance}} - read - {{device}}";
    #       format = "time_series";
    #     }
    #     {
    #       expr = without-socket-port
    #         "rate(node_disk_written_bytes_total${exclude-devices}[1m])";
    #       legendFormat = "{{instance}} - write - {{device}}";
    #       format = "time_series";
    #     }
    #   ];
    #   fieldConfig = {
    #     defaults = {
    #       unit = "Bps";
    #       decimals = 2;
    #       # Different devices have dramatically different throughput.  Use a
    #       # logarithmic scale to help us identify spikes and sustained usage.
    #       # Seeing everything at the same linear scale isn't terribly helpful.
    #       custom = {
    #         scaleDistribution = {
    #           type = "log";
    #         };
    #       };
    #     };
    #   };
    #   gridPos = {
    #     h = 1 * height;
    #     w = 1 * width;
    #     x = 1 * width;
    #     y = 1 * height;
    #   };
    # }
    {
      title = "Disk Usage %";
      type = "timeseries";
      datasource = "Prometheus";
      targets = [
        {
          expr = without-socket-port ''
            (1 - (
              sum(node_filesystem_avail_bytes{
                fstype!~"${excludedFstypes}",
                device!~"^loop\\d+$|none|ramfs",
                mountpoint!~"/nix/store.*|.*boot.*"
              }) by (instance, mountpoint)
              /
              sum(node_filesystem_size_bytes{
                fstype!~"${excludedFstypes}",
                device!~"^loop\\d+$|none|ramfs",
                mountpoint!~"/nix/store.*|.*boot.*"
              }) by (instance, mountpoint)
            )) * 100
          '';
          legendFormat = "{{instance}} - {{mountpoint}}";
          format = "time_series";
        }
      ];
      gridPos = {
        h = 1 * height;
        w = 1 * width;
        x = 0 * width;
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
        x = 2 * width;
        y = 0 * height;
      };
      fieldConfig = {
        defaults = {
          # This tells Grafana to auto-format bytes/sec.
          unit = "Bps";
          # This gives us some rounding.  Allows the auto formatting to
          # actually do its job.
          decimals = 3;
        };
        overrides = [ ];
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
        w = 1 * width;
        x = 2 * width;
        y = 1 * height;
      };
    }
    {
      type = "stat";
      title = "Service Health Issues";
      datasource = "Prometheus";
      gridPos = {
        h = 1 * height;
        w = 1 * width;
        x = 1 * width;
        y = 1 * height;
      };
      targets = [
        {
          # without-socket-port is applied inside each sum so that label_replace
          # strips the port before aggregation.  If it were applied outside the
          # whole expression, copper:9558 and copper:8080 would both rename to
          # copper after the + on(instance) join, producing duplicate label sets
          # that PromQL rejects.
          expr = ''
            (
              sum by (instance) (
                ${without-socket-port ''max_over_time(systemd_unit_state{state="failed"}[1m])''}
              )
              or on(instance)
              sum by (instance) (
                ${without-socket-port ''systemd_unit_state{name="basic.target",state="active"}''}
              ) * 0
              or on(instance)
              sum by (instance) (
                ${without-socket-port ''goss_tests_run_outcomes_total''}
              ) * 0
            )
            + on(instance)
            (
              sum by (instance) (
                ${without-socket-port ''increase(goss_tests_outcomes_total{outcome="fail"}[2m]) > 0''}
              )
              or on(instance)
              sum by (instance) (
                ${without-socket-port ''systemd_unit_state{name="basic.target",state="active"}''}
              ) * 0
              or on(instance)
              sum by (instance) (
                ${without-socket-port ''goss_tests_run_outcomes_total''}
              ) * 0
            )
          '';
          format = "time_series";
          legendFormat = "{{instance}}";
        }
      ];
      fieldConfig = {
        defaults = {
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "green";
                value = 0;
              }
              {
                color = "red";
                value = 1;
              }
            ];
          };
        };
        overrides = [ ];
      };
    }
    {
      type = "table";
      title = "Service Health Status";
      datasource = "Prometheus";
      gridPos = {
        h = 1 * height;
        w = 2 * width;
        x = 0 * width;
        y = 2 * height;
      };
      targets = [
        {
          expr = without-socket-port ''
            (systemd_unit_state{
              state="failed"
            } == 1)
            or
            (systemd_unit_state{
              name="basic.target",
              state="active"
            } == 1)
          '';
          format = "table";
          refId = "A";
          instant = true;
        }
        {
          expr = without-socket-port ''
            increase(goss_tests_outcomes_total{outcome="fail"}[2m]) > 0
          '';
          format = "table";
          refId = "B";
          instant = true;
        }
      ];
      transformations = [
        {
          id = "labelsToFields";
          options = {
            mode = "columns";
          };
        }
        {
          id = "filterFieldsByName";
          options = {
            include = {
              names = [
                "instance"
                "name"
                "outcome"
                "resource_id"
                "type"
              ];
            };
          };
        }
        {
          id = "filterByValue";
          options = {
            filters = [
              {
                fieldName = "name";
                config = {
                  id = "equal";
                  options = {
                    value = "basic.target";
                  };
                };
              }
            ];
            match = "any";
            type = "exclude";
          };
        }
        {
          id = "organize";
          options = {
            renameByName = {
              "instance" = "Host";
              "name" = "Service";
              "outcome" = "Result";
              "resource_id" = "Check";
              "type" = "Type";
            };
          };
        }
      ];
      fieldConfig = {
        defaults = {
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "green";
                value = 0;
              }
              {
                color = "red";
                value = 1;
              }
            ];
          };
        };
        overrides = [ ];
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
        x = 2 * width;
        y = 1 * height;
      };
      fieldConfig = {
        defaults = {
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "red";
                value = null;
              }
              {
                color = "green";
                value = 1;
              }
            ];
          };
        };
        overrides = [ ];
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
}
