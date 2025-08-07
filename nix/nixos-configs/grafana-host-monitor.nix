################################################################################
# Declares a dashboard for monitoring various health metrics of hosts.
# This includes:
# 1. CPU utilization.
# 2. Disk utilization (by partition).
# 3. Memory utilization.
# 4. Network utilization.
# 5. System uptime.
# 6. Failed systemd service listings.
################################################################################
{ without-socket-port, height, width, ... }: {
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
      title = "Disk I/O - Read/Write Bytes/sec";
      type = "timeseries";
      datasource = "Prometheus";
      targets = let
        # Exclude device which are non-physical (LVM volume mappers, shows as
        # "dm") and optical drives (shows as "sr").
        exclude-devices = ''{device!~"^sr.*|^dm-.*"}'';
      in [
        {
          expr = without-socket-port
            "rate(node_disk_read_bytes_total${exclude-devices}[1m])";
          legendFormat = "{{instance}} - read - {{device}}";
          format = "time_series";
        }
        {
          expr = without-socket-port
            "rate(node_disk_written_bytes_total${exclude-devices}[1m])";
          legendFormat = "{{instance}} - write - {{device}}";
          format = "time_series";
        }
      ];
      fieldConfig = {
        defaults = {
          unit = "Bps";
          decimals = 2;
          # Different devices have dramatically different throughput.  Use a
          # logarithmic scale to help us identify spikes and sustained usage.
          # Seeing everything at the same linear scale isn't terribly helpful.
          custom = {
            scaleDistribution = {
              type = "log";
            };
          };
        };
      };
      gridPos = {
        h = 1 * height;
        w = 1 * width;
        x = 1 * width;
        y = 1 * height;
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
              sum(node_filesystem_avail_bytes{
                fstype!~"tmpfs|proc|sysfs|devtmpfs|overlay|squashfs",
                device!~"^loop\\d+$|none|ramfs",
                mountpoint!~"/nix/store.*|.*boot.*"
              }) by (instance, mountpoint)
              /
              sum(node_filesystem_size_bytes{
                fstype!~"tmpfs|proc|sysfs|devtmpfs|overlay|squashfs",
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
        w = 1 * width;
        x = 2 * width;
        y = 1 * height;
      };
    }
    {
      type = "stat";
      title = "Failed Service Count";
      gridPos = {
        h = 1 * height;
        w = 1 * width;
        x = 1 * width;
        y = 1 * height;
      };
      targets = [
        {
          expr = without-socket-port ''
            sum by (instance) (
              max_over_time(systemd_unit_state{state="failed"}[1m])
            )
            or
            on(instance) systemd_unit_state{
              name="basic.target",
              state="active"
            } * 0
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
              { color = "green"; value = 0; }
              { color = "red"; value = 1; }
            ];
          };
        };
        overrides = [];
      };
    }
    {
      type = "table";
      title = "Failed Service List";
      gridPos = {
        h = 1 * height;
        w = 1 * width;
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
          format = "time_series";
          # Required for the transformations, I guess.
          refId = "A";
          # legendFormat = "{{instance}}";
          instant = true;
        }
      ];
      transformations = [
        # {
        #   id = "seriesToRows";
        #   options = {
        #     byField = "Value";
        #   };
        # }
        {
          id = "labelsToFields";
          options = {
            mode = "columns";
            keepLabels = [
              "instance"
              "name"
            ];
          };
        }
        {
          id = "filterFieldsByName";
          options = {
            filters = [
              { id = ""; value = "instance"; }
              { id = ""; value = "name"; }
            ];
            include.names = [
              "instance"
              "name"
            ];
          };
        }
        # This allows us to have a table that is empty, instead of just "No
        # Data" which kind of makes me feel like there was an error (and
        # there often is).
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
            excludeByName = true;
            indexByName = false;
            renameByName = {
              "instance" = "Host";
              "name" = "Service";
              # "Value" = "Status";
            };
            fields = [
              { name = "Host"; }
              { name = "Service"; }
              # { name = "Status"; }
            ];
          };
        }
      ];
      fieldConfig = {
        defaults = {
          thresholds = {
            mode = "absolute";
            steps = [
              { color = "green"; value = 0; }
              { color = "red"; value = 1; }
            ];
          };
        };
        overrides = [];
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
