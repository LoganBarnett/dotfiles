################################################################################
# DHCP Client Monitoring - Grafana dashboard.
#
# Provides panels for monitoring DHCP leases and connected hosts including:
# 1. Current DHCP leases table (with MAC, hostname, IP, expiration).
# 2. Recently connected hosts (last 24 hours).
# 3. Total active leases count.
# 4. Lease activity timeline.
################################################################################
{ without-socket-port, height, width, ... }: {
  id = null;
  uid = "dhcp-monitoring";
  schemaVersion = 16;
  title = "DHCP Clients";
  refresh = "30s";
  panels = [
    # Row 0: Summary statistics.
    {
      type = "stat";
      title = "Active DHCP Leases";
      datasource = "Prometheus";
      targets = [
        {
          expr = without-socket-port ''
            count(dhcp_lease)
          '';
          refId = "A";
        }
      ];
      fieldConfig = {
        defaults = {
          thresholds = {
            mode = "absolute";
            steps = [
              { color = "green"; value = null; }
              { color = "yellow"; value = 50; }
              { color = "orange"; value = 70; }
              { color = "red"; value = 75; }
            ];
          };
        };
        overrides = [];
      };
      gridPos = {
        h = height;
        w = width;
        x = 0 * width;
        y = 0 * height;
      };
      options = {
        reduceOptions = {
          values = false;
          calcs = [ "last" ];
          fields = "";
        };
        orientation = "auto";
        textMode = "value_and_name";
        colorMode = "background";
      };
    }
    {
      type = "stat";
      title = "Recently Connected (24h)";
      datasource = "Prometheus";
      targets = [
        {
          expr = without-socket-port ''
            count(
              dhcp_lease > (time() - 86400)
            )
          '';
          refId = "A";
        }
      ];
      fieldConfig = {
        defaults = {
          thresholds = {
            mode = "absolute";
            steps = [
              { color = "blue"; value = null; }
            ];
          };
        };
        overrides = [];
      };
      gridPos = {
        h = height;
        w = width;
        x = 1 * width;
        y = 0 * height;
      };
      options = {
        reduceOptions = {
          values = false;
          calcs = [ "last" ];
          fields = "";
        };
        orientation = "auto";
        textMode = "value_and_name";
        colorMode = "background";
      };
    }
    {
      type = "stat";
      title = "New Hosts (1h)";
      datasource = "Prometheus";
      targets = [
        {
          expr = without-socket-port ''
            count(
              dhcp_lease > (time() - 3600)
            )
          '';
          refId = "A";
        }
      ];
      fieldConfig = {
        defaults = {
          thresholds = {
            mode = "absolute";
            steps = [
              { color = "purple"; value = null; }
              { color = "orange"; value = 1; }
            ];
          };
        };
        overrides = [];
      };
      gridPos = {
        h = height;
        w = width;
        x = 2 * width;
        y = 0 * height;
      };
      options = {
        reduceOptions = {
          values = false;
          calcs = [ "last" ];
          fields = "";
        };
        orientation = "auto";
        textMode = "value_and_name";
        colorMode = "background";
      };
    }

    # Row 1: All current DHCP leases table (main view for copy+paste).
    {
      type = "table";
      title = "Current DHCP Leases";
      datasource = "Prometheus";
      gridPos = {
        h = 2 * height;
        w = 3 * width;
        x = 0 * width;
        y = 1 * height;
      };
      targets = [
        {
          expr = without-socket-port ''
            dhcp_lease
          '';
          format = "table";
          refId = "A";
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
          id = "organize";
          options = {
            excludeByName = {
              "Time" = true;
              "dnsmasq" = true;
              "__name__" = true;
              "job" = true;
            };
            renameByName = {
              "hostname" = "Hostname";
              "ip" = "IP Address";
              "mac" = "MAC Address";
              "Value" = "Lease Expiration";
            };
            indexByName = {
              "hostname" = 0;
              "ip" = 1;
              "mac" = 2;
              "Value" = 3;
            };
          };
        }
        {
          id = "convertFieldType";
          options = {
            conversions = [
              {
                targetField = "Lease Expiration";
                destinationType = "time";
              }
            ];
          };
        }
      ];
      fieldConfig = {
        defaults = {
          custom = {
            align = "left";
            filterable = true;
          };
        };
        overrides = [
          {
            matcher = {
              id = "byName";
              options = "Lease Expiration";
            };
            properties = [
              {
                id = "unit";
                value = "dateTimeFromNow";
              }
            ];
          }
        ];
      };
      options = {
        showHeader = true;
        sortBy = [
          {
            displayName = "Lease Expiration";
            desc = false;
          }
        ];
      };
    }

    # Row 3: Recently connected hosts (last 24 hours) - detailed table.
    {
      type = "table";
      title = "Recently Connected Hosts (Last 24h)";
      datasource = "Prometheus";
      gridPos = {
        h = 2 * height;
        w = 3 * width;
        x = 0 * width;
        y = 3 * height;
      };
      targets = [
        {
          expr = without-socket-port ''
            dhcp_lease > (time() - 86400)
          '';
          format = "table";
          refId = "A";
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
          id = "organize";
          options = {
            excludeByName = {
              "Time" = true;
              "dnsmasq" = true;
              "__name__" = true;
              "job" = true;
            };
            renameByName = {
              "hostname" = "Hostname";
              "ip" = "IP Address";
              "mac" = "MAC Address";
              "Value" = "First Seen";
            };
            indexByName = {
              "Value" = 0;
              "hostname" = 1;
              "ip" = 2;
              "mac" = 3;
            };
          };
        }
        {
          id = "convertFieldType";
          options = {
            conversions = [
              {
                targetField = "First Seen";
                destinationType = "time";
              }
            ];
          };
        }
      ];
      fieldConfig = {
        defaults = {
          custom = {
            align = "left";
            filterable = true;
          };
        };
        overrides = [
          {
            matcher = {
              id = "byName";
              options = "First Seen";
            };
            properties = [
              {
                id = "unit";
                value = "dateTimeFromNow";
              }
            ];
          }
        ];
      };
      options = {
        showHeader = true;
        sortBy = [
          {
            displayName = "First Seen";
            desc = true;
          }
        ];
      };
    }

    # Row 5: Lease activity over time.
    {
      title = "Active Lease Count Over Time";
      type = "timeseries";
      datasource = "Prometheus";
      targets = [
        {
          expr = without-socket-port ''
            count(dhcp_lease)
          '';
          legendFormat = "Active Leases";
          format = "time_series";
        }
      ];
      fieldConfig = {
        defaults = {
          unit = "short";
          decimals = 0;
        };
        overrides = [];
      };
      gridPos = {
        h = height;
        w = 3 * width;
        x = 0 * width;
        y = 5 * height;
      };
    }
  ];
}
