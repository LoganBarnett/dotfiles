################################################################################
# DHCP Client Monitoring - Grafana dashboard.
#
# Provides panels for monitoring DHCP leases and connected hosts using
# per-lease data from the dhcp-lease-textfile config on silicon.
#
# 1. Total active lease count.
# 2. Hosts first seen in the last 24 hours / last hour.
# 3. Full current lease table (hostname, IP, MAC, expiry) for copy+paste.
# 4. Recently connected hosts table sorted by first_seen.
# 5. Lease count over time.
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
          expr = without-socket-port "count(dhcp_lease_expiry)";
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
      title = "New Connections (24h)";
      datasource = "Prometheus";
      targets = [
        {
          # Count devices whose first_seen falls within the last 24 hours.
          expr = without-socket-port ''
            count(dhcp_lease_first_seen > (time() - 86400)) or vector(0)
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
      title = "New Connections (1h)";
      datasource = "Prometheus";
      targets = [
        {
          # Highlight when a device recently joined the network.
          expr = without-socket-port ''
            count(dhcp_lease_first_seen > (time() - 3600)) or vector(0)
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

    # Row 1: All current leases, full detail for copy+paste.
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
          # Multiply by 1000 to convert seconds to milliseconds, which is what
          # Grafana's time type and date unit formats expect.
          expr = without-socket-port "dhcp_lease_expiry * 1000";
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
              "__name__" = true;
              "instance" = true;
              "job" = true;
            };
            renameByName = {
              "hostname" = "Hostname";
              "ip" = "IP Address";
              "mac" = "MAC Address";
              "vendor" = "Vendor";
              "Value" = "Lease Expiry";
            };
            indexByName = {
              "hostname" = 0;
              "vendor" = 1;
              "ip" = 2;
              "mac" = 3;
              "Value" = 4;
            };
          };
        }
        {
          id = "convertFieldType";
          options = {
            conversions = [
              {
                targetField = "Lease Expiry";
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
              options = "Lease Expiry";
            };
            properties = [
              { id = "unit"; value = "dateTimeFromNow"; }
            ];
          }
        ];
      };
      options = {
        showHeader = true;
        sortBy = [
          { displayName = "Hostname"; desc = false; }
        ];
      };
    }

    # Row 3: Recently connected hosts, sorted newest first.
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
          # Filter to devices first seen within the last 24 hours, then
          # multiply by 1000 to convert epoch seconds to milliseconds for
          # Grafana's time type.
          expr = without-socket-port ''
            (dhcp_lease_first_seen > (time() - 86400)) * 1000
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
              "__name__" = true;
              "instance" = true;
              "job" = true;
            };
            renameByName = {
              "hostname" = "Hostname";
              "ip" = "IP Address";
              "mac" = "MAC Address";
              "vendor" = "Vendor";
              "Value" = "First Seen";
            };
            indexByName = {
              "Value" = 0;
              "hostname" = 1;
              "vendor" = 2;
              "ip" = 3;
              "mac" = 4;
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
              { id = "unit"; value = "dateTimeFromNow"; }
            ];
          }
        ];
      };
      options = {
        showHeader = true;
        sortBy = [
          { displayName = "First Seen"; desc = true; }
        ];
      };
    }

    # Row 5: Lease count over time.
    {
      title = "Active Lease Count Over Time";
      type = "timeseries";
      datasource = "Prometheus";
      targets = [
        {
          expr = without-socket-port "count(dhcp_lease_expiry)";
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
