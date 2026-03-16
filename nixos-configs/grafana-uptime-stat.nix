################################################################################
# Host Uptime (Stat) - Grafana dashboard.
#
# Shows current up/down state for all hosts as colored stat panels.  Paired
# with the timeseries variant, which shows historical uptime data; this one
# provides a glanceable at-a-glance view of what is currently up or down.
################################################################################
{ without-socket-port, ... }:
{
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
          refId = "A";
        }
      ];
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
          mappings = {
            type = "value";
            options = [
              {
                value = 0;
                text = "DOWN";
              }
              {
                value = 1;
                text = "UP";
              }
            ];
          };
        };
        overrides = [ ];
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
}
