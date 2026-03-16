################################################################################
# Host Uptime (Timeseries) - Grafana dashboard.
#
# Shows uptime history as a time series.  Paired with the stat variant, which
# gives a glanceable current view; this one shows the historical record of
# hosts going up and down.
################################################################################
{ without-socket-port, ... }:
{
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
        };
      };
    }
  ];
}
