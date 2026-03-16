################################################################################
# Active Alerts - Grafana dashboard.
#
# Shows currently firing and pending alerts from Alertmanager.  Intended as an
# information radiator: a quick glance at whether anything is on fire without
# needing push notifications.
################################################################################
{ ... }:
{
  id = null;
  uid = "active-alerts";
  schemaVersion = 38;
  title = "Active Alerts";
  refresh = "1m";
  panels = [
    {
      type = "alertlist";
      title = "Active Alerts";
      datasource = {
        type = "alertmanager";
        uid = "alertmanager";
      };
      gridPos = {
        h = 24;
        w = 24;
        x = 0;
        y = 0;
      };
      options = {
        # Show firing and pending; suppress resolved/normal noise.
        stateFilter = {
          firing = true;
          pending = true;
          error = true;
          noData = false;
          normal = false;
          inactive = false;
        };
        groupMode = "default";
        groupBy = [ ];
        alertInstanceLabelFilter = "";
        sortOrder = 3; # Sort by time (most recent first).
        maxItems = 50;
        dashboardAlerts = false;
      };
    }
  ];
}
