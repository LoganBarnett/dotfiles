################################################################################
# I've other people use the nomenclature of "facts", and it kind of makes sense
# because "settings" would be a very ambiguous term.  "Facts" is only de facto
# via Puppet, as I understand.
#
# It means settings, but more like a meta settings.  Settings here should be
# logical instead of pertaining to specific systems whenever possible.
################################################################################
{
  network = {
    # See https://stackoverflow.com/a/70106730 for how to setup Prometheus for
    # monitoring remote / uncontrolled systems (remote_read vs. remote_write).
    hosts = {
      argon = {
        controlledHost = true;
        monitors = [
          "blackbox-ping"
          "node"
          "wireguard"
        ];
      };
      arsenic = {
        controlledHost = true;
        monitors = [
          "blackbox-ping"
          "node"
        ];
      };
      bromine = {
        controlledHost = true;
        monitors = [
          "blackbox-ping"
          "node"
        ];
      };
      gallium = {
        controlledHost = true;
        monitors = [
          "blackbox-ping"
          "node"
        ];
      };
      germanium = {
        controlledHost = true;
        monitors = [
          "node"
        ];
      };
      grafana = {
        controlledHost = true;
        monitors = [];
      };
      lithium = {
        controlledHost = true;
        monitors = [
          "blackbox-ping"
          "node"
        ];
      };
      nickel = {
        controlledHost = true;
        monitors = [
          "blackbox-ping"
          # "openldap"
          "node"
        ];
      };
      selenium = {
        controlledHost = true;
        monitors = [
          "blackbox-ping"
          # "octoprint"
          "node"
        ];
      };
    };
    users = {
      logan = {
        description = "The reason we suffer.";
        email = "logustus@proton";
        type = "person";
        full-name = "Logan Barnett";
      };
      bromine-grafana-service = {
        email = "bromine-grafana-service@proton";
        type = "service";
        description = "Grafana on Bromine.";
        full-name = "bromine-grafana-service";
      };
      gallium-nextcloud-service = {
        email = "gallium-nextcloud-service@proton";
        type = "service";
        description = "Nextcloud on Gallium.";
        full-name = "gallium-nextcloud-service";
      };
      # monitor-grafana-service = {
      #   email = "monitor-grafana-service@proton";
      #   type = "service";
      #   description = "Grafana on Monitor.";
      #   full-name = "monitor-grafana-service";
      # };
      selenium-octoprint-service = {
        email = "selenium-octoprint-service@proton";
        type = "service";
        description = "Octoprint on Selenium.";
        full-name = "selenium-octoprint-service";
      };
    };
    groups = {
      "3d-printer-admins" = {
        description = "People who can administer Octoprint.";
        members = [
          "logan"
        ];
      };
      "3d-printer-printers" = {
        description = "People who can use 3D printers.";
        members = [
          "logan"
        ];
      };
      "grafana-admins" = {
        description = "People who can administer Grafana.";
        members = [
          "logan"
        ];
      };
      "grafana-viewers" = {
        description = "People who can view dashboards in Grafana.";
        members = [
          "logan"
        ];
      };
      "nextcloud-admins" = {
        description = "People who can administer Nextcloud.";
        members = [
          "logan"
        ];
      };
      "nextcloud-users" = {
        description = "People who can use Nextcloud.";
        members = [
          "logan"
        ];
      };
      screen-addicts = {
        description =
          "Users who are addicted to screens and require help with "
          + "self-regulation."
        ;
        members = [
          "kai"
          "solomon"
        ];
      };
    };
  };
}
