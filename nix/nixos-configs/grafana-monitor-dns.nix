################################################################################
# DNS Smart Block - Grafana monitoring dashboard.
#
# Provides panels for monitoring DNS Smart Block health, queue processing, and
# classification statistics including:
# 1. Service health status (log processor, queue processors, blocklist server).
# 2. Classification statistics by type (gaming, video-streaming).
# 3. Total domains classified and seen.
# 4. Classification events timeline.
# 5. Blocklist request rates.
################################################################################
{ without-socket-port, height, width, ... }: {
  id = null;
  uid = "dns-smart-block-monitoring";
  schemaVersion = 16;
  title = "DNS Smart Block";
  refresh = "10s";
  panels = let
    # Helper function to create a stat panel showing service health.
    mkServiceHealthPanel = {
      title,
      service,
      x,
      y
    }: {
      type = "stat";
      title = title;
      datasource = "Prometheus";
      targets = [
        {
          expr = without-socket-port ''
            systemd_unit_state{
              name="${service}",
              state="active"
            }
          '';
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
          mappings = [
            { type = "value"; options = { "0" = { text = "DOWN"; }; "1" = { text = "UP"; }; }; }
          ];
        };
        overrides = [];
      };
      gridPos = {
        h = height;
        w = width;
        x = x;
        y = y;
      };
      options = {
        reduceOptions = {
          values = false;
          calcs = [ "last" ];
          fields = "";
        };
        orientation = "auto";
        textMode = "value";
        colorMode = "background";
      };
    };

    # Helper function to create a stat panel for current count metrics.
    mkCountStatPanel = {
      title,
      expr,
      x,
      y,
      colorSteps ? [
        { color = "green"; value = null; }
        { color = "yellow"; value = 100; }
        { color = "orange"; value = 500; }
        { color = "red"; value = 1000; }
      ]
    }: {
      type = "stat";
      title = title;
      datasource = "Prometheus";
      targets = [
        {
          inherit expr;
          refId = "A";
        }
      ];
      fieldConfig = {
        defaults = {
          thresholds = {
            mode = "absolute";
            steps = colorSteps;
          };
        };
        overrides = [];
      };
      gridPos = {
        h = height;
        w = width;
        x = x;
        y = y;
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
    };
  in [
    # Row 0: Service Health Status.
    (mkServiceHealthPanel {
      title = "Log Processor";
      service = "dns-smart-block-log-processor.service";
      x = 0 * width;
      y = 0 * height;
    })
    (mkServiceHealthPanel {
      title = "Gaming Classifier";
      service = "dns-smart-block-queue-processor-gaming.service";
      x = 1 * width;
      y = 0 * height;
    })
    (mkServiceHealthPanel {
      title = "Video Streaming Classifier";
      service = "dns-smart-block-queue-processor-video-streaming.service";
      x = 2 * width;
      y = 0 * height;
    })

    # Row 1: Current Classification Counts.
    (mkCountStatPanel {
      title = "Total Domains Classified";
      expr = without-socket-port "dns_smart_block_domains_classified_total";
      x = 0 * width;
      y = 1 * height;
    })
    (mkCountStatPanel {
      title = "Gaming Domains";
      expr = without-socket-port ''dns_smart_block_domains_classified{classification_type="gaming"}'';
      x = 1 * width;
      y = 1 * height;
    })
    (mkCountStatPanel {
      title = "Video Streaming Domains";
      expr = without-socket-port ''dns_smart_block_domains_classified{classification_type="video-streaming"}'';
      x = 2 * width;
      y = 1 * height;
    })

    # Row 2: Historical Classification Totals and Domains Seen.
    (mkCountStatPanel {
      title = "Total Classifications (All Time)";
      expr = without-socket-port "dns_smart_block_classifications_all_total";
      x = 0 * width;
      y = 2 * height;
      colorSteps = [
        { color = "blue"; value = null; }
      ];
    })
    (mkCountStatPanel {
      title = "Unique Domains Seen";
      expr = without-socket-port "dns_smart_block_domains_seen";
      x = 1 * width;
      y = 2 * height;
      colorSteps = [
        { color = "purple"; value = null; }
      ];
    })
    {
      type = "stat";
      title = "Blocklist Server";
      datasource = "Prometheus";
      targets = [
        {
          expr = without-socket-port ''
            systemd_unit_state{
              name="dns-smart-block-blocklist-server.service",
              state="active"
            }
          '';
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
          mappings = [
            { type = "value"; options = { "0" = { text = "DOWN"; }; "1" = { text = "UP"; }; }; }
          ];
        };
        overrides = [];
      };
      gridPos = {
        h = height;
        w = width;
        x = 2 * width;
        y = 2 * height;
      };
      options = {
        reduceOptions = {
          values = false;
          calcs = [ "last" ];
          fields = "";
        };
        orientation = "auto";
        textMode = "value";
        colorMode = "background";
      };
    }

    # Row 3: Classification Rate (over time).
    {
      title = "Classification Rate (classifications/min)";
      type = "timeseries";
      datasource = "Prometheus";
      targets = [
        {
          expr = without-socket-port ''
            sum by (classification_type) (
              rate(dns_smart_block_classifications_total[5m]) * 60
            )
          '';
          legendFormat = "{{classification_type}}";
          format = "time_series";
        }
      ];
      fieldConfig = {
        defaults = {
          unit = "cpm";
          decimals = 2;
        };
        overrides = [];
      };
      gridPos = {
        h = height;
        w = 3 * width;
        x = 0 * width;
        y = 3 * height;
      };
    }

    # Row 4: Classification Events by Action.
    {
      title = "Classification Events by Action";
      type = "timeseries";
      datasource = "Prometheus";
      targets = [
        {
          expr = without-socket-port ''
            dns_smart_block_events
          '';
          legendFormat = "{{action}}";
          format = "time_series";
        }
      ];
      fieldConfig = {
        defaults = {
          unit = "short";
        };
        overrides = [];
      };
      gridPos = {
        h = height;
        w = 3 * width;
        x = 0 * width;
        y = 4 * height;
      };
    }

    # Row 5: Blocklist Request Rate.
    {
      title = "Blocklist Requests Rate (req/min)";
      type = "timeseries";
      datasource = "Prometheus";
      targets = [
        {
          expr = without-socket-port ''
            sum by (classification_type, status) (
              rate(dns_smart_block_blocklist_requests_total[5m]) * 60
            )
          '';
          legendFormat = "{{classification_type}} ({{status}})";
          format = "time_series";
        }
      ];
      fieldConfig = {
        defaults = {
          unit = "rpm";
          decimals = 2;
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

    # Row 6: Domains Classified Over Time.
    {
      title = "Domains Classified (Current)";
      type = "timeseries";
      datasource = "Prometheus";
      targets = [
        {
          expr = without-socket-port ''
            dns_smart_block_domains_classified
          '';
          legendFormat = "{{classification_type}}";
          format = "time_series";
        }
        {
          expr = without-socket-port ''
            dns_smart_block_domains_classified_total
          '';
          legendFormat = "Total";
          format = "time_series";
        }
      ];
      fieldConfig = {
        defaults = {
          unit = "short";
        };
        overrides = [];
      };
      gridPos = {
        h = height;
        w = 3 * width;
        x = 0 * width;
        y = 6 * height;
      };
    }
  ];
}
