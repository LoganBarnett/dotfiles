################################################################################
# Garage-Queue Metrics - Grafana dashboard.
#
# Provides panels for monitoring the garage-queue job system:
# - Queue depth, enqueue/completion rates, and items in flight.
# - Connected workers, per-worker capacity and processing state.
# - Server and worker latency distributions (p50/p95/p99).
################################################################################
{ without-socket-port, ... }:
let
  height = 8;
  width = 6; # quarter-row (24 / 4)

  mkPanel =
    {
      title,
      expr,
      legendFormat ? "{{instance}}",
      unit ? "short",
      decimals ? 2,
      x ? 0,
      y ? 0,
    }:
    {
      inherit title;
      type = "timeseries";
      datasource = "Prometheus";
      targets = [
        {
          expr = without-socket-port expr;
          inherit legendFormat;
          format = "time_series";
        }
      ];
      gridPos = {
        h = height;
        w = width;
        inherit x y;
      };
      fieldConfig.defaults = {
        inherit unit decimals;
      };
    };

  mkMultiTargetPanel =
    {
      title,
      targets,
      unit ? "s",
      decimals ? 3,
      x ? 0,
      y ? 0,
      w ? width,
    }:
    {
      inherit title;
      type = "timeseries";
      datasource = "Prometheus";
      targets = map (t: {
        expr = without-socket-port t.expr;
        legendFormat = t.legendFormat;
        format = "time_series";
      }) targets;
      gridPos = {
        h = height;
        inherit w x y;
      };
      fieldConfig.defaults = {
        inherit unit decimals;
      };
    };

  row0 = 0;
  row1 = height;
  row2 = 2 * height;
in
{
  title = "Garage-Queue";
  uid = "garage-queue-dashboard";
  schemaVersion = 37;
  version = 1;
  refresh = "10s";
  panels = [
    # ── Row 0: Queue overview ──────────────────────────────────────────
    {
      type = "row";
      title = "Queue Overview";
      gridPos = {
        h = 1;
        w = 24;
        x = 0;
        y = row0;
      };
      collapsed = false;
      panels = [ ];
    }
    (mkPanel {
      title = "Queue Depth";
      expr = "gq_server_queue_depth";
      legendFormat = "{{queue}}";
      x = 0 * width;
      y = row0 + 1;
    })
    (mkPanel {
      title = "Enqueue Rate";
      expr = ''rate(gq_server_items_enqueued_total[5m])'';
      legendFormat = "{{queue}}";
      unit = "ops";
      x = 1 * width;
      y = row0 + 1;
    })
    (mkPanel {
      title = "Completion Rate";
      expr = ''rate(gq_server_items_completed_total[5m])'';
      legendFormat = "{{queue}}";
      unit = "ops";
      x = 2 * width;
      y = row0 + 1;
    })
    (mkPanel {
      title = "Items In Flight";
      expr = "gq_server_items_in_flight";
      x = 3 * width;
      y = row0 + 1;
    })

    # ── Row 1: Workers ─────────────────────────────────────────────────
    {
      type = "row";
      title = "Workers";
      gridPos = {
        h = 1;
        w = 24;
        x = 0;
        y = row1;
      };
      collapsed = false;
      panels = [ ];
    }
    (mkPanel {
      title = "Connected Workers";
      expr = "gq_server_workers_connected";
      x = 0 * width;
      y = row1 + 1;
    })
    (mkPanel {
      title = "Items Processing (per worker)";
      expr = "gq_worker_items_processing";
      x = 1 * width;
      y = row1 + 1;
    })
    (mkPanel {
      title = "Worker Capacity";
      expr = "gq_worker_capacity_total";
      x = 2 * width;
      y = row1 + 1;
    })
    (mkPanel {
      title = "Worker Processing Rate";
      expr = ''rate(gq_worker_items_processed_total[5m])'';
      unit = "ops";
      x = 3 * width;
      y = row1 + 1;
    })

    # ── Row 2: Latency ─────────────────────────────────────────────────
    {
      type = "row";
      title = "Latency";
      gridPos = {
        h = 1;
        w = 24;
        x = 0;
        y = row2;
      };
      collapsed = false;
      panels = [ ];
    }
    (mkMultiTargetPanel {
      title = "Server Item Duration";
      w = 12;
      x = 0;
      y = row2 + 1;
      targets = [
        {
          expr = ''histogram_quantile(0.50, sum(rate(gq_server_item_duration_seconds_bucket[5m])) by (le, queue))'';
          legendFormat = "p50 {{queue}}";
        }
        {
          expr = ''histogram_quantile(0.95, sum(rate(gq_server_item_duration_seconds_bucket[5m])) by (le, queue))'';
          legendFormat = "p95 {{queue}}";
        }
        {
          expr = ''histogram_quantile(0.99, sum(rate(gq_server_item_duration_seconds_bucket[5m])) by (le, queue))'';
          legendFormat = "p99 {{queue}}";
        }
      ];
    })
    (mkMultiTargetPanel {
      title = "Worker Item Duration";
      w = 12;
      x = 12;
      y = row2 + 1;
      targets = [
        {
          expr = ''histogram_quantile(0.50, sum(rate(gq_worker_item_duration_seconds_bucket[5m])) by (le, instance))'';
          legendFormat = "p50 {{instance}}";
        }
        {
          expr = ''histogram_quantile(0.95, sum(rate(gq_worker_item_duration_seconds_bucket[5m])) by (le, instance))'';
          legendFormat = "p95 {{instance}}";
        }
        {
          expr = ''histogram_quantile(0.99, sum(rate(gq_worker_item_duration_seconds_bucket[5m])) by (le, instance))'';
          legendFormat = "p99 {{instance}}";
        }
      ];
    })
  ];
}
