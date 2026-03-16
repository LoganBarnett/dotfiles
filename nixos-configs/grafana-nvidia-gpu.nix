################################################################################
# NVIDIA GPU Metrics - Grafana dashboard.
#
# Provides panels for monitoring NVIDIA GPU health on hosts with a GPU.
# Assumes a single GPU per host; a PCI ID prefix could distinguish multiple
# GPUs in the future.
#
# 1. GPU utilization.
# 2. Memory utilization.
# 3. Memory used (bytes).
# 4. Temperature.
# 5. Power draw (watts).
################################################################################
{ without-socket-port, ... }:
let
  mkGpuPanel =
    {
      title,
      expr,
      unit ? "percent",
      decimals ? 2,
      y ? 0,
      h ? 8,
    }:
    {
      title = title;
      type = "timeseries";
      datasource = "Prometheus";
      targets = [
        {
          expr = without-socket-port expr;
          legendFormat = "{{instance}}";
          format = "time_series";
        }
      ];
      gridPos = {
        x = 0;
        y = y;
        w = 24;
        h = h;
      };
      fieldConfig.defaults.unit = unit;
      fieldConfig.defaults.decimals = decimals;
    };
in
{
  title = "NVIDIA GPU Metrics";
  uid = "nvidia-gpu-dashboard";
  schemaVersion = 37;
  version = 1;
  refresh = "10s";
  panels = [
    (mkGpuPanel {
      title = "GPU Utilization";
      expr = "nvidia_smi_utilization_gpu_ratio";
      unit = "percent";
      y = 0;
    })
    (mkGpuPanel {
      title = "Memory Utilization";
      expr = "nvidia_smi_memory_free_bytes / nvidia_smi_memory_total_bytes";
      unit = "percent";
      y = 9;
    })
    (mkGpuPanel {
      title = "Memory Used";
      expr = "nvidia_smi_memory_used_bytes";
      unit = "bytes";
      y = 18;
    })
    (mkGpuPanel {
      title = "Temperature";
      expr = "nvidia_smi_temperature_gpu";
      unit = "celsius";
      y = 27;
    })
    (mkGpuPanel {
      title = "Power Draw (Watts)";
      expr = "nvidia_smi_power_draw_watts";
      unit = "watts";
      y = 36;
    })
  ];
}
