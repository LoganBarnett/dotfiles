{ ... }: {
  # This becomes available as `pkgs.lib`.  How it can just be "lib" remains a
  # mystery.
  nixpkgs.overlays = [(final: prev: {
    lib = prev.lib // {
      custom = {
        monitor-to-exporter-name = monitor: {
          blackbox-ping = "blackbox";
        }.${monitor} or monitor;
      };
    };
  })];
}
