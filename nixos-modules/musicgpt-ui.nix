{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkOption mkIf types;
  cfg = config.services.musicgpt-ui;

  # Heuristic: respect nixpkgs' CUDA toggle. If false or unset, we don't pass
  # --gpu.
  gpuEnabledDefault =
    (pkgs.config.cudaSupport or false)
    # Also honor classic NixOS toggle if present on this system.
    || (config.hardware.nvidia.cudaSupport or false);
in
{
  options.services.musicgpt-ui = {
    enable = mkEnableOption ''
      Run the MusicGPT UI server (`musicgpt` in UI mode).
    '';

    package = mkOption {
      type = types.package;
      default = pkgs.musicgpt;
      description = ''
        The MusicGPT package providing the `musicgpt` binary.
      '';
    };

    model = mkOption {
      type = types.str;
      default = "small";
      example = "medium-fp16";
      description = ''
        Model name passed to <code>--model</code>. Default: <code>small</code>.
        Known values (not enforced): <code>small</code>,
        <code>small-fp16</code>, <code>small-quant</code>, <code>medium</code>,
        <code>medium-fp16</code>, <code>medium-quant</code>, <code>large</code>.
        These are documented here to reduce maintenance burden rather than
        enforced as an enum.
      '';
    };

    port = mkOption {
      type = types.port;
      default = 8642;
      description = ''
        TCP port for the web UI (<code>--ui-port</code>).
      '';
    };

    useSplitDecoder = mkOption {
      type = types.bool;
      default = false;
      description = ''
        If true, pass <code>--use-split-decoder</code>.
      '';
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Open the firewall for the chosen port and pass <code>--ui-expose</code>
        so the UI binds on 0.0.0.0.
      '';
    };

    dataDir = mkOption {
      type = types.path;
      default = "/var/lib/musicgpt";
      description = ''
        Data path used by MusicGPT (<code>--data-path</code>).
      '';
    };

    gpuEnabled = (mkEnableOption ''
      Enable GPU acceleration via CUDA (RoCM not supported currently).
    '') // { default = gpuEnabledDefault; };
  };

  config = mkIf cfg.enable {
    # Open the firewall if requested.
    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [ cfg.port ];

    systemd.services.musicgpt-ui = {
      description = "MusicGPT UI";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];

      # Use a dedicated state dir; systemd will create it with correct
      # ownership.
      serviceConfig = {
        ExecStart = let
          args = [
            # Run in UI mode, do not auto-open browser in daemon contexts.
            "--ui-no-open"
            "--ui-port" (toString cfg.port)
            "--model" cfg.model
            "--data-path" cfg.dataDir
          ]
          ++ lib.optionals cfg.useSplitDecoder [ "--use-split-decoder" ]
          ++ lib.optionals (cfg.openFirewall) [ "--ui-expose" ]
          ++ lib.optionals cfg.gpuEnabled [ "--gpu" ];
        in ''
          ${lib.getExe cfg.package} ${lib.escapeShellArgs args}
        '';

        DynamicUser = true;
        # not used directly (we pass dataDir), but harmless if different.
        StateDirectory = "musicgpt";
        WorkingDirectory = cfg.dataDir;
        Restart = "on-failure";
        # Harden a bit for a web app:
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        NoNewPrivileges = true;
        # Network is required for model fetches & UI:
      };
      # Ensure the data dir exists with sane perms when not under
      # StateDirectory:
      preStart = ''
        # Create data dir if the admin customized it away from
        # /var/lib/musicgpt.
        if [ ! -d "${cfg.dataDir}" ]; then
          # chown: set to runtime user/group created by DynamicUser at start;
          # fallback to root:root on first boot Note: DynamicUser makes a
          # transient user; hence we keep permissive root ownership and
          # directory mode 0750.  If you require persistent ownership, set a
          # static User/Group instead.
          mkdir --parents "${cfg.dataDir}"
          chmod 0750 "${cfg.dataDir}"
        fi
      '';
    };
  };
}
