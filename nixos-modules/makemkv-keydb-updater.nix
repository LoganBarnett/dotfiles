################################################################################
# Automatic MakeMKV AACS key database updater service.
#
# This module periodically downloads the community-maintained FindVUK key
# database and installs it as ~/.MakeMKV/KEYDB.cfg.  Without this file MakeMKV
# falls back to brute-forcing Blu-ray AACS encryption, which effectively hangs
# forever at 100 % CPU.
#
# The download source (fvonline-db.bplaced.net) is very slow, so no timeout is
# imposed on the curl transfer.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkEnableOption
    mkOption
    mkIf
    types
    ;
  cfg = config.services.makemkv-keydb-updater;

  updateScript = pkgs.writeShellScript "makemkv-update-keydb" ''
    set -euo pipefail

    USER_HOME=$(${pkgs.getent}/bin/getent passwd "${cfg.user}" | ${pkgs.coreutils}/bin/cut -d: -f6)
    SETTINGS_DIR="$USER_HOME/.MakeMKV"
    KEYDB_FILE="$SETTINGS_DIR/KEYDB.cfg"
    TEMP_DIR=$(${pkgs.coreutils}/bin/mktemp -d)

    trap '${pkgs.coreutils}/bin/rm -rf "$TEMP_DIR"' EXIT

    echo "Downloading AACS key database from FindVUK..."

    # Download the key database zip.  No --max-time: the server is very slow
    # and the file is ~22 MB, so the transfer can legitimately take minutes.
    if ! ${pkgs.curl}/bin/curl \
      --silent \
      --fail \
      --output "$TEMP_DIR/keydb.zip" \
      '${cfg.url}'; then
      echo "WARNING: Failed to download AACS key database" >&2
      if [[ -f "$KEYDB_FILE" ]]; then
        echo "Keeping existing KEYDB.cfg" >&2
        exit 0
      fi
      echo "ERROR: No existing KEYDB.cfg to fall back on" >&2
      exit 1
    fi

    # Extract the KEYDB.cfg from the zip.
    if ! ${pkgs.unzip}/bin/unzip -p "$TEMP_DIR/keydb.zip" > "$TEMP_DIR/KEYDB.cfg" 2>/dev/null; then
      echo "WARNING: Failed to extract KEYDB.cfg from zip" >&2
      if [[ -f "$KEYDB_FILE" ]]; then
        echo "Keeping existing KEYDB.cfg" >&2
        exit 0
      fi
      echo "ERROR: No existing KEYDB.cfg to fall back on" >&2
      exit 1
    fi

    # Basic sanity check: file should be non-empty and look like a key database.
    if [[ ! -s "$TEMP_DIR/KEYDB.cfg" ]]; then
      echo "WARNING: Extracted KEYDB.cfg is empty" >&2
      if [[ -f "$KEYDB_FILE" ]]; then
        echo "Keeping existing KEYDB.cfg" >&2
        exit 0
      fi
      echo "ERROR: No existing KEYDB.cfg to fall back on" >&2
      exit 1
    fi

    # Create settings directory if it doesn't exist.
    ${pkgs.coreutils}/bin/mkdir -p "$SETTINGS_DIR"
    ${pkgs.coreutils}/bin/chown "${cfg.user}:${cfg.group}" "$SETTINGS_DIR"

    # Atomically replace the key database.
    ${pkgs.coreutils}/bin/mv "$TEMP_DIR/KEYDB.cfg" "$KEYDB_FILE"
    ${pkgs.coreutils}/bin/chown "${cfg.user}:${cfg.group}" "$KEYDB_FILE"

    echo "AACS key database updated successfully"
  '';
in
{
  options.services.makemkv-keydb-updater = {
    enable = mkEnableOption "automatic MakeMKV AACS key database updater";

    user = mkOption {
      type = types.str;
      default = "root";
      description = ''
        User account whose MakeMKV settings directory receives the KEYDB.cfg.
      '';
    };

    group = mkOption {
      type = types.str;
      default = "root";
      description = ''
        Primary group of the user account.
      '';
    };

    url = mkOption {
      type = types.str;
      default = "http://fvonline-db.bplaced.net/export/keydb_eng.zip";
      description = ''
        URL of the FindVUK key database zip archive.
      '';
    };

    interval = mkOption {
      type = types.str;
      default = "30d";
      description = ''
        How often to refresh the key database.

        Accepts systemd time span format (e.g., "30d", "7d").
      '';
    };

    onCalendar = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "monthly";
      description = ''
        Optional calendar-based scheduling instead of interval-based.

        If set, this overrides the interval option.
      '';
    };
  };

  config = mkIf cfg.enable {
    systemd.services.makemkv-update-keydb = {
      description = "Update MakeMKV AACS Key Database";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];

      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${updateScript}";
        User = "root";

        # Security hardening.
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = "read-only";
        NoNewPrivileges = true;
        PrivateDevices = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
        ];
        RestrictNamespaces = true;
        LockPersonality = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        SystemCallArchitectures = "native";

        # Allow writing to user's MakeMKV directory.
        ReadWritePaths =
          if cfg.user == "root" then
            [ "/root/.MakeMKV" ]
          else
            [ "/home/${cfg.user}/.MakeMKV" ];
      };
    };

    systemd.timers.makemkv-update-keydb = {
      description = "MakeMKV AACS Key Database Update Timer";
      wantedBy = [ "timers.target" ];

      timerConfig =
        if cfg.onCalendar != null then
          {
            OnCalendar = cfg.onCalendar;
            Persistent = true;
          }
        else
          {
            OnBootSec = "5min";
            OnUnitActiveSec = cfg.interval;
            Persistent = true;
          };
    };
  };
}
