################################################################################
# Automatic MakeMKV beta key updater service.
#
# This module provides automatic updates of the MakeMKV beta key by scraping the
# official forum page and updating the user's settings.conf file.  The beta key
# is required for Blu-ray ripping functionality and expires approximately every
# 30 days.
#
# The service runs as the specified user and updates ~/.MakeMKV/settings.conf
# with the latest beta key from the MakeMKV forum.
################################################################################
{ config, lib, pkgs, ... }: let
  inherit (lib) mkEnableOption mkOption mkIf types;
  cfg = config.services.makemkv-updater;

  updateScript = pkgs.writeShellScript "makemkv-update-beta-key" ''
    set -euo pipefail

    USER_HOME=$(${pkgs.getent}/bin/getent passwd "${cfg.user}" | ${pkgs.coreutils}/bin/cut -d: -f6)
    SETTINGS_DIR="$USER_HOME/.MakeMKV"
    SETTINGS_FILE="$SETTINGS_DIR/settings.conf"
    TEMP_FILE=$(${pkgs.coreutils}/bin/mktemp)

    trap '${pkgs.coreutils}/bin/rm -f "$TEMP_FILE"' EXIT

    echo "Fetching latest MakeMKV beta key from forum..."

    # Download the forum page containing the beta key.
    if ! ${pkgs.curl}/bin/curl \
      --silent \
      --fail \
      --max-time 30 \
      --output "$TEMP_FILE" \
      'https://forum.makemkv.com/forum/viewtopic.php?f=5&t=1053'; then
      echo "ERROR: Failed to download beta key page" >&2
      exit 1
    fi

    # Extract the beta key from <code> tags in the HTML.
    BETA_KEY=$(${pkgs.gnused}/bin/sed -n 's|.*<code>\(T-[^<]*\)</code>.*|\1|p' "$TEMP_FILE" | ${pkgs.coreutils}/bin/head -n1)

    if [[ -z "$BETA_KEY" ]]; then
      echo "ERROR: Could not extract beta key from forum page" >&2
      exit 1
    fi

    if [[ ! "$BETA_KEY" =~ ^T- ]]; then
      echo "ERROR: Extracted key does not match expected format: $BETA_KEY" >&2
      exit 1
    fi

    echo "Found beta key: $BETA_KEY"

    # Create settings directory if it doesn't exist.
    ${pkgs.coreutils}/bin/mkdir -p "$SETTINGS_DIR"
    ${pkgs.coreutils}/bin/chown "${cfg.user}:${cfg.group}" "$SETTINGS_DIR"

    # Update or create the settings file with the new beta key.
    if [[ -f "$SETTINGS_FILE" ]]; then
      # Settings file exists; update the app_Key line.
      if ${pkgs.gnugrep}/bin/grep -q '^app_Key' "$SETTINGS_FILE"; then
        # Replace existing app_Key line.
        ${pkgs.gnused}/bin/sed -i "s|^app_Key.*|app_Key = \"$BETA_KEY\"|" "$SETTINGS_FILE"
        echo "Updated existing beta key in $SETTINGS_FILE"
      else
        # Append app_Key to existing file.
        echo "app_Key = \"$BETA_KEY\"" >> "$SETTINGS_FILE"
        echo "Added beta key to $SETTINGS_FILE"
      fi

      # Disable automatic update checking.
      if ${pkgs.gnugrep}/bin/grep -q '^app_UpdateEnable' "$SETTINGS_FILE"; then
        ${pkgs.gnused}/bin/sed -i "s|^app_UpdateEnable.*|app_UpdateEnable = \"0\"|" "$SETTINGS_FILE"
      else
        echo "app_UpdateEnable = \"0\"" >> "$SETTINGS_FILE"
      fi
    else
      # Create new settings file with beta key and update checking disabled.
      {
        echo "app_Key = \"$BETA_KEY\""
        echo "app_UpdateEnable = \"0\""
      } > "$SETTINGS_FILE"
      ${pkgs.coreutils}/bin/chown "${cfg.user}:${cfg.group}" "$SETTINGS_FILE"
      echo "Created $SETTINGS_FILE with beta key and updates disabled"
    fi

    echo "MakeMKV beta key updated successfully"
  '';
in {
  options.services.makemkv-updater = {
    enable = mkEnableOption "automatic MakeMKV beta key updater";

    user = mkOption {
      type = types.str;
      default = "logan";
      description = ''
        User account whose MakeMKV settings should be updated.

        The beta key will be written to this user's ~/.MakeMKV/settings.conf file.
      '';
    };

    group = mkOption {
      type = types.str;
      default = "users";
      description = ''
        Primary group of the user account.

        Used to set correct ownership of the settings file.
      '';
    };

    interval = mkOption {
      type = types.str;
      default = "30d";
      description = ''
        How often to check for and update the beta key.

        Accepts systemd time span format (e.g., "30d", "4w", "1month").
        Default is 30 days to match the typical beta key expiration period.
      '';
    };

    onCalendar = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "monthly";
      description = ''
        Optional calendar-based scheduling instead of interval-based.

        If set, this overrides the interval option and uses systemd calendar
        syntax (e.g., "monthly", "weekly", "Mon *-*-* 00:00:00").

        Leave null to use interval-based scheduling.
      '';
    };
  };

  config = mkIf cfg.enable {
    # Pre-create the MakeMKV settings directory so systemd can bind-mount it.
    systemd.tmpfiles.rules = [
      (if cfg.user == "root" then
        "d /root/.MakeMKV 0755 root ${cfg.group} -"
      else
        "d /home/${cfg.user}/.MakeMKV 0755 ${cfg.user} ${cfg.group} -")
    ];

    systemd.services.makemkv-update-beta-key = {
      description = "Update MakeMKV Beta Key";
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
        RestrictAddressFamilies = [ "AF_INET" "AF_INET6" ];
        RestrictNamespaces = true;
        LockPersonality = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        SystemCallArchitectures = "native";

        # Allow writing to user's home directory for settings file.
        # Use conditional path based on whether user is root.
        ReadWritePaths = if cfg.user == "root" then [ "/root/.MakeMKV" ] else [ "/home/${cfg.user}/.MakeMKV" ];
      };
    };

    systemd.timers.makemkv-update-beta-key = {
      description = "MakeMKV Beta Key Update Timer";
      wantedBy = [ "timers.target" ];

      timerConfig = if cfg.onCalendar != null then {
        OnCalendar = cfg.onCalendar;
        Persistent = true;
      } else {
        OnBootSec = "5min";
        OnUnitActiveSec = cfg.interval;
        Persistent = true;
      };
    };
  };
}
