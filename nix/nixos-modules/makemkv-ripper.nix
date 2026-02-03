################################################################################
# Automatic DVD/Blu-ray ripper using MakeMKV.
#
# This module provides automatic disc ripping triggered by disc insertion via
# udev rules, or manual invocation via systemd.  It extracts disc metadata to
# generate filenames, checks for existing rips to avoid duplicates, and stores
# the output in a configurable directory (defaults to NFS-mounted Kodi media).
#
# The service runs in a hardened systemd sandbox with minimal filesystem and
# network access.
#
# Usage:
#   Manual trigger: systemctl start makemkv-rip@sr0
#   Auto-trigger: Insert disc (if autoRip is enabled)
#   Check status: systemctl status makemkv-rip@sr0
################################################################################
{ config, lib, pkgs, ... }: let
  inherit (lib) mkEnableOption mkOption mkIf types;
  cfg = config.services.makemkv-ripper;

  ripScript = pkgs.writeShellScript "makemkv-rip" ''
    set -euo pipefail

    DEVICE="''${1:-${cfg.defaultDevice}}"
    OUTPUT_DIR="${cfg.outputPath}"
    TEMP_DIR=$(${pkgs.coreutils}/bin/mktemp -d)

    trap '${pkgs.coreutils}/bin/rm -rf "$TEMP_DIR"' EXIT

    log() {
      echo "[$(${pkgs.coreutils}/bin/date '+%Y-%m-%d %H:%M:%S')] $*"
    }

    error() {
      log "ERROR: $*" >&2
    }

    # Verify device exists.
    if [[ ! -e "$DEVICE" ]]; then
      error "Device $DEVICE does not exist"
      exit 1
    fi

    log "Starting rip from device: $DEVICE"
    log "Output directory: $OUTPUT_DIR"

    # Query disc information to extract metadata.
    log "Querying disc metadata..."
    if ! ${pkgs.makemkv}/bin/makemkvcon -r --cache=1 info "dev:$DEVICE" > "$TEMP_DIR/disc-info.txt" 2>&1; then
      error "Failed to query disc information"
      ${pkgs.coreutils}/bin/cat "$TEMP_DIR/disc-info.txt" >&2
      exit 1
    fi

    # Extract disc title from MakeMKV output.
    # MakeMKV outputs lines like: CINFO:2,0,"DISC_TITLE"
    DISC_TITLE=$(${pkgs.gnugrep}/bin/grep -oP 'CINFO:2,0,"\K[^"]+' "$TEMP_DIR/disc-info.txt" || echo "Unknown")

    # Clean up title for filesystem use (remove special characters).
    CLEAN_TITLE=$(echo "$DISC_TITLE" | ${pkgs.gnused}/bin/sed 's/[^a-zA-Z0-9 _-]//g' | ${pkgs.gnused}/bin/sed 's/  */ /g' | ${pkgs.coreutils}/bin/tr -s ' ' '_')

    if [[ -z "$CLEAN_TITLE" || "$CLEAN_TITLE" == "Unknown" ]]; then
      # Fallback to disc serial or timestamp if no title found.
      DISC_SERIAL=$(${pkgs.gnugrep}/bin/grep -oP 'CINFO:32,0,"\K[^"]+' "$TEMP_DIR/disc-info.txt" || echo "")
      if [[ -n "$DISC_SERIAL" ]]; then
        CLEAN_TITLE="Disc_$DISC_SERIAL"
      else
        CLEAN_TITLE="Disc_$(${pkgs.coreutils}/bin/date +%Y%m%d_%H%M%S)"
      fi
    fi

    OUTPUT_FILE="$OUTPUT_DIR/$CLEAN_TITLE.mkv"

    log "Disc title: $DISC_TITLE"
    log "Output file: $OUTPUT_FILE"

    # Check if file already exists.
    if [[ -f "$OUTPUT_FILE" ]]; then
      log "File already exists, skipping rip: $OUTPUT_FILE"
      ${lib.optionalString cfg.ejectWhenComplete ''
        log "Ejecting disc..."
        ${pkgs.util-linux}/bin/eject "$DEVICE" || log "Warning: Failed to eject disc"
      ''}
      exit 0
    fi

    # Ensure output directory exists.
    ${pkgs.coreutils}/bin/mkdir -p "$OUTPUT_DIR"

    # Rip all titles from the disc.
    log "Starting rip process..."
    if ! ${pkgs.makemkv}/bin/makemkvcon mkv \
      --cache=512 \
      --noscan \
      --progress=-same \
      "dev:$DEVICE" \
      all \
      "$TEMP_DIR" 2>&1 | ${pkgs.coreutils}/bin/tee "$TEMP_DIR/rip.log"; then
      error "Rip failed"
      ${pkgs.coreutils}/bin/cat "$TEMP_DIR/rip.log" >&2
      exit 1
    fi

    # Move all ripped files to final destination.
    # MakeMKV names files as title_t00.mkv, title_t01.mkv, etc.
    RIPPED_FILES=$(${pkgs.findutils}/bin/find "$TEMP_DIR" -name "*.mkv" -type f)

    if [[ -z "$RIPPED_FILES" ]]; then
      error "No MKV files found after rip"
      exit 1
    fi

    FILE_COUNT=$(echo "$RIPPED_FILES" | ${pkgs.coreutils}/bin/wc -l)
    log "Moving $FILE_COUNT ripped file(s) to output directory"

    # Move each file, prepending the disc title to the original filename.
    while IFS= read -r RIPPED_FILE; do
      BASENAME=$(${pkgs.coreutils}/bin/basename "$RIPPED_FILE")
      OUTPUT_FILE="$OUTPUT_DIR/''${CLEAN_TITLE}_$BASENAME"

      log "Moving: $OUTPUT_FILE"
      ${pkgs.coreutils}/bin/mv "$RIPPED_FILE" "$OUTPUT_FILE"

      # Set ownership.
      ${pkgs.coreutils}/bin/chown ${cfg.outputUser}:${cfg.outputGroup} "$OUTPUT_FILE"
    done <<< "$RIPPED_FILES"

    log "Rip completed successfully: $FILE_COUNT file(s) in $OUTPUT_DIR"

    ${lib.optionalString cfg.ejectWhenComplete ''
      log "Ejecting disc..."
      ${pkgs.util-linux}/bin/eject "$DEVICE" || log "Warning: Failed to eject disc"
    ''}

    ${lib.optionalString (cfg.notifyCommand != null) ''
      log "Sending notification..."
      ${cfg.notifyCommand} "MakeMKV rip complete" "$CLEAN_TITLE has been ripped to $OUTPUT_FILE" || log "Warning: Notification failed"
    ''}
  '';
in {
  options.services.makemkv-ripper = {
    enable = mkEnableOption "MakeMKV automatic disc ripper";

    outputPath = mkOption {
      type = types.path;
      default = "/mnt/kodi-media";
      description = ''
        Directory where ripped MKV files will be stored.

        Defaults to the Kodi media directory on NFS mount.
      '';
    };

    defaultDevice = mkOption {
      type = types.str;
      default = "/dev/sr0";
      description = ''
        Default optical drive device to use when not specified.

        Can be overridden by passing device to systemd service:
        systemctl start makemkv-rip@sr1
      '';
    };

    autoRip = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Automatically start ripping when a disc is inserted.

        When enabled, udev rules will trigger the rip service on disc insertion.
        When disabled, rips must be manually triggered via systemd.
      '';
    };

    ejectWhenComplete = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Automatically eject the disc after ripping completes.
      '';
    };

    outputUser = mkOption {
      type = types.str;
      default = "root";
      description = ''
        User to own the output files.
      '';
    };

    outputGroup = mkOption {
      type = types.str;
      default = "users";
      description = ''
        Group to own the output files.
      '';
    };

    notifyCommand = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "\${pkgs.libnotify}/bin/notify-send";
      description = ''
        Optional command to run for notifications when rip completes.

        The command will be called with two arguments:
        1. Summary (e.g., "MakeMKV rip complete")
        2. Body (e.g., "Movie_Title has been ripped to /path/file.mkv")

        Example: Use notify-send for desktop notifications or a webhook script.
      '';
    };

    allowedNetworkHosts = mkOption {
      type = types.listOf types.str;
      default = [
        "forum.makemkv.com"
      ];
      description = ''
        List of hostnames that MakeMKV is allowed to contact.

        Used for online metadata lookups and beta key validation.
        Set to empty list to disable all network access.
      '';
    };
  };

  config = mkIf cfg.enable {
    # Install MakeMKV system-wide.
    environment.systemPackages = [ pkgs.makemkv ];

    # Load sg (SCSI generic) kernel module for optical drive access.
    # MakeMKV requires access to /dev/sg* devices for proper operation.
    boot.kernelModules = [ "sg" ];

    # Template service that takes device as parameter (e.g., sr0, sr1).
    systemd.services."makemkv-rip@" = {
      description = "MakeMKV Disc Ripper for %I";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];

      # Make the device name available as an environment variable.
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${ripScript} /dev/%I";
        TimeoutStartSec = "4h";  # Long timeout for large discs.

        # Run as root to access optical drive and write to output directory.
        User = "root";

        # Security hardening.
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        NoNewPrivileges = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        PrivateNetwork = true;  # Disable all network access.
        RestrictNamespaces = true;
        LockPersonality = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        SystemCallArchitectures = "native";
        MemoryDenyWriteExecute = false;  # MakeMKV needs this for JIT.

        # Allow access to optical drives and output directory.
        DevicePolicy = "closed";
        DeviceAllow = [
          "block-sr rwm"  # Block devices sr* (optical drives).
          "char-sg rwm"  # SCSI generic devices (required for MakeMKV).
          "char-usb_device rw"  # For USB optical drives.
        ];
        ReadWritePaths = [ cfg.outputPath ];
        # Allow reading MakeMKV settings (including beta key).
        ReadOnlyPaths = [ "/root/.MakeMKV" ];
      };
    };

    # Udev rules to auto-trigger rip on disc insertion.
    services.udev.extraRules = mkIf cfg.autoRip ''
      # Trigger on DVD insertion.
      ACTION=="change", SUBSYSTEM=="block", KERNEL=="sr[0-9]*", \
        ENV{ID_CDROM_MEDIA_DVD}=="1", \
        TAG+="systemd", ENV{SYSTEMD_WANTS}="makemkv-rip@$kernel.service"

      # Trigger on Blu-ray insertion.
      ACTION=="change", SUBSYSTEM=="block", KERNEL=="sr[0-9]*", \
        ENV{ID_CDROM_MEDIA_BD}=="1", \
        TAG+="systemd", ENV{SYSTEMD_WANTS}="makemkv-rip@$kernel.service"
    '';
  };
}
