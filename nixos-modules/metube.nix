################################################################################
# Metube is a web frontend for yt-dlp.  This module wraps the metube package
# with site-specific integration: reverse proxy registration, systemd mount
# ordering, and media group membership.
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
    mkIf
    mkOption
    optional
    optionalAttrs
    types
    ;
  cfg = config.services.metube-host;
  bgutilDep = optional cfg.bgutil.enable "bgutil-pot.service";
  # Per-file post-download script.  Used both by the YTDL_OPTIONS exec hook
  # (invoked immediately after each new download) and by the directory-scan
  # service below (for backfill on existing files).  Reads the original URL
  # from the yt-dlp info.json sidecar.  Handles two concerns:
  #
  #   1. Thumbnail — downloads if no jpg/webp/png companion exists.
  #      Future downloads already have thumbnails written by YTDL_OPTIONS
  #      writethumbnail, so this is mainly a backfill path.
  #
  #   2. H.264+AAC MP4 compat copy — downloads if .compat.mp4 is absent.
  compatDownloadExecScript = pkgs.writeShellScript "metube-compat-download-exec" ''
    set -euo pipefail
    input="$1"
    # Only WebM files need post-processing; skip everything else.
    [[ "$input" == *.webm ]] || exit 0
    stem="''${input%.webm}"
    info_json="$stem.info.json"
    [[ -f "$info_json" ]] || exit 0
    url=$(${pkgs.jq}/bin/jq -r '.webpage_url // empty' "$info_json")
    if [[ -z "$url" ]]; then
      echo "No webpage_url in $info_json; skipping." >&2
      exit 0
    fi

    # Download thumbnail if none of the expected extensions are present.
    if [[ ! -f "$stem.jpg" ]] && [[ ! -f "$stem.webp" ]] && [[ ! -f "$stem.png" ]]; then
      ${pkgs.yt-dlp}/bin/yt-dlp \
        --write-thumbnail \
        --skip-download \
        --no-write-info-json \
        --no-playlist \
        -o "$stem.%(ext)s" \
        "$url" \
        || echo "Thumbnail download failed for $input; continuing." >&2
    fi

    # Download H.264+AAC MP4 compat copy if absent.
    compat_base="$stem.compat"
    if [[ ! -f "$compat_base.mp4" ]]; then
      ${pkgs.yt-dlp}/bin/yt-dlp \
        --format "bestvideo[vcodec^=avc1]+bestaudio[ext=m4a]/bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best" \
        --merge-output-format mp4 \
        --no-write-info-json \
        --no-write-thumbnail \
        --no-playlist \
        -o "$compat_base.%(ext)s" \
        "$url"
    fi
  '';
  # Scans the entire download directory and calls compatDownloadExecScript for
  # each WebM file that is missing a companion compat copy.  Errors for
  # individual files are logged but do not abort the scan.
  compatDownloadScript = pkgs.writeShellScript "metube-compat-download" ''
    set -uo pipefail
    shopt -s globstar nullglob
    for f in ${lib.escapeShellArg cfg.downloadDir}/**/*.webm; do
      ${compatDownloadExecScript} "$f" \
        || echo "Compat download failed for $f; skipping." >&2
    done
  '';
in
{
  imports = [ ./https.nix ];

  options.services.metube-host = {
    enable = mkEnableOption "Metube web frontend for yt-dlp";

    package = mkOption {
      type = types.package;
      default = pkgs.metube;
      description = "The metube package to use.";
    };

    port = mkOption {
      type = types.port;
      default = 8090;
      description = "Internal HTTP port for the metube service.";
    };

    downloadDir = mkOption {
      type = types.path;
      description = "Directory where downloaded media is stored.";
    };

    fqdn = mkOption {
      type = types.str;
      default = "metube.proton";
      description = "FQDN for the HTTPS reverse proxy endpoint.";
    };

    mountDependencies = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = ''
        Systemd units that must be active before metube starts.  Listed in
        both <code>after</code> and <code>requires</code>.
      '';
    };

    extraGroups = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Additional groups for the metube service user.";
    };

    includeHighCompatibilityCopy = mkOption {
      type = types.bool;
      default = true;
      description = ''
        After each download, fetch an H.264+AAC MP4 copy of the video
        alongside the original file (e.g. foo.compat.mp4 next to foo.webm).
        Enables playback on Safari (M1/M2/Intel) and other browsers that
        lack AV1 or WebM support.

        The copy is downloaded directly from the source rather than
        transcoded, so it completes in roughly the same time as the original
        download.  A systemd path unit also watches the download directory
        and triggers a backfill scan on startup and after each new download,
        catching any files that are missing a compatibility copy.

        Warning: this keeps two copies of every video on disk.
      '';
    };

    bgutil = {
      enable =
        mkEnableOption "bgutil PO token provider sidecar for YouTube bot-check bypass"
        // {
          default = true;
        };

      package = mkOption {
        type = types.package;
        default = pkgs.bgutil-pot;
        description = "The bgutil-pot package to use.";
      };
    };
  };

  config = mkIf cfg.enable {
    services.https.fqdns.${cfg.fqdn} = {
      enable = true;
      internalPort = cfg.port;
    };

    users.groups.metube = { };

    users.users.metube = {
      isSystemUser = true;
      group = "metube";
      extraGroups = cfg.extraGroups;
    };

    users.groups.bgutil-pot = mkIf cfg.bgutil.enable { };

    users.users.bgutil-pot = mkIf cfg.bgutil.enable {
      isSystemUser = true;
      group = "bgutil-pot";
    };

    systemd.services.bgutil-pot = mkIf cfg.bgutil.enable {
      description = "bgutil PO token provider for yt-dlp";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = "${lib.getExe cfg.bgutil.package} server --host 127.0.0.1";
        User = "bgutil-pot";
        Group = "bgutil-pot";
        StateDirectory = "bgutil-pot";
        Restart = "on-failure";
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadWritePaths = [ "/var/lib/bgutil-pot" ];
      };
    };

    systemd.services.metube = {
      description = "MeTube web frontend for yt-dlp";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ] ++ cfg.mountDependencies ++ bgutilDep;
      requires = cfg.mountDependencies ++ bgutilDep;
      environment = {
        DOWNLOAD_DIR = cfg.downloadDir;
        HOST = "127.0.0.1";
        PORT = toString cfg.port;
        STATE_DIR = "/var/lib/metube";
        # yt-dlp caches the EJS challenge solver script here on first use
        # (requires remote_components below).  Without an explicit cache dir,
        # yt-dlp would try to write to ~/.cache which does not exist for a
        # system user.
        XDG_CACHE_HOME = "/var/lib/metube/.cache";
        # Enable downloading the EJS challenge solver script from GitHub.  The
        # bundled yt-dlp-ejs Python package in nixpkgs lags behind the version
        # required by current yt-dlp; this lets yt-dlp fetch the correct
        # version automatically and cache it for subsequent runs.
        #
        # When includeHighCompatibilityCopy is enabled, the exec postprocessor
        # runs compatDownloadExecScript after each download to fetch the H.264
        # compat copy while yt-dlp's authentication context is still active.
        YTDL_OPTIONS = builtins.toJSON (
          {
            remote_components = [ "ejs:github" ];
            writethumbnail = true;
            writeinfojson = true;
          }
          // optionalAttrs cfg.includeHighCompatibilityCopy {
            postprocessors = [
              {
                key = "Exec";
                exec_cmd = "${compatDownloadExecScript} {}";
                when = "after_move";
              }
            ];
          }
        );
      };
      serviceConfig = {
        ExecStart = lib.getExe cfg.package;
        User = "metube";
        Group = "metube";
        StateDirectory = "metube";
        Restart = "on-failure";
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadWritePaths = [
          cfg.downloadDir
          "/var/lib/metube"
        ];
      };
    };

    # Scans the download directory for WebM files missing a compat copy and
    # downloads them.  Runs at startup to backfill any existing files and is
    # re-triggered by the path unit below after each new download.
    systemd.services.metube-compat-download =
      mkIf cfg.includeHighCompatibilityCopy
        {
          description = "Download H.264+AAC MP4 compat copies for metube WebM files";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ] ++ cfg.mountDependencies;
          requires = cfg.mountDependencies;
          serviceConfig = {
            Type = "oneshot";
            ExecStart = toString compatDownloadScript;
            User = "metube";
            Group = "metube";
            ProtectSystem = "strict";
            ProtectHome = true;
            ReadWritePaths = [ cfg.downloadDir ];
          };
        };

    # Watches the download directory and re-triggers the download service
    # whenever files change (i.e. after each completed download), as a safety
    # net for any files the exec hook may have missed.
    systemd.paths.metube-compat-download = mkIf cfg.includeHighCompatibilityCopy {
      description = "Watch for new metube downloads to fetch compat copies for";
      wantedBy = [ "multi-user.target" ];
      after = cfg.mountDependencies;
      requires = cfg.mountDependencies;
      pathConfig = {
        PathModified = cfg.downloadDir;
        Unit = "metube-compat-download.service";
      };
    };
  };
}
