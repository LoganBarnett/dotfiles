################################################################################
# This is the original NixOS Octoprint module taken from:
# https://github.com/loganbarnett/nixpkgs/blob/comfyui-fetch-model-hide-rebase/nixos/modules/services/misc/octoprint.nix
# This is so I could add logging capabilities to the module, and then hopefully
# contribute the changes back.  I also have reason to suspect that the
# configuration settings are not declarative but instead some kind of additive.
# I need to confirm this behavior, and perhaps submit another change for that
# too.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:
let

  cfg = config.services.octoprint;

  baseConfig = {
    plugins.curalegacy.cura_engine = "${pkgs.curaengine_stable}/bin/CuraEngine";
    server.port = cfg.port;
    webcam.ffmpeg = "${pkgs.ffmpeg.bin}/bin/ffmpeg";
  } // lib.optionalAttrs (cfg.host != null) { server.host = cfg.host; };

  fullConfig = lib.recursiveUpdate cfg.extraConfig baseConfig;

  cfgUpdate = pkgs.writeText "octoprint-config.yaml" (builtins.toJSON fullConfig);

  loggingConfig = pkgs.writeText "octoprint-logging.yaml" (builtins.toJSON {
    loggers = {
      # octoprint = {
      #   level = "DEBUG";
      # };
      "octoprint.plugins.auth_ldap" = {
        level = "DEBUG";
      };
    };
  });

  pluginsEnv = package.python.withPackages (ps: [ ps.octoprint ] ++ (cfg.plugins ps));

  package = pkgs.octoprint;

in
{
  ##### interface

  options = {

    services.octoprint = {

      enable = lib.mkEnableOption "OctoPrint, web interface for 3D printers";

      host = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Host to bind OctoPrint to.
        '';
      };

      port = lib.mkOption {
        type = lib.types.port;
        default = 5000;
        description = ''
          Port to bind OctoPrint to.
        '';
      };

      openFirewall = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Open ports in the firewall for OctoPrint.";
      };

      user = lib.mkOption {
        type = lib.types.str;
        default = "octoprint";
        description = "User for the daemon.";
      };

      group = lib.mkOption {
        type = lib.types.str;
        default = "octoprint";
        description = "Group for the daemon.";
      };

      stateDir = lib.mkOption {
        type = lib.types.path;
        default = "/var/lib/octoprint";
        description = "State directory of the daemon.";
      };

      plugins = lib.mkOption {
        type = lib.types.functionTo (lib.types.listOf lib.types.package);
        default = plugins: [ ];
        defaultText = lib.literalExpression "plugins: []";
        example = lib.literalExpression "plugins: with plugins; [ themeify stlviewer ]";
        description = "Additional plugins to be used. Available plugins are passed through the plugins input.";
      };

      extraConfig = lib.mkOption {
        type = lib.types.attrs;
        default = { };
        description = "Extra options which are added to OctoPrint's YAML configuration file.";
      };

      logging = lib.mkOption {
        # The entirety of the logging options can be found here:
        # https://docs.octoprint.org/en/master/configuration/logging_yaml.html
        # This should be discouraged.  There are known top level values for
        # these, and even the lower values have a known structure.
        # Top level:
        # - loggers
        # - handlers
        # - formatters
        # --
        # Loggers are key values where the key is a "component", such as
        # "octoprint.events", or "octoprint.plugins.backup".  Aside from
        # plugins, the components are well known but not documented entirely
        # (only the "important" ones are documented).
        # The value is simply an object with one key: "level", and its known
        # values can be "DEBUG" or... nothing else is documented.  This does use
        # https://docs.python.org/3/library/logging.handlers.html
        # for logging so maybe we can look there, though really it should be
        # documented.
        # Handler keys can only be (confirmed?) console, file, and serialFile.
        # The values are objects with class, level, formatter, when,
        # backupCount, and filename.  Example:
        # handlers:
        #   # stdout
        #   console:
        #     class: logging.StreamHandler
        #     level: DEBUG
        #     formatter: colored
        #     stream: ext://sys.stdout
        #
        #   # octoprint.log
        #   file:
        #     class: logging.handlers.TimedRotatingFileHandler
        #     level: DEBUG
        #     formatter: simple
        #     when: D
        #     backupCount: 1
        #     filename: /path/to/octoprints/logs/octoprint.log
        #
        #   # serial.log
        #   serialFile:
        #     class: logging.handlers.RotatingFileHandler
        #     level: DEBUG
        #     formatter: simple
        #     maxBytes: 2097152 # 2 * 1024 * 1024 = 2 MB in bytes
        #     filename: /path/to/octoprints/logs/serial.log
        # Handlers are documented partly here:
        # https://docs.python.org/3/library/logging.handlers.html#module-logging.handlers
        # formatters only has simple and colored.  They take a single format
        #   key, whose value is a format string.
        # Example:
        # formatters:
        #   simple:
        #     format: "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
        #   colored:
        #     format: "%(log_color)s%(asctime)s - %(name)s - %(levelname)s - %(message)s%(reset)s"
        # Although this document suggests there might be more keys available:
        # https://docs.python.org/3/library/logging.html#logrecord-attributes
        # I wonder if there's a validation for Python logger attributes
        # somewhere?
        type = lib.types.attrs;
        default = { };
        description = ''
          Logger settings.  See
          https://docs.octoprint.org/en/master/configuration/logging_yaml.html
          for available options.
        '';
        # TODO: Provide an example.
      };

    };

  };

  ##### implementation

  config = lib.mkIf cfg.enable {

    users.users = lib.optionalAttrs (cfg.user == "octoprint") {
      octoprint = {
        group = cfg.group;
        uid = config.ids.uids.octoprint;
      };
    };

    users.groups = lib.optionalAttrs (cfg.group == "octoprint") {
      octoprint.gid = config.ids.gids.octoprint;
    };

    systemd.tmpfiles.rules = [
      "d '${cfg.stateDir}' - ${cfg.user} ${cfg.group} - -"
      # this will allow octoprint access to raspberry specific hardware to check for throttling
      # read-only will not work: "VCHI initialization failed" error
      "a /dev/vchiq - - - - u:octoprint:rw"
    ];

    systemd.services.octoprint = {
      description = "OctoPrint, web interface for 3D printers";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      path = [ pluginsEnv ];

      preStart = ''
        if [ -e "${cfg.stateDir}/config.yaml" ]; then
          ${pkgs.yaml-merge}/bin/yaml-merge "${cfg.stateDir}/config.yaml" "${cfgUpdate}" > "${cfg.stateDir}/config.yaml.tmp"
          mv "${cfg.stateDir}/config.yaml.tmp" "${cfg.stateDir}/config.yaml"
        else
          cp "${cfgUpdate}" "${cfg.stateDir}/config.yaml"
          chmod 600 "${cfg.stateDir}/config.yaml"
        fi
        rm -f "${cfg.stateDir}/logging.yaml"
        cp "${loggingConfig}" "${cfg.stateDir}/logging.yaml"
      '';

      serviceConfig = {
        ExecStart = "${pluginsEnv}/bin/octoprint serve -b ${cfg.stateDir}";
        User = cfg.user;
        Group = cfg.group;
        SupplementaryGroups = [
          "dialout"
        ];
      };
    };

    networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall [ cfg.port ];
  };
}
