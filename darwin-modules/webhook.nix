################################################################################
# Darwin equivalent of nixpkgs's nixos/modules/services/networking/webhook.nix.
#
# Uses launchd instead of systemd.  Option names and types mirror the NixOS
# module so the two stay easy to compare and keep in sync.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.webhook;

  hookFormat = pkgs.formats.json { };

  hookType = lib.types.submodule (
    { name, ... }:
    {
      freeformType = hookFormat.type;
      options = {
        id = lib.mkOption {
          type = lib.types.str;
          default = name;
          description = ''
            The ID of your hook.  This value is used to create the HTTP
            endpoint (protocol://yourserver:port/prefix/''${id}).
          '';
        };
        execute-command = lib.mkOption {
          type = lib.types.str;
          description = "The command that should be executed when the hook is triggered.";
        };
      };
    }
  );

  hookFiles =
    lib.mapAttrsToList (
      name: hook: hookFormat.generate "webhook-${name}.json" [ hook ]
    ) cfg.hooks
    ++ lib.mapAttrsToList (
      name: hook: pkgs.writeText "webhook-${name}.json.tmpl" "[${hook}]"
    ) cfg.hooksTemplated;

in
{
  options.services.webhook = {
    enable = lib.mkEnableOption "webhook incoming webhook server";

    package = lib.mkPackageOption pkgs "webhook" { };

    ip = lib.mkOption {
      type = lib.types.str;
      default = "0.0.0.0";
      description = "The IP webhook should serve hooks on.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 9000;
      description = "The port webhook should be reachable from.";
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Register the webhook binary with the macOS Application Firewall so
        external callers can reach the server.

        The binary path is re-registered on every activation because it changes
        when the webhook derivation is updated.
      '';
    };

    urlPrefix = lib.mkOption {
      type = lib.types.str;
      default = "hooks";
      description = ''
        The URL path prefix to use for served hooks
        (protocol://yourserver:port/''${prefix}/hook-id).
      '';
    };

    enableTemplates = lib.mkOption {
      type = lib.types.bool;
      default = cfg.hooksTemplated != { };
      defaultText = lib.literalExpression "hooksTemplated != {}";
      description = ''
        Enable the generated hooks file to be parsed as a Go template.  See
        https://github.com/adnanh/webhook/blob/master/docs/Templates.md for
        more information.
      '';
    };

    hooks = lib.mkOption {
      type = lib.types.attrsOf hookType;
      default = { };
      example = {
        redeploy = {
          execute-command = "/var/scripts/redeploy.sh";
          command-working-directory = "/var/webhook";
        };
      };
      description = ''
        Hook definitions served by webhook.  Each attribute becomes an HTTP
        endpoint at protocol://host:port/''${urlPrefix}/''${name}.

        See https://github.com/adnanh/webhook/blob/master/docs/Hook-Definition.md
        for the full schema.
      '';
    };

    hooksTemplated = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = ''
        Same as hooks, but specified as literal JSON strings so they can
        include Go template syntax which is not representable as Nix values.
        Requires enableTemplates = true, which is set automatically when this
        option is non-empty.
      '';
    };

    verbose = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to show verbose output.";
    };

    extraArgs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "-secure" ];
      description = ''
        Extra arguments appended to the webhook invocation.  See
        https://github.com/adnanh/webhook/blob/master/docs/Webhook-Parameters.md.
      '';
    };

    environment = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = "Extra environment variables passed to webhook.";
    };

    secretEnvVars = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      example = {
        WEBHOOK_TOKEN = "/run/agenix/my-webhook-token";
      };
      description = ''
        Map from environment variable name to secret file path.  At daemon
        start, each file is read and its contents exported as the named
        variable.  Use this to pass agenix secret values into webhook's Go
        template engine without embedding secrets in the Nix store.

        Requires enableTemplates = true (set automatically when hooksTemplated
        is non-empty) for the injected values to be referenced in hook
        definitions.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions =
      let
        overlappingHooks = builtins.intersectAttrs cfg.hooks cfg.hooksTemplated;
      in
      [
        {
          assertion = hookFiles != [ ];
          message = "At least one hook needs to be configured for webhook to run.";
        }
        {
          assertion = overlappingHooks == { };
          message = ''
            `services.webhook.hooks` and `services.webhook.hooksTemplated` have
            overlapping attribute(s): ${lib.concatStringsSep ", " (builtins.attrNames overlappingHooks)}
          '';
        }
      ];

    environment.systemPackages = [ cfg.package ];

    system.activationScripts.postActivation.text = lib.mkIf cfg.openFirewall ''
      /usr/libexec/ApplicationFirewall/socketfilterfw \
        --add ${cfg.package}/bin/webhook >/dev/null 2>&1 || true
      /usr/libexec/ApplicationFirewall/socketfilterfw \
        --unblockapp ${cfg.package}/bin/webhook >/dev/null 2>&1 || true
    '';

    launchd.daemons.webhook =
      let
        webhookArgs = [
          "${cfg.package}/bin/webhook"
          "-ip"
          cfg.ip
          "-port"
          (toString cfg.port)
          "-urlprefix"
          cfg.urlPrefix
        ]
        ++ lib.concatMap (hook: [
          "-hooks"
          hook
        ]) hookFiles
        ++ lib.optional cfg.enableTemplates "-template"
        ++ lib.optional cfg.verbose "-verbose"
        ++ cfg.extraArgs;

        # When secretEnvVars is set, wrap the invocation in a shell script
        # that reads each secret file and exports its contents as the named
        # variable before exec-ing webhook.  This keeps secret values out of
        # the Nix store while making them available to webhook's Go template
        # engine at config load time.
        #
        # set -e is intentional: if any secret file is not yet available
        # (e.g. agenix hasn't placed it yet), the script exits non-zero and
        # launchd restarts it via KeepAlive until the file appears.
        wrapperScript = pkgs.writeShellScript "webhook-wrapper" ''
          set -euo pipefail
          ${lib.concatStringsSep "\n" (
            lib.mapAttrsToList (
              name: path: ''export ${name}="$(< ${path})"''
            ) cfg.secretEnvVars
          )}
          exec ${lib.escapeShellArgs webhookArgs}
        '';

        programArguments =
          if cfg.secretEnvVars == { } then
            webhookArgs
          else
            [
              "${pkgs.bash}/bin/bash"
              "${wrapperScript}"
            ];
      in
      {
        serviceConfig = {
          Label = "org.webhook.webhook";
          ProgramArguments = programArguments;
          EnvironmentVariables = cfg.environment;
          RunAtLoad = true;
          KeepAlive = true;
          StandardOutPath = "/var/log/webhook.log";
          StandardErrorPath = "/var/log/webhook.log";
        };
      };
  };
}
