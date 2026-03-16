################################################################################
# Gitea webhook setup module.
#
# This module runs on the Gitea host and configures webhooks using direct
# database access.  This avoids the need for API tokens that become invalid
# after rebuilds.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.giteaWebhookSetup;

  # Script to setup a single webhook.
  setupWebhookScript = pkgs.writeShellApplication {
    name = "gitea-setup-webhook";
    runtimeInputs = with pkgs; [
      config.services.gitea.package
      postgresql
      gawk
      gnugrep
      coreutils
    ];
    text = builtins.readFile ../scripts/gitea-setup-webhook;
  };

  webhookType = types.submodule {
    options = {
      owner = mkOption {
        type = types.str;
        description = "Repository owner (user or organization)";
      };

      repo = mkOption {
        type = types.str;
        description = "Repository name";
      };

      url = mkOption {
        type = types.str;
        description = "Webhook URL to call";
      };

      events = mkOption {
        type = types.listOf types.str;
        default = [ "push" ];
        description = "List of events that trigger the webhook";
      };

      secretFile = mkOption {
        type = types.path;
        description = "Path to file containing webhook secret";
      };
    };
  };
in
{
  options.services.giteaWebhookSetup = {
    enable = mkEnableOption "Gitea webhook setup via local CLI";

    webhooks = mkOption {
      type = types.listOf webhookType;
      default = [ ];
      description = "List of webhooks to configure";
    };
  };

  config = mkIf (cfg.enable && config.services.gitea.enable) {
    # Create a systemd service that runs on the Gitea host.
    systemd.services.gitea-webhook-setup = {
      description = "Configure Gitea webhooks via database";
      after = [ "gitea.service" ];
      wants = [ "gitea.service" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        User = "gitea";
      };

      script = ''
        echo "Setting up Gitea webhooks..."

        # Configure each webhook by invoking the setup script.
        ${concatMapStringsSep "\n" (webhook: ''
          ${setupWebhookScript}/bin/gitea-setup-webhook \
            "${webhook.owner}" \
            "${webhook.repo}" \
            "${webhook.url}" \
            "${webhook.secretFile}" \
            "${concatStringsSep "," webhook.events}"
        '') cfg.webhooks}

        echo "All webhooks configured successfully"
      '';
    };
  };
}
