################################################################################
# Gitea webhook configuration for deployment server.
#
# This configuration should be imported on the Gitea host (copper) to set up
# webhooks that notify the deployment server (rubidium) of repository changes.
################################################################################
{
  config,
  facts,
  lib,
  ...
}:
{
  imports = [
    ../nixos-modules/gitea-webhook-setup.nix
  ];

  # Shared webhook secret with deployment server.
  age.secrets.deployment-webhook-secret = {
    generator.script = "long-passphrase";
    owner = "gitea";
    group = "gitea";
    mode = "0400";
  };

  # Configure webhooks for the deployment server.
  services.giteaWebhookSetup = {
    enable = true;

    webhooks = [
      {
        owner = "logan";
        repo = "dotfiles";
        url = "http://rubidium.${facts.network.domain}:9001/webhook";
        events = [ "push" ];
        secretFile = config.age.secrets.deployment-webhook-secret.path;
      }
    ];
  };
}
