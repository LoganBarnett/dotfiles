################################################################################
# Nix deployment server module.
#
# This module turns a NixOS host into an automatic deployment server that:
# 1. Watches a Git repository for changes
# 2. Evaluates if any host configurations would change
# 3. Automatically deploys updates to affected hosts
# 4. Tracks deployment status for external tools (like Claude)
################################################################################
{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.nixDeploymentServer;

  checkHostChanged = pkgs.writeShellApplication {
    name = "check-host-changed";
    text = builtins.readFile ../scripts/check-host-changed.sh;
  };

  deployHost = pkgs.writeShellApplication {
    name = "deploy-host";
    text = builtins.readFile ../scripts/deploy-host.sh;
  };

  # Hosts are passed as positional arguments; repository path and status
  # directory come from the environment.
  deploymentOrchestrator = pkgs.writeShellApplication {
    name = "deployment-orchestrator";
    runtimeInputs = [ checkHostChanged deployHost pkgs.git ];
    text = builtins.readFile ../scripts/deployment-orchestrator.sh;
  };
in {
  options.services.nixDeploymentServer = {
    enable = mkEnableOption "Nix deployment server";

    repositoryPath = mkOption {
      type = types.path;
      default = "/var/lib/nix-deployment/dotfiles";
      description = ''
        Path to the local clone of the dotfiles repository.
      '';
    };

    repositoryUrl = mkOption {
      type = types.str;
      example = "git+ssh://git@gitea.proton:2222/logan/dotfiles.git";
      description = ''
        Git URL of the dotfiles repository to watch.
      '';
    };

    hosts = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        List of hostnames to manage deployments for.
      '';
    };

    statusDirectory = mkOption {
      type = types.path;
      default = "/var/lib/nix-deployment/status";
      description = ''
        Directory where deployment status files are stored.
      '';
    };

    webhookPort = mkOption {
      type = types.port;
      default = 9001;
      description = ''
        Port for the webhook receiver to listen on.
      '';
    };

    webhookSecretFile = mkOption {
      type = types.path;
      description = ''
        Path to file containing the webhook secret.  Typically provided by
        agenix.
      '';
    };
  };

  config = mkIf cfg.enable {
    # Create necessary directories.
    systemd.tmpfiles.rules = [
      "d ${cfg.repositoryPath} 0755 nix-deploy nix-deploy -"
      "d ${cfg.statusDirectory} 0755 nix-deploy nix-deploy -"
      "d /var/lib/nix-deployment 0755 nix-deploy nix-deploy -"
    ];

    # Create a dedicated user for deployments.
    users.users.nix-deploy = {
      isSystemUser = true;
      group = "nix-deploy";
      home = "/var/lib/nix-deployment";
      createHome = true;
      shell = pkgs.bash;
      openssh.authorizedKeys.keys = config.users.users.logan.openssh.authorizedKeys.keys;
    };

    users.groups.nix-deploy = {};

    # Allow nix-deploy user to perform deployments.
    nix.settings.trusted-users = [ "nix-deploy" ];

    # SSH configuration for the deployment user.
    programs.ssh.extraConfig = ''
      Host ${concatStringsSep " " cfg.hosts}
        User root
        StrictHostKeyChecking accept-new
    '';

    # Initial repository clone.
    systemd.services.nix-deployment-init = {
      description = "Initialize Nix deployment repository";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      path = with pkgs; [ git openssh ];

      serviceConfig = {
        Type = "oneshot";
        User = "nix-deploy";
        Group = "nix-deploy";
        RemainAfterExit = true;
      };

      script = ''
        if [[ ! -d "${cfg.repositoryPath}/.git" ]]; then
          echo "Cloning repository..."
          git clone "${cfg.repositoryUrl}" "${cfg.repositoryPath}"
        else
          echo "Repository already exists."
        fi
      '';
    };

    # Webhook receiver service.
    systemd.services.nix-deployment-webhook = {
      description = "Nix deployment webhook receiver";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" "nix-deployment-init.service" ];

      path = with pkgs; [
        socat
        (writeShellApplication {
          name = "webhook-receiver";
          runtimeInputs = [ coreutils jq systemd ];
          text = builtins.readFile ../scripts/webhook-receiver.sh;
        })
      ];

      serviceConfig = {
        Type = "simple";
        User = "nix-deploy";
        Group = "nix-deploy";
        Restart = "always";
        RestartSec = 5;
      };

      script = ''
        # Simple webhook receiver using socat.
        socat TCP-LISTEN:${toString cfg.webhookPort},reuseaddr,fork \
          EXEC:webhook-receiver
      '';
    };

    # Deployment check service (triggered by webhook).
    systemd.services.nix-deployment-check = {
      description = "Check and deploy Nix configuration changes";

      environment = {
        NIX_DEPLOYMENT_REPO_PATH = toString cfg.repositoryPath;
        NIX_DEPLOYMENT_STATUS_DIR = toString cfg.statusDirectory;
      };

      path = with pkgs; [ openssh nix coreutils deploymentOrchestrator ];

      serviceConfig = {
        Type = "oneshot";
        User = "nix-deploy";
        Group = "nix-deploy";
      };

      script = ''
        exec deployment-orchestrator \
          ${concatStringsSep " " (map escapeShellArg cfg.hosts)}
      '';
    };

    # Timer for periodic checks (backup for webhook).
    systemd.timers.nix-deployment-check = {
      description = "Periodic Nix deployment check";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnBootSec = "5m";
        OnUnitActiveSec = "30m";
      };
    };

    # Status API service for Claude and other tools.
    systemd.services.nix-deployment-status = {
      description = "Nix deployment status API";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      serviceConfig = {
        Type = "simple";
        User = "nix-deploy";
        Group = "nix-deploy";
        Restart = "always";
        RestartSec = 5;
      };

      script = ''
        cd ${cfg.statusDirectory}
        ${pkgs.python3}/bin/python -m http.server 9002 --bind 127.0.0.1
      '';
    };

    # Open firewall for webhook.
    networking.firewall.allowedTCPPorts = [ cfg.webhookPort ];

    # Allow nix-deploy user to start deployment service.
    security.polkit.extraConfig = ''
      polkit.addRule(function(action, subject) {
        if (action.id == "org.freedesktop.systemd1.manage-units" &&
            action.lookup("unit") == "nix-deployment-check.service" &&
            subject.user == "nix-deploy") {
          return polkit.Result.YES;
        }
      });
    '';
  };
}