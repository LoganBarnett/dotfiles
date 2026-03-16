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

  # Script to check if a host's configuration has changed.
  checkHostChanged = pkgs.writeScript "check-host-changed" ''
    #!${pkgs.bash}/bin/bash
    set -euo pipefail

    HOST="$1"
    FLAKE_PATH="$2"

    # Get the current system path for the host.
    CURRENT_PATH=$(ssh "$HOST" readlink /run/current-system 2>/dev/null || echo "")

    # Build the new system path without activating.
    NEW_PATH=$(nix build --no-link --print-out-paths "$FLAKE_PATH#nixosConfigurations.$HOST.config.system.build.toplevel" 2>/dev/null || echo "")

    if [[ -z "$CURRENT_PATH" || -z "$NEW_PATH" ]]; then
      echo "ERROR: Could not determine system paths"
      exit 1
    fi

    if [[ "$CURRENT_PATH" != "$NEW_PATH" ]]; then
      echo "CHANGED"
      echo "Current: $CURRENT_PATH"
      echo "New: $NEW_PATH"
    else
      echo "UNCHANGED"
    fi
  '';

  # Script to deploy to a host.
  deployHost = pkgs.writeScript "deploy-host" ''
    #!${pkgs.bash}/bin/bash
    set -euo pipefail

    HOST="$1"
    FLAKE_PATH="$2"
    STATUS_DIR="$3"

    echo "Deploying to $HOST..."

    # Create status file.
    STATUS_FILE="$STATUS_DIR/$HOST.status"
    echo "DEPLOYING" > "$STATUS_FILE"
    date -Iseconds >> "$STATUS_FILE"

    # Perform the deployment.
    if nixos-rebuild switch --flake "$FLAKE_PATH#$HOST" --target-host "$HOST" --use-remote-sudo 2>&1 | tee -a "$STATUS_FILE.log"; then
      echo "SUCCESS" > "$STATUS_FILE"
      date -Iseconds >> "$STATUS_FILE"
    else
      echo "FAILED" > "$STATUS_FILE"
      date -Iseconds >> "$STATUS_FILE"
    fi
  '';

  # Main deployment orchestrator.
  deploymentOrchestrator = pkgs.writeScript "deployment-orchestrator" ''
    #!${pkgs.bash}/bin/bash
    set -euo pipefail

    REPO_PATH="${cfg.repositoryPath}"
    STATUS_DIR="${cfg.statusDirectory}"

    echo "Starting deployment check at $(date -Iseconds)"

    # Update the repository.
    cd "$REPO_PATH"
    git fetch origin
    git reset --hard origin/$(git rev-parse --abbrev-ref HEAD)

    # Get list of hosts to check.
    HOSTS=(${concatStringsSep " " (map (h: ''"${h}"'') cfg.hosts)})

    # Check each host for changes.
    for host in "''${HOSTS[@]}"; do
      echo "Checking $host..."

      if result=$(${checkHostChanged} "$host" "$REPO_PATH"); then
        if echo "$result" | grep -q "^CHANGED"; then
          echo "Host $host has changes, deploying..."
          ${deployHost} "$host" "$REPO_PATH" "$STATUS_DIR" &
        else
          echo "Host $host unchanged, skipping."
        fi
      else
        echo "Failed to check $host"
      fi
    done

    # Wait for all background deployments to complete.
    wait

    echo "Deployment check completed at $(date -Iseconds)"
  '';
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

      path = with pkgs; [ git openssh nix coreutils ];

      serviceConfig = {
        Type = "oneshot";
        User = "nix-deploy";
        Group = "nix-deploy";
      };

      script = ''
        exec ${deploymentOrchestrator}
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
  };
}