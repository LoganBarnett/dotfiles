################################################################################
# General deployment server configuration.
#
# This configuration can be imported by any host that should act as a
# deployment server.  It provides automatic Git repository watching and
# deployment orchestration.
################################################################################
{ config, host-id, lib, pkgs, ... }: {
  imports = [
    ../nixos-modules/persistent-terminal.nix
    ../nixos-modules/nix-deployment-server.nix
  ];

  # Webhook secret managed by agenix - shared with Gitea host.
  age.secrets.deployment-webhook-secret = {
    generator.script = "long-passphrase";
    owner = "nix-deploy";
    group = "nix-deploy";
    mode = "0400";
  };

  # Enable persistent terminal sessions with dtach.
  services.persistentTerminal = {
    enable = true;
    users = [ "logan" ];
    shellWrapper = true;
  };

  # Configure automatic deployment server.
  services.nixDeploymentServer = {
    enable = true;
    repositoryUrl = "git+ssh://git@gitea.proton:2222/logan/dotfiles.git";

    # List of hosts to manage.  Add more hosts as needed.
    hosts = [
      # Development machines
      "lithium"
      "iridium"
      "sodium"

      # Servers
      "bromine"
      "carbon"
      "neon"
      "scandium"
      "titanium"

      # Media/Entertainment
      "hydrogen"
      "oxygen"

      # Infrastructure
      "helium"
      "nitrogen"
    ];

    webhookPort = 9001;
    webhookSecretFile = config.age.secrets.deployment-webhook-secret.path;
  };

  # SSH configuration for deployment access.
  # Ensure nix-deploy user can access all managed hosts.
  users.users.nix-deploy.openssh.authorizedKeys.keys = [
    # This will inherit keys from logan user via the module
  ];

  # Additional tools for deployment management.
  environment.systemPackages = with pkgs; [
    # JSON processor for parsing deployment status.
    jq

    # HTTP client for testing webhook and status endpoints.
    curl

    # Process monitoring.
    htop

    # Better git experience.
    gitFull
    lazygit
  ];

  # Nginx reverse proxy for deployment status.
  # This makes the status available at a nice URL instead of just a port.
  services.nginx = {
    enable = true;
    virtualHosts."deployment.${host-id}.proton" = {
      locations."/status" = {
        proxyPass = "http://127.0.0.1:9002";
        extraConfig = ''
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
        '';
      };

      locations."/webhook" = {
        proxyPass = "http://127.0.0.1:${toString config.services.nixDeploymentServer.webhookPort}";
        extraConfig = ''
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Gitea-Signature $http_x_gitea_signature;
        '';
      };
    };
  };

  # Create convenience scripts for deployment management.
  environment.etc."deployment-tools/check-all-hosts.sh" = {
    mode = "0755";
    text = ''
      #!/usr/bin/env bash
      # Check deployment status of all hosts.

      echo "Checking deployment status for all hosts..."
      for status_file in ${config.services.nixDeploymentServer.statusDirectory}/*.status; do
        if [[ -f "$status_file" ]]; then
          host=$(basename "$status_file" .status)
          status=$(head -1 "$status_file")
          timestamp=$(tail -1 "$status_file")
          printf "%-20s %-10s %s\n" "$host" "$status" "$timestamp"
        fi
      done
    '';
  };

  environment.etc."deployment-tools/trigger-deployment.sh" = {
    mode = "0755";
    text = ''
      #!/usr/bin/env bash
      # Manually trigger a deployment check.

      echo "Triggering deployment check..."
      systemctl start nix-deployment-check.service
      echo "Check started. Monitor with: journalctl -u nix-deployment-check -f"
    '';
  };

  # Quick access aliases for common operations.
  programs.bash.interactiveShellInit = ''
    alias deploy-status='/etc/deployment-tools/check-all-hosts.sh'
    alias deploy-trigger='/etc/deployment-tools/trigger-deployment.sh'
    alias deploy-logs='journalctl -u nix-deployment-check -f'

    # Convenience function to attach to a Claude session.
    claude-session() {
      pts attach claude-main
    }

    # Show deployment help.
    deploy-help() {
      echo "Deployment Server Commands:"
      echo "  deploy-status   - Check status of all host deployments"
      echo "  deploy-trigger  - Manually trigger deployment check"
      echo "  deploy-logs     - Follow deployment logs"
      echo "  claude-session  - Attach to main Claude development session"
      echo ""
      echo "Persistent Sessions:"
      echo "  pts new NAME    - Create new persistent session"
      echo "  pts attach NAME - Attach to existing session"
      echo "  pts list        - List all sessions"
    }
  '';
}