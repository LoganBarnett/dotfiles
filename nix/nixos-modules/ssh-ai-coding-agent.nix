################################################################################
# Configures SSH client for AI coding agents.
#
# Allows specification of environment-variable-gated SSH configurations that use
# specific users and SSH keys when connecting to matched hosts.  This enables
# secure, read-only access for AI coding agents like Claude Code without
# requiring manual SSH config management.
################################################################################
{ config, lib, pkgs, ... }:

let
  cfg = config.services.ssh-ai-coding-agent;

  # Generate a Match block for a single environment variable.
  # Note: ssh_config doesn't support "Match env", so we use "Match exec" with a
  # test command to check the environment variable value.
  mkEnvMatch = envName: envValue: hosts: user: identityFile: ''
    Match host ${lib.concatStringsSep "," hosts} exec "test \"''$${envName}\" = \"${envValue}\""
      User ${user}
      IdentityFile ${identityFile}
  '';

  # Generate Match blocks for all configured environment variables.
  mkMatchBlocks = lib.concatMapStringsSep "\n\n" (envCfg:
    mkEnvMatch envCfg.name envCfg.value cfg.hostMatchers cfg.user cfg.identityFile
  ) (lib.mapAttrsToList (name: value: { inherit name value; }) cfg.environmentVariables);

in
{
  options.services.ssh-ai-coding-agent = {
    enable = lib.mkEnableOption "SSH configuration for AI coding agents";

    user = lib.mkOption {
      type = lib.types.str;
      default = "llm-coding-agent";
      description = ''
        The remote user to authenticate as when connecting to matched hosts.
      '';
      example = "llm-coding-agent";
    };

    identityFile = lib.mkOption {
      type = lib.types.str;
      description = ''
        Path to the SSH private key to use for authentication.  Can be a
        relative path like "~/.ssh/id_ed25519" or an absolute path.
      '';
      example = "/home/logan.barnett/.ssh/id_ed25519-llm-coding-agent";
    };

    environmentVariables = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = {};
      description = ''
        Environment variables to match on.  Each variable will create a
        separate Match block in the SSH config.  The SSH client will only use
        this configuration when the specified environment variable is set to
        the given value.
      '';
      example = {
        CLAUDECODE = "1";
        AIDER_CHAT = "1";
      };
    };

    hostMatchers = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = ''
        List of host patterns to match.  These patterns follow SSH config
        matching rules (supports wildcards like *.example.com).
      '';
      example = [ "*.nwea.pvt" "*.nweacolo.pvt" ];
    };

    homeManagerUsers = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = ''
        List of home-manager users to configure with this SSH setup.
      '';
      example = [ "logan.barnett" ];
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users = lib.genAttrs cfg.homeManagerUsers (userName: {
      programs.ssh = {
        enable = true;
        extraConfig = mkMatchBlocks;
      };
    });
  };
}
