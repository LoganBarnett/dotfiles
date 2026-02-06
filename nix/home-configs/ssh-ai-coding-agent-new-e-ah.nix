################################################################################
# SSH configuration for AI coding agents accessing work hosts.
#
# Configures environment-variable-gated SSH access for Claude Code and other AI
# coding agents to work infrastructure.
################################################################################
{ config, lib, ... }: let
  work-alias = lib.concatStrings [
    "n"
    "w"
    "e"
    "a"
  ];
in {
  age.secrets.llm-coding-agent-ssh = {
    generator.script = "ssh-ed25519-with-pub";
    rekeyFile = ../secrets/llm-coding-agent-ssh.age;
    # Allow the primary user to read this key for SSH client usage.
    mode = "0400";
    owner = lib.mkDefault config.system.primaryUser;
  };

  services.ssh-ai-coding-agent = {
    enable = true;
    user = "llm-coding-agent";
    identityFile = config.age.secrets.llm-coding-agent-ssh.path;
    environmentVariables = {
      CLAUDECODE = "1";
    };
    hostMatchers = [
      "*.${work-alias}.pvt"
      "*.${work-alias}colo.pvt"
    ];
    homeManagerUsers = [ "logan.barnett" ];
  };
}
