################################################################################
# SSH configuration for LLM coding agents accessing work hosts.
#
# Configures SSH access for Claude Code and other LLM coding agents to work
# infrastructure.  LLM_CODING_AGENT_SSH_PUB_KEY is exposed as a session
# variable so IPA automation can locate and upload the key.
################################################################################
{ osConfig, lib, ... }: let
  work-alias = lib.concatStrings [
    "n"
    "w"
    "e"
    "a"
  ];
  pub-key-path = "${../secrets/llm-coding-agent-ssh.pub}";
in {
  programs.ssh.matchBlocks = {
    "*.${work-alias}.pvt *.${work-alias}colo.pvt" = {
      user = "llm-coding-agent";
      identityFile = osConfig.age.secrets.llm-coding-agent-ssh.path;
    };
  };
  home.sessionVariables = {
    LLM_CODING_AGENT_SSH_PUB_KEY = pub-key-path;
  };
}
