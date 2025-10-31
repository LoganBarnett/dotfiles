################################################################################
# Declares a configuration for mcphost.
################################################################################
{ config, lib, pkgs, ... }: let
  work-alias = lib.concatStrings [
    # I'm the Riddler.
    "h"
    "m"
    "h"
  ];
  work-domain = "${work-alias}co.com";
in {
  home.file.".mcphost.json".text = (builtins.toJSON {
    mcpServers = {
      mcp-atlassian = {
        type = "local";
        # Ugh...
        command = [
          "podman"
          "run" "-i" "--rm"
          "-e" "JIRA_URL"
          "-e" "JIRA_USERNAME"
          "-e" "JIRA_API_TOKEN"
          "ghcr.io/sooperset/mcp-atlassian:latest"
        ];
        environment = {
          JIRA_URL = "https://nwea.atlassian.net";
          JIRA_USERNAME = "\${env://USER}@${work-domain}";
          JIRA_API_TOKEN = "\${env://MCP_ATLASSIAN_TOKEN}";
        };
      };
      bash-commands = {
        type = "builtin";
        name = "bash";
      };
      fs = {
        type = "builtin";
        name = "fs";
      };
      task-manager = {
        type = "builtin";
        name = "todo";
      };
      web-fetcher = {
        type = "builtin";
        name = "http";
      };
    };
  });
}
