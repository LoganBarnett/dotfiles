################################################################################
#
################################################################################
{ lib, pkgs, ... }: let
  inherit (lib) concatStrings;
  new-e-ah-alias = concatStrings [
    "n"
    "w"
    "e"
    "a"
  ];
  ache-em-ache-alias = concatStrings [
    "h"
    "m"
    "h"
  ];
  ache-em-ache-domain = "${ache-em-ache-alias}co.com";
in {
  home.file.".copilot/mcp-config-template.json".text = builtins.toJSON (let
    readOnlyTools = [
      # Jira read operations.
      "jira_get_user_profile"
      "jira_get_issue"
      "jira_search"
      "jira_search_fields"
      "jira_get_project_issues"
      "jira_get_transitions"
      "jira_get_worklog"
      "jira_download_attachments"
      "jira_get_agile_boards"
      "jira_get_board_issues"
      "jira_get_sprints_from_board"
      "jira_get_sprint_issues"
      "jira_get_link_types"
      # Cloud only.
      "jira_batch_get_changelogs"
      "jira_get_all_projects"
      "jira_get_project_versions"
      # Confluence read operations.
      "confluence_search"
      "confluence_get_page"
      "confluence_get_page_children"
      "confluence_get_comments"
      "confluence_get_labels"
      "confluence_search_user"
    ];
  in {
    mcpServers = {
      atlassian = {
        type = "local";
        # These don't work as advertised.  In both interactive and
        # non-interactive sessions, the confirmation is required and so this
        # isn't an auto-approve list.  The only away around it currently is via
        # `--allow-all-tools`.
        # tools = readOnlyTools;
        tools = [ "*" ];
        command = "podman";
        args = [
          "run"
          "--rm"
          "-i"
          "-e" "JIRA_URL"
          "-e" "JIRA_USERNAME"
          "-e" "JIRA_API_TOKEN"
          "-e" "CONFLUENCE_URL"
          "-e" "CONFLUENCE_USERNAME"
          "-e" "CONFLUENCE_API_TOKEN"
          "ghcr.io/sooperset/mcp-atlassian:latest"
        ];
        env = {
          JIRA_URL = "https://${new-e-ah-alias}.atlassian.net";
          JIRA_USERNAME = "<%USER%>@${ache-em-ache-domain}";
          # TODO: You still need to set this token in your session. Use:
          # export MCP_ATLASSIAN_TOKEN="$(pass show hmh-atlassian-token-mcphost)"
          JIRA_API_TOKEN = "<%MCP_ATLASSIAN_TOKEN%>";
        };
      };
    };
  });
  home.packages = [
    # TODO: This would be a handy, generic tool.
    (pkgs.writeShellApplication {
      name = "copilot-config-substitute";
      text = ''
        ${pkgs.perl}/bin/perl -pe '
          s/<%(\w+)%>/exists $ENV{$1} ? $ENV{$1} : "<%$1%>"/ge
        ' "$HOME/.copilot/mcp-config-template.json" > \
            "$HOME/.copilot/mcp-config.json"
      '';
    })
  ];
}
